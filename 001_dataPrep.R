# script to download and preprocess the MODIS MODQ13 data for the IBB-Basin
# Pre-Processing Steps included here:
# 1.) Download MODIS data from the Internet
# 2.) Transformation to GTiff clipped to the AOI
# 3.) Exclusion of low Quality Pixels with help of the pixel reliability layer
# 4.) Linear Interpolation of missung values

# the last step of the pre-processing, the savitzkiy-golay filtering was not implemented in R,
# but in Python. The use of the scientific numpy library helped to reduce computation time significantly


# function to install and load needed libraries
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; 
  library(mypkg, character.only=TRUE)  }
#require(MODIS) 
loadandinstall("rgdal")
loadandinstall("raster")
loadandinstall("zoo")
loadandinstall("gdalUtils")
loadandinstall("signal")
loadandinstall("MODIS")
loadandinstall("MODISExtra")
loadandinstall("stringr")


# definition of some general parameteres
cores = 7
cl = parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
aoi = readOGR("../results/shapes/watershed_IBB.shp")
begin = "2003.01.01"
end = "2016.12.31"
product = "MOD13Q1"
years = 2003:2016





# initiate MODIS data download
#load functions from file and set basedir depending on operating system
MODISoptions(localArcPath = "data/MODIS/",
             outDirPath = "results/",
             MODISserverOrder = c("LPDAAC"),stubbornness = 5) 

collection <- getCollection(product, forceCheck = TRUE)

fls <- runGdal(product, collection = collection,
               begin = begin, end = end,
               extent = aoi, 
               job = "MOD13Q1", # name of directory in outDirPath for MODIS extraction
               SDSstring = "100000000001") # select SDS layer to extract, here NDVI and pixel reliability



# exclusion of low quality pixels
f_cleanvi <- function(vi, rel) {
  i <- (rel == 0 | rel == 1) # only keep pixels with values 0 and 1 (refer to MOD13Q1 user guide)
  res <- matrix(NA, length(i), 1)
  if(sum(i, na.rm=TRUE) > 0) {
    i <- which(i)
    res[i] <- vi[i]
  }
  res
}


VIs = list.files("../results/MOD13Q1/", pattern ="NDVI", full.names = TRUE)
RELs = list.files("../results/MOD13Q1/", patettern = "pixel", full.names = TRUE)

for (index in 1:length(VIs)){
  vi = raster(VIs[index])
  rel = raster(RELs[index])
  overlay(vi,rel, fun=f_cleanvi, format="GTiff", options="COMPRESS=LZW",
          filename=paste0("../results/clean/NDVI_",str_sub(vi[index],-15,-10),".tif"))
}

# linear interpolation of missing values for each year

inPol <- function(arg){
  if(sum( is.na(arg))!=length(arg)){
    if(is.na(arg[1])){
      
      NonNAindex <- which(!is.na(arg))
      firstNonNA <- min(NonNAindex)
      arg[1] <- arg[firstNonNA]
      as.integer(na.spline(as.numeric(arg), na.rm = F))
      
    }else{
      as.integer(na.spline(as.numeric(arg), na.rm = F))
    }
  }else{
    rep(0,length(arg))
  }
}

cleanNDVI = list.files("../results/clean/",pattern=".tif",full.names = TRUE)
beginCluster(cores)
for (year in years){
  tmp = stack(cleanNDVI[grep(year,cleanNDVI)])
  tmp = clusterR(tmp, calc, args=list(fun=inPol,forceapply=TRUE))
  writeRaster(tmp,filename=paste0("../results/linPol/linPol_",year,".tif"))
}
endCluster
