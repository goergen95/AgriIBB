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





# initiate MODIS data download
#load functions from file and set basedir depending on operating system
MODISoptions(localArcPath = "data/MODIS/",
             outDirPath = "results/",
             MODISserverOrder = c("LAADS", "LPDAAC"),stubbornness = 5) 

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




tifs = list.files("B:/ibb/results/linPol/", pattern =".tif", full.names=TRUE)
DOY = as.character(seq(1,353,16))
DOY[1:7] = c("001","017","033","049","065","081","097")

years = 2016

beginCluster(7)
for (year in years){
  vi = brick(tifs[grep(year,tifs)])
  DOYi = paste(year,DOY,sep="")
  ti = MODIS::orgTime(extractDate(DOYi, asDate = TRUE, pos1=1,pos2=7)$inputLayerDates)
  clusterR(vi,MODIS::whittaker.raster,args=list(timeInfo = ti, lambda=6000,nIter=3,threshold=2000,pillow=0,outputAs = "one",outDirPath = "B:/ibb/results/whittaker/"))
  MODIS::whittaker.raster(vi,timeInfo = ti, lambda=6000,nIter=3,threshold=2000,pillow=0,outputAs = "one",outDirPath = "B:/ibb/results/whittaker/")
  print(year)
}
endCluster()

savG = function(x){
  if(sum(is.na(x))!=length(x)){
    if(sum(is.na(x))>=1){
      suppressWarnings(x <- as.numeric(as.integer(na.roughfix(as.integer(as.numeric(x))))))
    }
    suppressWarnings(x <- as.numeric(as.integer(as.numeric(x))))
    if (length(x)!=0){
      #ltt <-  sgolayfilt(x)
      ltt <-  sgolayfilt(as.numeric(x), p=3, n=7)
      ida <- x-ltt
      idb <- which(ida <= 0)
      x[idb] <- ltt[idb]
      #sgolayfilt(x, p=3, n=7)
      suppressWarnings(x <- as.integer(sgolayfilt(as.numeric(x), p=3, n=7)))
    }else{
      suppressWarnings(x <- as.integer(as.numeric(x)))
    }
  }else{
    suppressWarnings(x[1:length(x)] <- NA)
  }
  return(x)
}


#s = crop(vi,extent(vi,1000,900,1050,900))
#plot(s)
#mapview::mapview(s[[1]])
#t = calc(s,savG)
#plot(t)
#plot(s[1],type="l")
#plot(t[1],type="l")


beginCluster(7)
for(year in years){
  vi = brick(tifs[grep(year,tifs)])
  clusterR(vi, calc, args=list(fun=savG),filename=paste0("B:/ibb/results/savG/savG_",year,".tif"), format = "GTiff")
  print(year)
}
endCluster()

