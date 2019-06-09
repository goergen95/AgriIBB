loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; 
  library(mypkg, character.only=TRUE)  }
#require(MODIS) 
loadandinstall("rgdal")
loadandinstall("raster")
loadandinstall("zoo")
loadandinstall("gdalUtils")
loadandinstall("pracma")
loadandinstall("stringr")
loadandinstall("prospectr")
loadandinstall("bfast")
loadandinstall("signal")
loadandinstall("MODIS")
loadandinstall("randomForest")

## function to derive ndvi and clean
f_cleanvi <- function(vi, rel) {
  i <- (rel <= 2)
  res <- matrix(NA, length(i), 1)
  if(sum(i, na.rm=TRUE) > 0) {
    i <- which(i)
    res[i] <- vi[i]
  }
  res
}

## function to do linear interpolation
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


## function to smooth TS with SG method
savGolay <- function(arg){
  
  # first check for NAs in the vector
  if(sum( is.na(arg))==1){
    
    NonNAindex <- which(is.na(as.numeric(arg)))
    arg[NonNAindex] <- median(as.numeric(arg),na.rm=T)
    
  }
  
  if(is.na(arg[1])){
    
    NonNAindex <- which(!is.na(as.numeric(arg)))
    firstNonNA <- min(NonNAindex)
    arg[1] <- arg[firstNonNA]
    
  }
  
  if(is.na(arg[length(arg)])){
    
    NonNAindex <- which(!is.na(as.numeric(arg)))
    firstNonNA <- max(NonNAindex)
    arg[length(arg)] <- arg[firstNonNA]
    
  }
  
  # then do the SG filtering
  if(sum( is.na(arg))!=length(arg)){
    if(sum( is.na(arg))>=1){
      suppressWarnings(arg <- as.numeric(as.integer(na.roughfix(as.integer(as.numeric(arg))))))
    }
    suppressWarnings(arg <- as.numeric(as.integer(as.numeric(arg))))
    
    if (length(arg)!=0){
      #ltt <-  sgolayfilt(arg)
      ltt <-  sgolayfilt(as.numeric(arg), p=3, n=7)
      ida <- arg-ltt
      idb <- which(ida <= 0)
      arg[idb] <- ltt[idb]
      #sgolayfilt(arg, p=3, n=7)
      suppressWarnings(as.integer(sgolayfilt(as.numeric(arg), p=3, n=7)))
    }else{
      suppressWarnings(as.integer(as.numeric(arg)))
    }
  }else{
    suppressWarnings(as.integer(as.numeric(arg)))
  }  
  
}


# find fucking errors
dummy <- list()
 for (i in 1:length(rasters[[1]])){
   arg <- rasters[i]
   dummy[[i]] <- savGolay(arg)
   print(i)
 }



viname <- "ndvi"
product <- "MOD13Q1"
sets <- 1 # which data set to extract from hdf, 1=ndvi for 13q1 prodict

tileH <- 24
tileV <- 04
begin <- "2009.01.01"
end <- "2009.12.31"

# name of the raw time series output
filename <- "h24v04_ModMyd_brick_raw.tif"






#load functions from file and set basedir depending on operating system
MODISoptions(localArcPath = "data/MODIS/",
             outDirPath = "results/",
             MODISserverOrder = c("LAADS", "LPDAAC"),stubbornness = 5) # LAADS läuft meiner Meinung nach stabiler

collection <- getCollection("MOD13Q1", forceCheck = TRUE)

fls <- runGdal("MOD13Q1", collection = collection,
               begin = "2016-12-02", end = "2016-12-18",
               extent = lu, # Tile ID
               job = "MOD13Q1.006", # Ausgabeordner in 'outDirPath', siehe oben
               SDSstring = "100000000001") # zu extrahierende SDS Layer








t<-.Platform
if(t$OS.type == "unix") setwd("~/Desktop/abandoned/raster/MODIS/h24v04") # set working directory

MODISoptions(localArcPath="~/Desktop/abandoned/raster/MODIS/", 
             outDirPath="~/Desktop/abandoned/raster/MODIS/")
localArcPath <- getOption("localArcPath")

ofilename <- paste(product,"_",viname,"_brick.grd", sep = "")

modis.hdf <- getHdf(product=product, begin=begin, end=end, stubbornness=5,
                    tileH=tileH, tileV=tileV)
modis.hdf1 <- list.files(pattern=c("MOD13Q1",".hdf"))[1:322]


# This loop goes through each element in modis.hdf, which stores the paths to the downloaded HDF files.
rasters <- stack()
for (i in 1:length(modis.hdf1)) {
  
  ## For each element in modis.hdf, which is a oath to one of the HDF files, extract the corresponding filename
  fn1 <- paste(c(unlist(strsplit(unlist(strsplit(modis.hdf1[i], c("[/]"))),"[.]"))[c(1:5)],c("_cvi.hdf")),
               collapse='',sep="")
  
  
  ## extract the data sets that are stored in each HDF file (e.g. NDVI and data quality layer)     
  sds1 <- get_subdatasets(modis.hdf1[i])
  
  # translate the 1st subdataset of the HDF file to GTiff (the 1st data set in the HDF file corresponds to NDVI)
  tmp1 <- rasterTmpFile()
  extension(tmp1) <- 'tif'
  gdal_translate(sds1[sets], dst_dataset = tmp1)
  
  # reliability data
  tmp4 <- rasterTmpFile()
  extension(tmp4) <- 'tif'
  gdal_translate(sds1[12], dst_dataset = tmp4)
  
  
  clvi1 <- overlay(raster("results/MOD13Q1.006/MOD13Q1.A2016353.250m_16_days_NDVI.tif"), raster("results/MOD13Q1.006/MOD13Q1.A2016353.250m_16_days_pixel_reliability.tif"), fun=f_cleanvi,format="GTiff",
                    filename = "results/clean_ndvi/20170100MOD13Q1A2016353", dataType="INT2S", overwrite=TRUE) 
  
  #clvi1 <- clusterR(x=stack(raster(tmp1), raster(tmp4)), fun=overlay, 
  #                  args=list(fun=f_cleanvi,format="GTiff", filename = fn1, dataType="INT2U", overwrite=TRUE))
  
  rasters <- addLayer(rasters, clvi1)
  
  
  # perform garbage colelction to free additional memory
  rm(clvi1, tmp1, tmp4)
  gc()
  print(paste(i,"of",length(modis.hdf1),"done"))
  
  
}# end of "for"




rasters <- do.call("stack",list(list.files(pattern=c("_cvi"))))
writeRaster(rasters, filename=filename, options=c("COMPRESS=LZW",datatype='INT2S' ))


# apply linear interpolation to time series
beginCluster(n=7)# n=6 defines the number of cores to use
test<-clusterR(rasters, calc, args=list(fun=inPol,forceapply=T))
writeRaster(test, filename="ModMyd_brick_lin_INT2S.tif",  format="GTiff", overwrite=TRUE, datatype="INT2S")
writeRaster(test, filename="ModMyd_brick_lin_INT2U.tif",  format="GTiff", overwrite=TRUE, datatype="INT2U")


# apply savGol interpolation to time series
rasters <- brick("ModMyd_brick_lin_INT2S.tif")
test2<-clusterR(rasters, calc, args=list(fun=savGolay,forceapply=T))
writeRaster(test2, filename="ModMyd_brick_sav_INT2S.tif",  format="GTiff",overwrite=TRUE, datatype="INT2S")
writeRaster(test2, filename="ModMyd_brick_sav_INT2U.tif",  format="GTiff",overwrite=TRUE, datatype="INT2U")


dataType(brick("ModMyd_brick_sav_INT2S.tif"))

endCluster()# end the multicore-cluster
gc()

