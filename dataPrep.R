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
loadandinstall("rgeos")


cores = 7
cl = parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

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

