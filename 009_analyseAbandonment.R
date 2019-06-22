# script to analyse abandonment/fallow frequency
library(raster)
cores = 7
# classification scheme for agricultural activity analysis

# stable activley managed cropland: 11 to 14 years of activity

# stably inactive cropland: 0 to 3 years of activity

# recent abandonment within the study period: 4 to 7 years activity in 2003-2009; 0 to 3 years activity in 2010-2016

# recent recultivation within the study period: 0 to 3 years activity in 2003-2009; 4 to 7 years activity in 2010-2016


# read in needed files
predfiles = list.files("../results/prediction/",pattern="activitiy",full.names=TRUE)
agrMask = raster("../results/prediction/agrMask.tif")
# read files as raster and declare function to set active pixels to 1 and active pixels to 0
predRas = lapply(predfiles,raster)
maskActive = function(x){
  x[x==1]=0 # inactive 
  x[x==2]=1 # active
  return(x)
}

# apply previously declared function to all rasters individually
predRas = lapply(predRas,maskActive)

# create activity frequency raster layer by simply summing up the years of active pixels
afrequency = sum(stack(predRas))
writeRaster(afrequency,filename="../results/prediction/afrequency.tif", overwrite=TRUE)
ffrequency = afrequency
ffrequency[] = 14
ffrequency = ffrequency - afrequency
writeRaster(ffrequency,filename="../results/prediction/ffrequency.tif", overwrite =TRUE)
# extraction of stabley managed cropland
cropMap = afrequency
cropMap[cropMap<11] = NA
cropMap[!is.na(cropMap)] = 4 # number code for stabley managed cropland

# extraction of stabley inactive cropland
abandMap = afrequency
abandMap[abandMap>3] = NA
abandMap[!is.na(abandMap)] = 1 # number code for stabley inactive cropland


# preparation of period stacks
period0309 = sum(stack(predRas[1:7]))
period1116 = sum(stack(predRas[8:14]))
# setting other classes to NA
period0309[!is.na(cropMap)] = NA
period0309[!is.na(abandMap)] = NA
period1116[!is.na(cropMap)] = NA
period1116[!is.na(abandMap)] = NA

# extraction of recently abandoned cropland
recAband = period0309
recAband[period0309<4] = NA
recAband[period1116>3] = NA
recAband[!is.na(recAband)]=2 # number code for recent abandonment class

# extraction of recently recultivated cropland
recRecult = period0309
recRecult[period0309>=4] = NA
recRecult[period1116<=3] = NA
recRecult[!is.na(recRecult)]=3 # number code for recent recultivated class


Map = sum(stack(cropMap,abandMap,recAband,recRecult),na.rm = TRUE)
Map[Map==0] = NA
writeRaster(Map,"../results/prediction/abandonmentMap.tif",overwrite = TRUE)

# hotspot map of abandoned cropland
#function to make a circular weights matrix of given radius and resolution
#NB radius must me an even multiple of res!
make_circ_filter<-function(radius, res){
  circ_filter<-matrix(NA, nrow=1+(2*radius/res), ncol=1+(2*radius/res))
  dimnames(circ_filter)[[1]]<-seq(-radius, radius, by=res)
  dimnames(circ_filter)[[2]]<-seq(-radius, radius, by=res)
  sweeper<-function(mat){
    for(row in 1:nrow(mat)){
      for(col in 1:ncol(mat)){
        dist<-sqrt((as.numeric(dimnames(mat)[[1]])[row])^2 +
                     (as.numeric(dimnames(mat)[[1]])[col])^2)
        if(dist<=radius) {mat[row, col]<-1}
      }
    }
    return(mat)
  }
  out<-sweeper(circ_filter)
  return(out)
}
cw = make_circ_filter(res(Map)[1]*10,res(Map)[1]) 
# declare function which calculates percentage of abandoned pixels (class 1 and 2) in a matrix
return_percentag = function(x){
  x = na.omit(x)
  l = sum(x==1 | x==2)
  a = sum(!is.na(x))
  return(l/a)
}


# apply the focal opperation to the abandonment map and exclude non-agricultural pixels
beginCluster(cores)
hotspotAband = focal(Map,w=cw,fun=return_percentag)
endCluster()
hotspotAband[is.na(agrMask)] = NA
# save abandonment hotspot map to disk
writeRaster(hotspotAband, "../results/prediction/hotspotAband.tif", overwrite=TRUE)


return_percentag = function(x){
  x = na.omit(x)
  l = sum(x==3)
  a = sum(!is.na(x))
  return(l/a)
}


# apply the focal opperation to the abandonment map and exclude non-agricultural pixels
beginCluster(cores)
hotspotRecult = focal(Map,w=cw,fun=return_percentag)
endCluster()
hotspotRecult[is.na(agrMask)] = NA
writeRaster(hotspotRecult,filename="../results/prediction/hotspotRecult.tif",overwrite=TRUE)
