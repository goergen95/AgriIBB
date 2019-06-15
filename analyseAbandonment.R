# script to analyse abandonment/fallow frequency
library(raster)


# prepare paths to predicted active/inactive raster files
predfiles = list.files("../results/prediction/",pattern="activitiy",full.names=TRUE)
agrMask = raster("../results/prediction/agrMask.tif")
# read files as raster and declare function to set active pixels to 0, while inactive pixels remain 1
predRas = lapply(predfiles,raster)
maskActive = function(x){
  x[x==2]=0 
  return(x)
}

# apply previously declared function to all rasters individually
predRas = lapply(predRas,maskActive)
# create frequency raster layer by simply summing up the years of inactive pixels
ffrequency = sum(stack(predRas))
writeRaster(ffrequency,filename="../results/prediction/ffrequency.tif", overwrite=TRUE)
# we can also create a raster indicating the years opf active uses
# its simply the 14 years - the fallow frequency raster -  might be useful for future analysis
afrequency = predRas[[1]]
afrequency[] = 14
afrequency = afrequency-ffrequency
writeRaster(afrequency,filename="../results/prediction/afrequency.tif", overwrite=TRUE)


# next we need to differntiate between managed and abandoned pixels
# all pixels which are active 12 or more years are considered as active, since they 
# only indicate maximum three fallow years
active = afrequency
active[active<12] = NA
active[!is.na(active)] = 2


# secondly we define abandoned pixes which show agriculural years in between the years
# 2011 and 2016 in only two years - and vice-versa extract all pixels which 
# are to be considered active in the same period

# exclusion of all active pixels due to higher level criteria
period1116 = sum(stack(predRas[9:14]))
period1116[active==2] = NA

# extraction of all pixels with more than 2 active years in the period 2011-2016
active1116 = period1116
active1116[active1116>2] = NA
active1116[!is.na(active1116)]=2

# extraction of all abandoned pixels in the period 2011-2016
inactive1116 = period1116
inactive1116[active==2] = NA
inactive1116[active1116==2] = NA
inactive1116[!is.na(inactive1116)]= 1

# create combinated map
abandMap = stack(active,active1116,inactive1116)
abandMap = sum(abandMap,na.rm=TRUE)


# reclassify to more conveniant values: 0 = abandoned, 1 = active, NA = NA
abandMap[abandMap==0] = NA
abandMap[abandMap==1] = 0
abandMap[abandMap==2] = 1

# write file to disk
writeRaster(abandMap,filename="../results/prediction/abandMap.tif", overwrite=TRUE)


# calculation of fallow percentage in a 2.5km radius environment for each raster cell
# we use a circular search window 

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

# create circular moving window matrix
cw = make_circ_filter(res(abandMap)[1]*10,res(abandMap)[1]) 


# declare function which calculates percentage of abandoned pixels in a matrix
return_percentag = function(x){
  x = na.omit(x)
  l = sum(x==0)
  a = sum(!is.na(x))
  return(l/a)
}

# apply the focal opperation to the abandonment map and exclude non-agricultural pixels
hotspot = focal(abandMap,w=cw,fun=return_percentag)
hotspot[is.na(agrMask)] = NA

# save abandonment hotspot map to disk
writeRaster(hotspot, "../results/prediction/hotspot.tif", overwrite=TRUE)


