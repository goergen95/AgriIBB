# script to analyse abandonment/fallow frequency
library(raster)


# prepare paths to predicted active/inactive raster files
predfiles = list.files("../results/prediction/",pattern="activitiy",full.names=TRUE)

# read files as raster and declare function to set active pixels to 0, while inactive pixels remain 1
predRas = lapply(predfiles,raster)
maskActive = function(x){
  x[x==2]=0 
  return(x)
}

# apply previously declared function to all rasters individually
ffrequency = lapply(predRas,maskActive)
# create frequency raster layer by simply summing up the years of inactive pixels
ffrequency = sum(stack(ffrequency))
writeRaster(ffrequency,filename="../results/prediction/ffrequency.tif")
# we can also create a raster indicating the years opf active uses
# its simply the 14 years - the fallow frequency raster -  might be useful for future analysis
afrequency = predRas[[1]]
afrequency[] = 14
afrequency = afrequency-ffrequency
writeRaster(afrequency,filename="../results/prediction/afrequency.tif")
