# script to predict agricultural activity for each year 
library(raster)
# specifiy number of cores for parallell processing
cores = 7

# read in all needed files
rfModel = readRDS("../results/prediction/rfModel.rds")
predNames = readRDS("../results/prediction/predNames.rds")
agrMask = raster("../results/prediction/agrMask.tif")
NDVI = list.files("../results/savG/layered/",pattern=".tif",full.names =T)
GROW = list.files("../results/savG/",pattern=".tif",full.names = T)
years = 2003:2016

# start for loop iterating through each year
# parallel processing is used to speed up the prediction task
# after prediction the resulting layers are masked with a mask layer for the agricultural areas
# files are then saved to disk
beginCluster(cores)
for (year in years){
  r = stack(NDVI[grep(year,NDVI)])
  s =stack(GROW[grep(year,GROW)])
  s = stack(r,s)
  rm(r)
  names(s) = predNames
  print("Starting with raster prediction...")
  predRas = clusterR(s,raster::predict, args=list(model = rfModel))
  print("Starting to mask out non-agricultural pixels...")
  predRas[is.na(agrMask)] = NA
  print("Writing file to disk...")
  writeRaster(predRas, filename=paste0("../results/prediction/activitiy_",year,".tif"))
  print(paste0("Done with prediction for year ",year,"..."))
}
endCluster()


