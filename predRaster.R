# script to predict each year 
library(raster)




rfModel = readRDS("../results/prediction/rfModel.rds")
predNames = readRDS("../results/prediction/predNames.rds")
agrMask = raster("../results/prediction/agrMask.tif")


NDVI = list.files("../results/savG/layered/",pattern=".tif",full.names =T)
GROW = list.files("../results/savG/",pattern=".tif",full.names = T)
years = 2003:2016



cl =  parallel::makeCluster(7)
doParallel::registerDoParallel(cl)

for (year in years){
  r = stack(NDVI[grep(year,NDVI)])
  s =stack(GROW[grep(year,GROW)])
  s = stack(r,s)
  rm(r)
  names(s) = predNames
  print("Starting with raster prediction...")
  
  predRas = predict(s,rfModel)
  print("Starting to mask out non-agricultural pixels...")
  predRas[is.na(agrMask)] = NA
  print("Writing file to disk...")
  writeRaster(predRas, filename="../results/prediction/activitiy_",year,".tif")
  print("Done with prediction for year ",year,"...")
}

stopCluster(cl)


