# This script calculates some parameters for the growing season on a pixel basis
# these parameters are used as additional predictiors to the "raw" NDVI values
# Parameters which are calculated:
# 1.) Mean NDVI value for the growing season
# 2.) Standard Deviation for the growing season
# 3.) 25% and 75% quantile of the NDVI curve
# 4.) Amplitude of NDVI signal during the growing season (NDVImax - NDVImin)
# 5.) Sum of NDVI values over the year (Area under the Curve)
library(raster)
library(stringr)
VIs = list.files("../results/savG/layered/", pattern ="savG", full.names = TRUE)
years = 2003:2016
DOY = unique(str_sub(VIs,-12,-10))
cores = 7


## Calculate Seasonal Metrics
# Growing Season from 06./07 of April to 29./30. September
# DOY 97 to DOY 273
for (year in years){
  vi = lapply(VIs[grep(year,VIs)][7:18],raster) # index 7 and 18 for DOY 97 and 273
  vi = do.call(stack,vi)
  # calculte mean NDVI value
  MEAN = mean(vi)
  # calculate amplitude
  MAX = max(vi)
  MIN = min(vi)
  AMP = MAX - MIN
  # calculation of SD 
  beginCluster(cores)
  sq = (vi - MEAN)
  sq = sq * sq
  sq = sum(sq)
  SD = sqrt(sq/nlayers(vi))
  rm(sq)
  # calculate 25Q and 75Q
  
  Qs = calc(vi,fun=function(x) {quantile(x,probs=c(.25,.75),type=7)})# with: m = 1-p. p[k] = (k - 1) / (n - 1).
  Q25 = Qs[[1]]
  Q75 = Qs[[2]]
  endCluster()
  rm(Qs)
  # calculate integral of NDVI curve over the year
  AREA = sum(vi, na.rm = TRUE)
    
  # write rasters to disk
  writeRaster(MEAN, filename=paste0("../results/savG/layered/MEAN_",year,".tif"), format="GTiff",options="COMPRESS=LZW")
  writeRaster(AMP, filename=paste0("../results/savG/layered/AMP_",year,".tif"), format="GTiff",options="COMPRESS=LZW")
  writeRaster(SD, filename=paste0("../results/savG/layered/SD_",year,".tif"), format="GTiff",options="COMPRESS=LZW")
  writeRaster(Q25, filename=paste0("../results/savG/layered/Q25_",year,".tif"), format="GTiff",options="COMPRESS=LZW")
  writeRaster(Q75, filename=paste0("../results/savG/layered/Q75_",year,".tif"), format="GTiff",options="COMPRESS=LZW")
  writeRaster(AREA, filename=paste0("../results/savG/layered/AREA_",year,".tif"), format="GTiff",options="COMPRESS=LZW")
  print("Done with year ",year,".")
  }
