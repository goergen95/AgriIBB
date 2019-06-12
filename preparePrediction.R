# script to prepare the training data
library(rgdal)
library(gdalUtils)
library(rgeos)
library(mapview)
library(sp)
library(maptools)
library(raster)
library(stringr)



randomPoints = readOGR("../results/shapes/random_points.shp")
regions = readOGR("../results/shapes/regions.shp")
fields = readOGR("../results/shapes/fields.shp")

interRegion = gIntersects(randomPoints,regions,byid=TRUE)


# add region names to sample points
for (i in 1:length(randomPoints)){
  randomPoints$region[i] = regions$NAME_2[interRegion[,i]]
  print(i)
}

# prepare predictor stack
NDVIls = list.files("../results/savG/layered/", pattern="2016", full.names=TRUE)
GROWls = list.files("../results/savG",pattern=".tif",full.names = TRUE)
NDVI = stack(NDVIls)
GROW = stack(GROWls[grep("2016",list.files("../results/savG/",pattern=".tif"))])
predStack = stack(NDVI,GROW)
rm(NDVI,GROW)
DOYs = paste("DOY",str_sub(NDVIls,-7,-5),sep="")
paras = c("AMP","MEAN","Q25","Q75","SD","SUM")
predNames = c(DOYs,paras)
saveRDS(predNames,file="../results/prediction/predNames.rds")

# transform fields and sample points to raster to speed up data extraction
r = raster("../results/savG/layered/savG_2003001.tif")
r[] = NA
refRas = rasterize(randomPoints,r,randomPoints$id)
writeRaster(refRas, "../results/prediction/refRas.tif")

fields$rasVal = 1
agrMask = rasterize(fields,r,fields$rasVal)
writeRaster(agrMask,filename="../results/prediction/agrMask.tif")

names(predStack) = predNames
data = extract(predStack,randomPoints, cellnumbers=TRUE,df=TRUE)
randomPoints@data[,4:32] = data2[,3:31]


for (i in 1:length(predNames)){
  randomPoints@data[,predNames[i]] = data[,i+2]
}

writeOGR(randomPoints,dsn="../results/shapes/random_points.shp",overwrite_layer=TRUE,driver="ESRI Shapefile",layer="random_points")
