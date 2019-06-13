# script to prepare the training data
library(rgdal)
library(gdalUtils)
library(rgeos)
library(mapview)
library(sp)
library(maptools)
library(raster)
library(stringr)

# specifiy number of cores for parallell processing
cores = 7

# read in needed files
randomPoints = readOGR("../data/shapes/random_points.shp")
regions = readOGR("../data/shapes/regions.shp")
fields = readOGR("../data/shapes/fields.shp")
NDVIls = list.files("../results/savG/layered/", pattern="2016", full.names=TRUE)
GROWls = list.files("../results/savG",pattern=".tif",full.names = TRUE)

# adding region names to sample points
interRegion = gIntersects(randomPoints,regions,byid=TRUE)
for (i in 1:length(randomPoints)){
  randomPoints$region[i] = regions$NAME_2[interRegion[,i]]
  print(i)
}

# prepare predictor stack based on DOY-NDVI values and growing season parameters
NDVI = stack(NDVIls)
GROW = stack(GROWls[grep("2016",list.files("../results/savG/",pattern=".tif"))])
predStack = stack(NDVI,GROW)
rm(NDVI,GROW)
DOYs = paste("DOY",str_sub(NDVIls,-7,-5),sep="")
paras = c("AMP","MEAN","Q25","Q75","SD","SUM")
predNames = c(DOYs,paras)
saveRDS(predNames,file="../results/prediction/predNames.rds")

# transform fields and sample points to raster to speed up data extraction
# sample points to raster
r = raster("../results/savG/layered/savG_2003001.tif")
r[] = NA
refRas = rasterize(randomPoints,r,randomPoints$id)
writeRaster(refRas, "../results/prediction/refRas.tif", overwrite = TRUE)

# fields to raster (implemented with parallel processing)
fields$rasVal = 1
beginCluster(cores)
agrMask = clusterR(r,fun=raster::rasterize,args=list(x=fields,field=fields$rasVal))
#agrMask = rasterize(fields,r,fields$rasVal)
endCluster()
writeRaster(agrMask,filename="../results/prediction/agrMask.tif")


# extract predictor variables for each pixel ID in refRas
names(predStack) = predNames
data = extract(predStack,randomPoints, cellnumbers=TRUE,df=TRUE)
randomPoints@data[,4:32] = data2[,3:31]

for (i in 1:length(predNames)){
  randomPoints@data[,predNames[i]] = data[,i+2]
}

writeOGR(randomPoints,dsn="../results/shapes/random_points.shp",overwrite_layer=TRUE,driver="ESRI Shapefile",layer="random_points")
