# This script retrives temperature and percipitation values for the provinces in the IBB.
# Climatic data is based on gridded estimates of CRU TS 4.03
library(raster)
library(rgdal)
library(rgeos)

# reading needed files
regions = readOGR("../results/shapes/regions.shp")
prec = stack(list.files("../results/climate/", pattern="prec", full.names=TRUE))
temp = stack(list.files("../results/climate/", pattern="tmp", full.names=TRUE))
watershed = readOGR("../results/shapes/watershed_IBB.shp")
# for the calculation of the areas, the shapefile is transformed to UTM Zone 44N
# the majority of the IBB with agricultural areas lies within that zone
# later the shapefile is re-projected to WGS84
orgCRS = crs(regions)
utmCRS = "+proj=utm +zone=44 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
regions = spTransform(regions,utmCRS)
regions$area = gArea(regions,byid = TRUE) / 1e+6 # units in km2
regions= spTransform(regions,orgCRS)


# loop through the regions to retireve mean temperature values and percipitation values
years=2003:2016
regions@data[,paste("temp",years,sep="")] = NA
regions@data[,paste("prec",years,sep="")] = NA

for (i in 1:length(regions)){
  t = colMeans(extract(temp,regions[i,])[[1]])
  p = colMeans(extract(prec,regions[i,])[[1]]) 
  regions@data[i,paste("temp",years,sep="")] = t
  regions@data[i,paste("prec",years,sep="")] = p
}

writeOGR(regions,dsn="../results/shapes/regions.shp",overwrite_layer=TRUE,driver="ESRI Shapefile",layer="regions")
