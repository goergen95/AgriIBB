# This script retrives temperature and percipitation values for the regions in the IBB
# Climatic data is based on gridded estimates of CRU TS 4.03
library(raster)
library(rgdal)
library(rgeos)

# reading needed files
regions = readOGR("../data/shapes/regions.shp")
prec = stack(list.files("../results/climate/", pattern="prec", full.names=TRUE))
temp = stack(list.files("../results/climate/", pattern="tmp", full.names=TRUE))
fields = readOGR("../data/shapes/fields.shp")
watershed = readOGR("../data/shapes/watershed_IBB.shp")
years=2003:2016
# for the calculation of the areas, the shapefile is transformed to UTM Zone 44N
# the majority of the IBB with agricultural areas lies within that zone
# later the shapefile is re-projected to WGS84
orgCRS = crs(regions)
utmCRS = "+proj=utm +zone=44 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
regions = spTransform(regions,utmCRS)
regions$area = gArea(regions,byid = TRUE) / 1e+6 # units in km2
regions= spTransform(regions,orgCRS)

fields = spTransform(fields,utmCRS)
crpArea = gArea(fields,byid=TRUE) / 1e+6
provinces = unique(fields$province)
for (prov in provinces){
  regions$cropArea[regions$NAME_2==prov] = sum(crpArea[fields$province==prov],na.rm=TRUE)
}
regions$cropArea[is.na(regions$cropArea)] = 0
#initiating columns in the dataframe of regions to write precipitation and temperature data into
regions@data[,paste("temp",years,sep="")] = NA
regions@data[,paste("prec",years,sep="")] = NA


# for-loop iterating hrough the regions to retireve mean temperature and percipitation values of the pixels falling into the region
for (i in 1:length(regions)){
  t = colMeans(extract(temp,regions[i,])[[1]])
  p = colMeans(extract(prec,regions[i,])[[1]]) 
  regions@data[i,paste("temp",years,sep="")] = t
  regions@data[i,paste("prec",years,sep="")] = p
}

# writing file to disk
writeOGR(regions,dsn="../results/shapes/regions.shp",overwrite_layer=TRUE,driver="ESRI Shapefile",layer="regions")
