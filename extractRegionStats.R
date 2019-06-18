# script to extract yearly agricultural information on region level
# 1 MODIS pixel is apprx. 250 x 250 m = 62.500m, so 6.25 acres per pixel
library(raster)
library(rgdal)
library(rgeos)
cores=7
# read in all files needed for the analysis

regions = readOGR("../results/shapes/regions.shp")
regionNames = unique(regions@data$NAME_2)
fields = readOGR("../results/shapes/fields.shp")
predfiles = list.files("../results/prediction/", pattern = "activitiy", full.names = T)
abandMap = raster("../results/prediction/abandonmentMap.tif")
years = 2003:2016

# create an index of the names of regions which do not have any agricultural areas
# this saves a lot of unecessary comuputation time when pixel values are extracted
content = gContains(regions,fields,byid=TRUE)
indexEmpty = as.vector(which(colSums(content)==0))
namesEmpty = regionNames[indexEmpty]

if (file.exists("../results/prediction/regionRas.tif")){
  print("Using existing region raster. Make sure to delete regionRas.tif for new calculation.")
  regionRas = raster("../results/prediction/regionRas.tif")
}else{
r = raster(predfiles[1])
r[] = NA
regions$ID = 1:length(regions)
beginCluster(cores)
regionRas = rasterize(regions,r, regions$ID)
writeRaster(regionRas,filename="../results/prediction/regionRas.tif", overwrite=T)
writeOGR(regions,dsn="../results/shapes/regions.shp",driver="ESRI Shapefile",layer="regions",overwrite_layer=TRUE)
}
# initiate empty dataframes for acres of active and inactive areas for all regions and all years
dfActive = data.frame(regions=regionNames,
                      active_2003=rep(0,35),
                      active_2004=rep(0,35),
                      active_2005=rep(0,35),
                      active_2006=rep(0,35),
                      active_2007=rep(0,35),
                      active_2008=rep(0,35),
                      active_2009=rep(0,35),
                      active_2010=rep(0,35),
                      active_2011=rep(0,35),
                      active_2012=rep(0,35),
                      active_2013=rep(0,35),
                      active_2014=rep(0,35),
                      active_2015=rep(0,35),
                      active_2016=rep(0,35))
dfInactive = data.frame(regions=regionNames,
                        inactive_2003=rep(0,35),
                        inactive_2004=rep(0,35),
                        inactive_2005=rep(0,35),
                        inactive_2006=rep(0,35),
                        inactive_2007=rep(0,35),
                        inactive_2008=rep(0,35),
                        inactive_2009=rep(0,35),
                        inactive_2010=rep(0,35),
                        inactive_2011=rep(0,35),
                        inactive_2012=rep(0,35),
                        inactive_2013=rep(0,35),
                        inactive_2014=rep(0,35),
                        inactive_2015=rep(0,35),
                        inactive_2016=rep(0,35))

# applying a nested-for-loop which iterates through the years and regions
# pixel numbers for each regions are extracted, transformed to acrage
# and writes the data into the corresponding cell of the dataframes created before
regVals = as.vector(na.omit(unique(values(regionRas))))

for (year in years){
  r = raster(predfiles[grep(year,predfiles)])
  for (region in regionNames){
    if(region %in% namesEmpty) next
    print(region)
    tmp = as.vector(na.omit(r[regionRas==regions$ID[regions$NAME_2==region]]))
    active = sum(tmp==2) * 6.25
    inactive = sum(tmp==1) * 6.25
    dfActive[which(dfActive$regions==region),paste0("active_",year)] = active
    dfInactive[which(dfInactive$regions==region),paste0("inactive_",year)] = inactive
  }
  print(paste0("Done with year ",year))
}

# based on the names of the regions, the results are merged with the region shapefile
regions@data = base::merge(regions@data,dfActive,by.x="NAME_2",by.y="regions")
regions@data = base::merge(regions@data,dfInactive,by.x="NAME_2",by.y="regions")


# extract abandonment data
abadData = data.frame(region=regions$NAME_2,abandoned=rep(0,35),recAbd=rep(0,35),recRecult=rep(0,35),cropland=rep(0,35))

  for (region in regionNames){
    if(region %in% namesEmpty) next
    print(region)
    tmp = as.vector(na.omit(abandMap[regionRas==regions$ID[regions$NAME_2==region]]))
    aband = sum(tmp==1) * 6.25
    recAband = sum(tmp==2) * 6.25
    recRecu = sum(tmp==3) * 6.25
    cropLand = sum(tmp==4) * 6.25
    abadData$abandoned[which(abadData$region==region)] = aband
    abadData$recAbd[which(abadData$region==region)] = recAband
    abadData$recRecult[which(abadData$region==region)] = recRecu
    abadData$cropland[which(abadData$region==region)] = cropLand
  }

regions@data = base::merge(regions@data,abadData,by.x="NAME_2",by.y="region")
# writting file to disk
writeOGR(regions, dsn="../results/shapes/regions.shp", layer="regions", overwrite_layer=T, driver="ESRI Shapefile")
