# script to achieve an automated stratified random sampling
library(rgdal)
library(gdalUtils)
library(rgeos)
library(mapview)
library(sp)
library(maptools)


fields = readOGR("F:/R/results/shapes/agri_area_single.shp")
fields$id = 1:length(fields)
provinces = readOGR("F:/R/results/shapes/adm_1_2_balchasch_alakol.shp")
provinces = provinces[is.na(provinces$HASC_1),]

provinces@data = provinces@data[,c(2,5,14)]
provinces@data$NAME_1 = as.character(provinces@data$NAME_1)
provinces@data$NAME_2 = as.character(provinces@data$NAME_2)
centroids = gCentroid(fields, byid = TRUE)
contained = gContains(provinces,centroids, byid = TRUE)
index = apply(contained,1,function(x){
  return(which(x == TRUE))
})
index = as.vector(unlist(index))


provNames = provinces@data$NAME_2
fields$province = provNames[index]
fields$area = gArea(fields,byid = TRUE)

df = data.frame(prov = provNames,area=0, perc = 0,pixels=0)
for(province in provNames){
  df$area[df$prov==province] = sum(fields$area[fields$province==province])
}
df$perc = df$area / sum(df$area)
df$pixels = round(2500 * df$perc)

set.seed(123920)
randomPoints = lapply(provNames, function(x){
  if(length(fields[which(fields$province==x),])==0) return(NULL)
  tmp = spsample(fields[which(fields$province==x),],n=df$pixels[df$prov==x],"stratified")
  return(tmp)
})
compact = function(x) Filter(Negate(is.null),x)
randomPoints = compact(randomPoints)
tmp = randomPoints[[1]]
for(i in 2:length(randomPoints)){
  tmp = spRbind(tmp,randomPoints[[i]])
}

buffer = gBuffer(randomPoints,byid = TRUE, width = 0.001475257*4.5)#width is 3 times pixel resolution)
touch = gIntersects(buffer, byid = TRUE)

index = apply(touch,1,function(x){
  if (sum(x)==1)return(0)
  if (sum(x)>1)return(which(x==TRUE))
})
index = as.vector(unlist(index))
index = which(index!=0)
randomPoints = randomPoints[-index,]
randomPoints = SpatialPointsDataFrame(randomPoints, data.frame(id=1:length(randomPoints)))



writeOGR(buffer, dsn = "B:/ibb/results/shapes/buffer.shp",layer = "buffer",overwrite_layer = TRUE, driver ="ESRI Shapefile")
writeOGR(randomPoints, dsn = "B:/ibb/results/shapes/random_points.shp",layer = "random_points",overwrite_layer = TRUE, driver ="ESRI Shapefile")
writeOGR(provinces,dsn="B:/ibb/results/shapes/regions.shp", layer ="regions",driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(fields,dsn="B:/ibb/results/shapes/fields.shp", layer ="fields",driver="ESRI Shapefile", overwrite_layer = T )
write.csv(df,"B:/ibb/results/randomSample.csv")