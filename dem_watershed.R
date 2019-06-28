library(uavRst)
library(RSAGA)
library(rgdal)
library(raster)
library(link2GI)


# prepare saga comand line interface
if (!file.exists("../run/saga.rds")){
  saga = linkSAGA(ver_select = T)
  save(saga, file="../run/saga.rds")
}else{
  saga = readRDS("../run/saga.rds")
}

dem = raster("../results/dem/dem.tif")
# export dem to saga format
writeRaster(dem, filename="../run/dem_saga.sdat" , NAflag = 0, overwrite = TRUE)
# apply pre processing function
system(paste0(saga$sagaCmd," ta_preprocessor 4 ",
              " -ELEV ../run/dem_saga.sgrd",
              " -FILLED ../run/dem_filled.sgrd",
              " -FDIR ../run/dem_fdir.sgrd",
              " -WSHED ../run/dem_wshed.sgrd"))

fdir = raster(list.files("../run/",pattern="fdir.sdat",full.names = T),options="COMPRESS=LZW")
crs(fdir) = crs(dem)
writeRaster(fdir, filename="../results/dem/flowdirection.tif")
fdem = raster(list.files("../run/",pattern="filled.sdat",full.names = T),options="COMPRESS=LZW")
crs(fdem)=crs(dem)
writeRaster(fdem,"../results/dem/filled_dem.tif",options="COMPRESS=LZW")
wshed = raster(list.files("../run/",pattern="wshed.sdat",full.names = T),options="COMPRESS=LZW")
crs(wshed)=crs(dem)
writeRaster(wshed,"../results/dem/watersheds.tif",options="COMPRESS=LZW")


# calculate flow accumulation raster
system(paste0(saga$sagaCmd," ta_hydrology 2",
              " -ELEVATION ../run/dem_filled.sgrd",
              " -FLOW ../run/flowacc.sgrd",
              " -VAL_MEAN ../run/meancath.sgrd",
              " -METHOD 0"))

facc = raster("../run/flowacc.sdat")
crs(facc)=crs(dem)
writeRaster(facc,filename="../results/dem/faccumulation1.tif", options="COMPRESS=LZW")


system(paste0(saga$sagaCmd," ta_channels 0",
              " -ELEVATION ../run/dem_filled.sgrd",
              " -SINKROUTE ../run/dem_fdir.sgrd",
              " -INIT_GRID ../run/flowacc.sgrd",
              " -CHNLNTWRK ../run/channetwork.sgrd",
              " -CHNLROUTE ../run/chanroute.sgrd",
              " -SHAPES ../run/chanels.shp",
              #" -INIT_VALUE ",,
              " -MINLEN 5000"))



