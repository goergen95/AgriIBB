# -*- coding: utf-8 -*-
"""
Created on Sat Jun  8 19:19:43 2019

@author: darius goergen
"""
# This script applies both savitzkiy-golay filtering of MODIS NDVI time series

import rasterio as rio
import numpy as np
from pathlib import Path
from scipy.signal import savgol_filter
from osgeo import gdal


  
years = ["2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"]
DOYs = ["001","017","033","049","065","081","097","113","129","145","161","177","193","209","225","241","257","273","289","305","321","337","353"]



for year in years:
    print("Reading in raster data ....")
    with rio.open("../results/linPol/linPol_" + year + ".tif") as src:
        vi = src.read()
        vi_meta = src.profile
        
    #vi_meta["crs"] = "proj=latlong"
    print("Applying SG-Fltering")
    vi_filter = savgol_filter(vi,window_length=7,polyorder=3,axis=0)
    vi_meta["compress"] = "LZW"
    #vi_meta["dtype"] = "int16"
    #vi_meta["nodata"] = -32000
    vi_meta["count"] = 1
    print("Start to write files for " + year + ".")
    for DOY in list(range(0,23)):
        with rio.open("../results/savG/layered/savG_"+ year + DOYs[DOY] + ".tif", "w", **vi_meta) as out:
            out.write(vi_filter[DOY,:,:] , 1)
    
    print("Start to calculate growing season paramters for " + year + ".")
    vi_GS = vi_filter[6:18,:,:] # ownly include growing season monthts
    vi_MEAN = vi_GS.mean(axis=0, dtype="int32")
    vi_SUM = vi_GS.sum(axis=0, dtype="int32")
    vi_SD = vi_GS.std(axis=0,dtype="float64")
    vi_SD = vi_SD.astype("int32")
    vi_AMP = vi_GS.max(axis=0) - vi_GS.min(axis=0)
    vi_AMP = vi_AMP.astype("int32")
    vi_Q25 = np.percentile(vi_GS,axis=0,q=25)
    vi_Q25 = vi_Q25.astype("int32")
    vi_Q75 = np.percentile(vi_GS,axis=0,q=75)
    vi_Q75 = vi_Q75.astype("int32")
    print("Start to write growing season paramters for " + year + " to disk.")
    vi_meta["dtype"] = "int32"
    vi_meta["nodata"] = -32000
    with rio.open("../results/savG/MEAN_" + year + ".tif","w", **vi_meta) as out:
        out.write(vi_MEAN[:,:], 1)
    with rio.open("../results/savG/SUM_" + year + ".tif","w", **vi_meta) as out:
        out.write(vi_SUM[:,:], 1)
    with rio.open("../results/savG/SD_" + year + ".tif","w", **vi_meta) as out:
        out.write(vi_SD[:,:], 1)
    with rio.open("../results/savG/AMP_" + year + ".tif","w", **vi_meta) as out:
        out.write(vi_AMP[:,:], 1)
    with rio.open("../results/savG/Q25_" + year + ".tif","w", **vi_meta) as out:
        out.write(vi_Q25[:,:], 1)
    with rio.open("../results/savG/Q75_" + year + ".tif","w", **vi_meta) as out:
        out.write(vi_Q75[:,:], 1)
    print("Now proceeding with next year")


