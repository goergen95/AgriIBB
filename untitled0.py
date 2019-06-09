# -*- coding: utf-8 -*-
"""
Created on Sat Jun  8 19:19:43 2019

@author: dagoe
"""
# savitzkiy-golay filtering of MODIS NDVI time series

import rasterio as rio
import numpy as np
from pathlib import Path
from scipy.signal import savgol_filter
from osgeo import gdal


  
years = ["2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"]


for year in years:
    with rio.open("results/linPol/linPol_" + year + ".tif") as src:
        vi = src.read()
        vi_meta = src.profile
        
    #vi_meta["crs"] = "proj=latlong"
    vi_filter = savgol_filter(vi,window_length=7,polyorder=3,axis=0)
    vi_filter = vi_filter.astype("int16")
    vi_meta["compress"] = "LZW"
    vi_meta["dtype"] = "int16"
    vi_meta["nodata"] = -32000
    vi_meta["count"] = 23
    print("Start to write file for " + year + ".")
    with rio.open("results/savG/savG_" + year + ".tif", "w", **vi_meta) as out:
        for i in list(range(1,23)):
            out.write(vi_filter[i-1,:,:] , i)
    vi_meta["count"] = 1
    with rio.open("results/savG/savG_B23_" + year + ".tif", "w", **vi_meta) as out:
        out.write(vi_filter[22,:,:] , 1)
    

#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[0,:,:],1)
# with rio.open("B:/ibb/results/savG/SavG_B2" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[1,:,:],2)
#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[2,:,:],3)
#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[3,:,:],4)
#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[4,:,:],5)
#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[5,:,:],6)
#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[6,:,:],7)
#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[7,:,:],8)
#with rio.open("B:/ibb/results/savG/SavG_B1" + year + ".tif", "w", **vi_meta) as out:
#    out.write(vi_filter[8,:,:],1)

