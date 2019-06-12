# -*- coding: utf-8 -*-
"""
Created on Mon Jun 10 11:50:46 2019

@author: dagoe
"""

import rasterio as rio
import numpy as np
from pathlib import Path
from scipy.signal import savgol_filter
from osgeo import gdal
import os


dirname = "B:/ibb/results/clean/"


NDVI = []

for fname in os.listdir(dirname):
    with rio.open(os.path.join(dirname, fname)) as src:
        im = src.read()
    imarray = np.array(im)
    NDVI.append(imarray)
    print(fname)




