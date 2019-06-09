# This script calculates some parameters for the growing season on a pixel basis
# these parameters are used as additional predictiors to the "raw" NDVI values
# Parameters which are calculated:
# 1.) Mean NDVI value for the growing season
# 2.) Standard Deviation for the growing season
# 3.) 25% and 75% quantile of the NDVI curve
# 4.) Amplitude of NDVI signal during the growing season (NDVImax - NDVImin)
# 5.) Integral of the NDVI curve with NDVImin as a baseline
library(raster)
VIs = list.files("../results/savG/", pattern ="savG", full.names = TRUE)
years = 2003:2016
## Calculate Seasonal Metrics
# Growing Season from 1st of April to 30th of October
# DOY 81 to DOY 305

# mean NDVI value for the growing season

# 25% and 75% quantile

# Amplitude growing season (max - min)

# standard deviation over growing season

# Area-Under-Curve for growing season