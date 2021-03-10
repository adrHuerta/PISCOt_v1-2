rm(list = ls())

reticulate::use_virtualenv("/home/adrian/Documents/Repos/prob_Budyko/venv", 
                           required = TRUE)
reticulate::repl_python()

# python code

import xarray as xr
import rioxarray
import numpy as np
import os.path
import geopandas as gpd

# clipping (deleting ocean pixels)
shp = gpd.read_file(os.path.join(".", "data", "raw", "vectorial", "Sudamérica.shp"))

new_x = np.arange(-81.4, -67.19, 0.01)
new_y = np.arange(-18.6, 1.21, 0.01)


for files in range(1, 13):
 file = str(files).zfill(2)
 cc_file = xr.open_rasterio(os.path.join(".", "data", "raw", "gridded", "CC", "CC_monthly", "Peru", "cloud_coverage_" + file + ".tif"))
 cc_file = cc_file.drop("band")
 
 # gap filling of empty pixels
 cc_file_filled = cc_file.rio.interpolate_na(method = "nearest")
 
 # to PISCOt grid
 cc_file_filled = cc_file_filled.reindex(y = new_y, x = new_x, method = "nearest")
 
 # clipping (deleting ocean pixels)
 cc_file_filled = cc_file_filled.rio.clip(shp.geometry, cc_file_filled.rio.crs)
 
 # saving
 cc_file_filled.rio.to_raster(os.path.join(".", "data", "raw", "gridded", "CC", "CC_monthly", "cloud_coverage_" + file + "_2.tif"))

 # why I did not saved as netcdf directly: https://github.com/pydata/xarray/issues/2535

exit