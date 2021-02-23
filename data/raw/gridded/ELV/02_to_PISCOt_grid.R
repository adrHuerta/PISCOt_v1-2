rm(list = ls())

reticulate::use_virtualenv("/home/adrian/Documents/Repos/prob_Budyko/venv", 
                           required = T)
reticulate::repl_python()

# python code

import xarray as xr
import rioxarray
import numpy as np
import os.path
import geopandas as gpd

elv = xr.open_rasterio(os.path.join(".", "data", "raw", "gridded", "ELV", "ELV.tif"))
elv = elv.where(elv >= 0.00)

# PISCOt grid
new_x = np.arange(-81.4, -67.19, 0.01)
new_y = np.arange(-18.6, 1.21, 0.01)

# gap filling of empty pixels
elv = elv.drop("band")
elv_filled = elv.rio.interpolate_na(method = "nearest")

# to PISCOt grid
elv_filled = elv_filled.reindex(y = new_y, x = new_x, method = "nearest")

# clipping (deleting ocean pixels)
shp = gpd.read_file(os.path.join(".", "data", "raw", "vectorial", "Sudamérica.shp"))
elv_filled = elv_filled.rio.clip(shp.geometry, elv_filled.rio.crs)

# saving
elv_filled.rio.to_raster(os.path.join(".", "data", "raw", "gridded", "ELV", "ELV2.tif"))

exit
