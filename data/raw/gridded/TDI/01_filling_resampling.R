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

tdi = xr.open_rasterio(os.path.join(".", "data", "raw", "gridded", "TDI", "TDI.tif"))
tdi = tdi.where(tdi >= 0.00) 
tdi = tdi.drop("band")

# PISCOt grid
new_x = np.arange(-81.4, -67.19, 0.01)
new_y = np.arange(-18.6, 1.21, 0.01)

# gap filling with 0
tdi_filled = tdi.fillna(0)

# to PISCOt grid
tdi_filled = tdi_filled.reindex(y = new_y, x = new_x, method = "nearest")

# clipping (deleting ocean pixels)
shp = gpd.read_file(os.path.join(".", "data", "raw", "vectorial", "Sudamérica.shp"))
tdi_filled = tdi_filled.rio.clip(shp.geometry.buffer(.15), tdi_filled.rio.crs)
tdi_filled = tdi_filled.where(tdi_filled >= 0.00)

# saving
tdi_filled.rio.to_raster(os.path.join(".", "data", "raw", "gridded", "TDI", "TDI2.tif"))

exit
