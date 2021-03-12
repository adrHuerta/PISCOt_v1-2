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

# PISCOt grid
new_x = np.arange(-81.4, -67.19, 0.01)
new_y = np.arange(-18.6, 1.21, 0.01)

# shp
shp = gpd.read_file(os.path.join(".", "data", "raw", "vectorial", "Sudamérica.shp"))


for files in range(1, 13):
  file = str(files).zfill(2)
  lst_day = xr.open_rasterio(os.path.join(".", "data", "raw", "gridded", "LST", "DAY", file + ".tif"))
  lst_night = xr.open_rasterio(os.path.join(".", "data", "raw", "gridded", "LST", "NIGHT", file + ".tif"))
  
  lst_day = lst_day.drop("band")
  lst_night = lst_night.drop("band")
  
  lst_day_filled = lst_day.rio.interpolate_na(method = "nearest")
  lst_night_filled = lst_night.rio.interpolate_na(method = "nearest")
  
  lst_day_filled = lst_day_filled.reindex(y = new_y, x = new_x, method = "nearest")
  lst_night_filled = lst_night_filled.reindex(y = new_y, x = new_x, method = "nearest")
  
  lst_day_filled = lst_day_filled.rio.clip(shp.geometry.buffer(.25), lst_day_filled.rio.crs)
  lst_night_filled = lst_night_filled.rio.clip(shp.geometry.buffer(.25), lst_night_filled.rio.crs)
  
  lst_day_filled.rio.to_raster(os.path.join(".", "data", "raw", "gridded", "LST", "DAY", file + "_2.tif"))
  lst_night_filled.rio.to_raster(os.path.join(".", "data", "raw", "gridded", "LST", "NIGHT", file + "_2.tif"))

# why I did not saved as netcdf directly: https://github.com/pydata/xarray/issues/2535
  
exit