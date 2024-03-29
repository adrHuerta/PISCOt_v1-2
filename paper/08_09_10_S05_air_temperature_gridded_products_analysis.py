# -*- coding: utf-8 -*-
"""air_temperature_gridded_products_analysis.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/15XixHVIxuTrGqiaa2XHcajTdHW4skPpt
"""

import xarray as xr
import numpy as np
import pandas as pd

from google.colab import drive
drive.mount('/content/drive')

from scipy import stats

def sen_trend(x):
  
  if (np.isnan(x).all()):
    res = np.nan
  elif (np.var(x) == 0):
    res = 0
  else:
    res = stats.theilslopes(x, range(len(x)), 0.95)[0]
  
  return(res)

"""Annual mean values"""

PISCOt_v11_MTmax = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/MTmax.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
PISCOt_v12_MTmax = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/MTmax.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
CHIRTS_MTmax = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/MTmax.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
VS2018_MTmax = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/MTmax.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
TerraClimate_MTmax = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/MTmax.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
ERA5land_MTmax = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/MTmax.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")

PISCOt_v11_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
PISCOt_v12_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
CHIRTS_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
VS2018_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
TerraClimate_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
ERA5land_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")

PISCOt_v11_FD = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/FD.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
PISCOt_v12_FD = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/FD.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
CHIRTS_FD = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/FD.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
#VS2018_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
#TerraClimate_MTmin = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/MTmin.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")
ERA5land_FD = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/FD.nc").sel(time=slice('1981-01-01', '2010-12-31')).mean(dim="time")

PISCOt_v11_MTmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_PISCOt-v1.1_MTmax.nc")
PISCOt_v12_MTmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_PISCOt-v1.2_MTmax.nc")
CHIRTS_MTmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_CHIRTS_MTmax.nc")
VS2018_MTmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_VS2018_MTmax.nc")
TerraClimate_MTmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_TerraClimate_MTmax.nc")
ERA5land_MTmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_ERA5-Land_MTmax.nc")

PISCOt_v11_MTmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_PISCOt-v1.1_MTmin.nc")
PISCOt_v12_MTmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_PISCOt-v1.2_MTmin.nc")
CHIRTS_MTmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_CHIRTS_MTmin.nc")
VS2018_MTmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_VS2018_MTmin.nc")
TerraClimate_MTmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_TerraClimate_MTmin.nc")
ERA5land_MTmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_ERA5-Land_MTmin.nc")

PISCOt_v11_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_PISCOt-v1.1_FD.nc")
PISCOt_v12_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_PISCOt-v1.2_FD.nc")
CHIRTS_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_CHIRTS_FD.nc")
# VS2018_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_VS2018_FD.nc")
# TerraClimate_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_TerraClimate_FD.nc")
ERA5land_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/mean_ERA5-Land_FD.nc")

"""Compute trends"""

PISCOt_v11_MTmax_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/MTmax.nc").sel(time=slice('1983-01-01', '2013-12-31'))
PISCOt_v12_MTmax_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/MTmax.nc").sel(time=slice('1983-01-01', '2013-12-31'))
CHIRTS_MTmax_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/MTmax.nc").sel(time=slice('1983-01-01', '2013-12-31'))
VS2018_MTmax_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/MTmax.nc").sel(time=slice('1983-01-01', '2013-12-31'))
TerraClimate_MTmax_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/MTmax.nc").sel(time=slice('1983-01-01', '2013-12-31'))
ERA5land_MTmax_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/MTmax.nc").sel(time=slice('1983-01-01', '2013-12-31'))

PISCOt_v11_MTmin_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/MTmin.nc").sel(time=slice('1983-01-01', '2013-12-31'))
PISCOt_v12_MTmin_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/MTmin.nc").sel(time=slice('1983-01-01', '2013-12-31'))
CHIRTS_MTmin_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/MTmin.nc").sel(time=slice('1983-01-01', '2013-12-31'))
VS2018_MTmin_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/MTmin.nc").sel(time=slice('1983-01-01', '2013-12-31'))
TerraClimate_MTmin_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/MTmin.nc").sel(time=slice('1983-01-01', '2013-12-31'))
ERA5land_MTmin_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/MTmin.nc").sel(time=slice('1983-01-01', '2013-12-31'))

PISCOt_v11_FD_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/FD.nc").sel(time=slice('1983-01-01', '2013-12-31'))
PISCOt_v12_FD_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/FD.nc").sel(time=slice('1983-01-01', '2013-12-31'))
CHIRTS_FD_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/FD.nc").sel(time=slice('1983-01-01', '2013-12-31'))
# VS2018_FD_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/FD.nc").sel(time=slice('1983-01-01', '2013-12-31'))
# TerraClimate_FD_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/FD.nc").sel(time=slice('1983-01-01', '2013-12-31'))
ERA5land_FD_trend = xr.open_dataset("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/FD.nc").sel(time=slice('1983-01-01', '2013-12-31'))

xr.apply_ufunc(sen_trend, PISCOt_v11_MTmax_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_PISCOt-v1.1_MTmax.nc")
xr.apply_ufunc(sen_trend, PISCOt_v12_MTmax_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_PISCOt-v1.2_MTmax.nc")
xr.apply_ufunc(sen_trend, CHIRTS_MTmax_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_CHIRTS_MTmax.nc")
xr.apply_ufunc(sen_trend, VS2018_MTmax_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_VS2018_MTmax.nc")
xr.apply_ufunc(sen_trend, TerraClimate_MTmax_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_TerraClimate_MTmax.nc")
xr.apply_ufunc(sen_trend, ERA5land_MTmax_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_ERA5-Land_MTmax.nc")

xr.apply_ufunc(sen_trend, PISCOt_v11_MTmin_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_PISCOt-v1.1_MTmin.nc")
xr.apply_ufunc(sen_trend, PISCOt_v12_MTmin_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_PISCOt-v1.2_MTmin.nc")
xr.apply_ufunc(sen_trend, CHIRTS_MTmin_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_CHIRTS_MTmin.nc")
xr.apply_ufunc(sen_trend, VS2018_MTmin_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_VS2018_MTmin.nc")
xr.apply_ufunc(sen_trend, TerraClimate_MTmin_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_TerraClimate_MTmin.nc")
xr.apply_ufunc(sen_trend, ERA5land_MTmin_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_ERA5-Land_MTmin.nc")

xr.apply_ufunc(sen_trend, PISCOt_v11_FD_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_PISCOt-v1.1_FD.nc")
xr.apply_ufunc(sen_trend, PISCOt_v12_FD_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_PISCOt-v1.2_FD.nc")
xr.apply_ufunc(sen_trend, CHIRTS_FD_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_CHIRTS_FD.nc")
# xr.apply_ufunc(sen_trend, VS2018_FD_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_VS2018_FD.nc")
# xr.apply_ufunc(sen_trend, TerraClimate_FD_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_TerraClimate_FD.nc")
xr.apply_ufunc(sen_trend, ERA5land_FD_trend, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/slope_ERA5-Land_FD.nc")

