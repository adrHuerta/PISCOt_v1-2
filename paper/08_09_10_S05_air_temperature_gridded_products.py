# -*- coding: utf-8 -*-
"""air_temperature_gridded_products.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/192BUzQ3azkoPpimG47-wwVOA4ACIhvKg
"""

import xarray as xr
import numpy as np
import pandas as pd
import glob
import requests
# !pip install netcdf4
# !pip install rasterio
from zipfile import ZipFile

from google.colab import drive
drive.mount('/content/drive')

def FD_fun(x):
  FD_length = len(x[x < 0])
  if (np.isnan(x).all()):
    res = np.nan
  else:
    res = FD_length*100/len(x)
  
  return res

"""PISCOt v1.2 (available from drive)"""

# MTmax

PISCOt2_MTmax = []
for i in np.arange(1981, 2021):
  name_file = "tmax_daily_YI.nc".replace("YI", str(i))
  url_name_file = "/content/drive/MyDrive/DATA_research/PISCOt/version_release_candidate_rc/tmax/" + name_file
  file_data = xr.open_dataset(url_name_file)
  file_data = file_data.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
  file_data_index = file_data.resample(time="1Y").mean(dim="time")
  PISCOt2_MTmax.append(file_data_index)

xr.concat(PISCOt2_MTmax, dim="time").to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/MTmax.nc")

# MTmin

PISCOt2_MTmin = []
for i in np.arange(1981, 2021):
  print(i)
  name_file = "tmin_daily_YI.nc".replace("YI", str(i))
  url_name_file = "/content/drive/MyDrive/DATA_research/PISCOt/version_release_candidate_rc/tmin/" + name_file
  file_data = xr.open_dataset(url_name_file)
  file_data = file_data.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
  file_data_index = file_data.resample(time="1Y").mean(dim="time")
  PISCOt2_MTmin.append(file_data_index)

xr.concat(PISCOt2_MTmin, dim="time").to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/MTmin.nc")

# FD

PISCOt2_FD = []
for i in np.arange(1981, 2021):
  name_file = "tmin_daily_YI.nc".replace("YI", str(i))
  url_name_file = "/content/drive/MyDrive/DATA_research/PISCOt/version_release_candidate_rc/tmin/" + name_file
  file_data = xr.open_dataset(url_name_file)
  file_data = file_data.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
  file_data_index = xr.apply_ufunc(FD_fun, file_data, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).rename({"tmin":"fd"})
  PISCOt2_FD.append(file_data_index)

PISCOt2_FD = xr.concat(PISCOt2_FD, dim="time")
PISCOt2_FD["time"] = xr.concat(PISCOt2_MTmin, dim="time")["time"]
PISCOt2_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.2/FD.nc")

"""TERRACLIMATE"""

# for i in np.arange(1981, 2021):
#     name_file = "TerraClimate_tmax_YI.nc".replace("YI", str(i))
#     url_name_file = "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/" + name_file
#     filename = url_name_file.split("/")[-1]

#     print(filename)
#     with open("/content/drive/MyDrive/temporal/TerraClimate/" + filename, "wb") as f:
#       r = requests.get(url_name_file)
#       f.write(r.content)

# for i in np.arange(1981, 2021):
#     name_file = "TerraClimate_tmin_YI.nc".replace("YI", str(i))
#     url_name_file = "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/" + name_file
#     filename = url_name_file.split("/")[-1]

#     print(filename)
#     with open("/content/drive/MyDrive/temporal/TerraClimate/" + filename, "wb") as f:
#       r = requests.get(url_name_file)
#       f.write(r.content)

def nc_pre_pro(ds):
  ds = xr.open_dataset(ds)
  return ds.sel(lat=slice(-12,-18.5), lon=slice(-73.5, -68))

# MTmax

nc_file_tmax = sorted(glob.glob("/content/drive/MyDrive/temporal/TerraClimate/*_tmax_*.nc"))
nc_file_tmax = [nc_pre_pro(i).resample(time="1Y").mean(dim="time") for i in nc_file_tmax]
nc_file_tmax = xr.concat(nc_file_tmax, dim="time")
nc_file_tmax = nc_file_tmax.drop("crs").rename({"lat":"latitude","lon":"longitude"})
nc_file_tmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/MTmax.nc")

# MTmin

nc_file_tmin = sorted(glob.glob("/content/drive/MyDrive/temporal/TerraClimate/*_tmin_*.nc"))
nc_file_tmin = [nc_pre_pro(i).resample(time="1Y").mean(dim="time") for i in nc_file_tmin]
nc_file_tmin = xr.concat(nc_file_tmin, dim="time")
nc_file_tmin = nc_file_tmin.drop("crs").rename({"lat":"latitude","lon":"longitude"})
nc_file_tmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/TerraClimate/MTmin.nc")

"""ERA5Land (already downloaded from ERA5 repository\ for PISCOt)"""

# MTmax

nc_file_tmax = xr.open_dataset("/content/drive/MyDrive/temporal/ERA5_land_temp/ERA5land_tmax_1981_2020.nc")
nc_file_tmax = nc_file_tmax.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
nc_file_tmax = nc_file_tmax.resample(time="1Y").mean(dim="time")
nc_file_tmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/MTmax.nc")

# MTmin

nc_file_tmin = xr.open_dataset("/content/drive/MyDrive/temporal/ERA5_land_temp/ERA5land_tmin_1981_2020.nc")
nc_file_tmin = nc_file_tmin.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
nc_file_tmin = nc_file_tmin.resample(time="1Y").mean(dim="time")
nc_file_tmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/MTmin.nc")

# FD

nc_file_tmin = xr.open_dataset("/content/drive/MyDrive/temporal/ERA5_land_temp/ERA5land_tmin_1981_2020.nc")
nc_file_tmin = nc_file_tmin.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))

FD_file = []
for year in range(1981,2021):
  year_file = nc_file_tmin.sel(time = nc_file_tmin.time.dt.year == year)
  year_file = xr.apply_ufunc(FD_fun, year_file, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).rename({"tmin":"fd"})
  FD_file.append(year_file)

nc_file_FD = xr.concat(FD_file, dim="time")
nc_file_FD["time"] = nc_file_tmax["time"]
nc_file_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/ERA5_Land/FD.nc")

"""PISCOt v1.1 (alread downloaded from IRI Data Library)"""

with open("/content/drive/MyDrive/temporal/PISCOt_v1.1/tmax.nc", "wb") as f:
       r = requests.get("https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Temp/.v1p1/.tmax/.stable/.daily/.tmax/Y/%2810S%29%2819S%29RANGEEDGES/X/%2875W%29%2867W%29RANGEEDGES/data.nc")
       f.write(r.content)

with open("/content/drive/MyDrive/temporal/PISCOt_v1.1/tmin.nc", "wb") as f:
       r = requests.get("https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Temp/.v1p1/.tmin/.stable/.daily/.tmin/Y/%2810S%29%2819S%29RANGEEDGES/X/%2875W%29%2867W%29RANGEEDGES/data.nc")
       f.write(r.content)

# MTmax

nc_file_tmax = xr.open_dataset("/content/drive/MyDrive/temporal/PISCOt_v1.1/tmax.nc")
nc_file_tmax = nc_file_tmax.rename({"T":"time","Y":"latitude","X":"longitude"})
nc_file_tmax = nc_file_tmax.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
nc_file_tmax["time"] = pd.date_range(start='1981-01-01', end='2016-12-31', freq="D")
nc_file_tmax = nc_file_tmax.resample(time="1Y").mean(dim="time")
nc_file_tmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/MTmax.nc")

# MTmin

nc_file_tmin = xr.open_dataset("/content/drive/MyDrive/temporal/PISCOt_v1.1/tmin.nc")
nc_file_tmin = nc_file_tmin.rename({"T":"time","Y":"latitude","X":"longitude"})
nc_file_tmin = nc_file_tmin.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
nc_file_tmin["time"] = pd.date_range(start='1981-01-01', end='2016-12-31', freq="D")
nc_file_tmin = nc_file_tmin.resample(time="1Y").mean(dim="time")
nc_file_tmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/MTmin.nc")

# FD

nc_file_tmin = xr.open_dataset("/content/drive/MyDrive/temporal/PISCOt_v1.1/tmin.nc")
nc_file_tmin = nc_file_tmin.rename({"T":"time","Y":"latitude","X":"longitude"})
nc_file_tmin["time"] = pd.date_range(start='1981-01-01', end='2016-12-31', freq="D")
nc_file_tmin = nc_file_tmin.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))


FD_file = []
for year in range(1981,2017):
  year_file = nc_file_tmin.sel(time = nc_file_tmin.time.dt.year == year)
  year_file = xr.apply_ufunc(FD_fun, year_file, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).rename({"tmin":"fd"})
  FD_file.append(year_file)

nc_file_FD = xr.concat(FD_file, dim="time")
nc_file_FD["time"] = nc_file_tmax["time"]
nc_file_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/PISCOt_v1.1/FD.nc")

"""CHIRTS"""

file_url = "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRTS/.v1.0/.daily/.global/.0p05/.tmax/Y/%2811S%29%2819S%29RANGEEDGES/X/%2874W%29%2867.5W%29RANGEEDGES/T/%281%20Jan%20YIYI%29%2831%20Dec%20YIYI%29RANGEEDGES/data.nc"

for i in np.arange(1983, 2017):
    filename = "CHIRTS_tmax_YI.nc".replace("YI", str(i))
    url_name_file = file_url.replace("YIYI", str(i))

    with open("/content/drive/MyDrive/temporal/CHIRTS/" + filename, "wb") as f:
      r = requests.get(url_name_file)
      f.write(r.content)

file_url = "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRTS/.v1.0/.daily/.global/.0p05/.tmin/Y/%2811S%29%2819S%29RANGEEDGES/X/%2874W%29%2867.5W%29RANGEEDGES/T/%281%20Jan%20YIYI%29%2831%20Dec%20YIYI%29RANGEEDGES/data.nc"

for i in np.arange(1983, 2017):
    filename = "CHIRTS_tmin_YI.nc".replace("YI", str(i))
    url_name_file = file_url.replace("YIYI", str(i))

    with open("/content/drive/MyDrive/temporal/CHIRTS/" + filename, "wb") as f:
      r = requests.get(url_name_file)
      f.write(r.content)

# MTmax

CHIRTS_MTmax = []
for i in np.arange(1983, 2017):
  name_file = "CHIRTS_tmax_YI.nc".replace("YI", str(i))
  url_name_file = "/content/drive/MyDrive/temporal/CHIRTS/" + name_file
  file_data = xr.open_dataset(url_name_file)
  file_data = file_data.rename({"T":"time","Y":"latitude","X":"longitude"})
  file_data = file_data.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
  file_data["time"] = pd.date_range(start=str(i)+'-01-01', end=str(i)+'-12-31', freq="D")
  file_data_index = file_data.resample(time="1Y").mean(dim="time")
  CHIRTS_MTmax.append(file_data_index)

xr.concat(CHIRTS_MTmax, dim="time").to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/MTmax.nc")

# MTmin

CHIRTS_MTmin = []
for i in np.arange(1983, 2017):
  name_file = "CHIRTS_tmin_YI.nc".replace("YI", str(i))
  url_name_file = "/content/drive/MyDrive/temporal/CHIRTS/" + name_file
  file_data = xr.open_dataset(url_name_file)
  file_data = file_data.rename({"T":"time","Y":"latitude","X":"longitude"})
  file_data = file_data.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
  file_data["time"] = pd.date_range(start=str(i)+'-01-01', end=str(i)+'-12-31', freq="D")
  file_data_index = file_data.resample(time="1Y").mean(dim="time")
  CHIRTS_MTmin.append(file_data_index)

xr.concat(CHIRTS_MTmin, dim="time").to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/MTmin.nc")

# FD

CHIRTS_FD = []
for i in np.arange(1983, 2017):
  name_file = "CHIRTS_tmin_YI.nc".replace("YI", str(i))
  url_name_file = "/content/drive/MyDrive/temporal/CHIRTS/" + name_file
  file_data = xr.open_dataset(url_name_file)
  file_data = file_data.rename({"T":"time","Y":"latitude","X":"longitude"})
  file_data = file_data.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
  file_data_index = xr.apply_ufunc(FD_fun, file_data, vectorize=True, input_core_dims=[["time"]],output_dtypes=['float32']).rename({"tmin":"fd"})
  CHIRTS_FD.append(file_data_index)

CHIRTS_FD = xr.concat(CHIRTS_FD, dim="time")
CHIRTS_FD["time"] = xr.concat(CHIRTS_MTmin, dim="time")["time"]
CHIRTS_FD.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/CHIRTS/FD.nc")

"""VS2018"""

# import requests
# url = 'https://digital.csic.es/bitstream/10261/139347/2/Tmax.zip'
# req = requests.get(url)
# filename = "/content/drive/MyDrive/temporal/VS2018/Tmax.zip"
 
# with open(filename,'wb') as output_file:
#     output_file.write(req.content)

# url = 'https://digital.csic.es/bitstream/10261/139347/3/Tmin.zip'
# req = requests.get(url)
# filename = "/content/drive/MyDrive/temporal/VS2018/Tmin.zip"
 
# with open(filename,'wb') as output_file:
#     output_file.write(req.content)

with ZipFile("/content/drive/MyDrive/temporal/VS2018/Tmax.zip", 'r') as zip:
    zip.extractall("/content/drive/MyDrive/temporal/VS2018")
    zip.close()

with ZipFile("/content/drive/MyDrive/temporal/VS2018/Tmin.zip", 'r') as zip:
    zip.extractall("/content/drive/MyDrive/temporal/VS2018")
    zip.close()



# MTmax

nc_file_tmax = xr.open_dataset("/content/drive/MyDrive/temporal/VS2018/Tmax.nc", decode_times=False)
nc_file_tmax = nc_file_tmax.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
nc_file_tmax["time"] = pd.date_range(start='1964-01-01', end="2014-07-31", freq="M")
nc_file_tmax = nc_file_tmax.sel(time=slice('1981-01-01', '2013-12-31'))
nc_file_tmax = nc_file_tmax.rename({"temperatura":"tmax"})
nc_file_tmax = nc_file_tmax.where(nc_file_tmax['tmax'] > -50)
nc_file_tmax = nc_file_tmax.resample(time="1Y").mean(dim="time")
nc_file_tmax.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/MTmax.nc")

# MTmin

nc_file_tmin = xr.open_dataset("/content/drive/MyDrive/temporal/VS2018/Tmin.nc", decode_times=False)
nc_file_tmin = nc_file_tmin.sel(latitude=slice(-12,-18.5), longitude=slice(-73.5, -68))
nc_file_tmin["time"] = pd.date_range(start='1964-01-01', end="2014-07-31", freq="M")
nc_file_tmin = nc_file_tmin.sel(time=slice('1981-01-01', '2013-12-31'))
nc_file_tmin = nc_file_tmin.rename({"temperatura":"tmin"})
nc_file_tmin = nc_file_tmin.where(nc_file_tmin['tmin'] > -50)
nc_file_tmin = nc_file_tmin.resample(time="1Y").mean(dim="time")
nc_file_tmin.to_netcdf("/content/drive/MyDrive/PISCOt_big_data/global_gridded_products/VS2018/MTmin.nc")

nc_file_tmax.isel(time=0).tmax.plot()

