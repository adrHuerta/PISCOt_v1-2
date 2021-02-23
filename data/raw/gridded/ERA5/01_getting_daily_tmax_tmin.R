rm(list = ls())

# setting python venv 
reticulate::use_virtualenv("/home/adrian/Documents/Repos/ERA5_2m_air_temp/venv/", 
                           required = T)
reticulate::repl_python()

import cdsapi
import xarray as xr
from pandas import date_range, Timedelta
from cdo import Cdo
import glob
cdo = Cdo()

c = cdsapi.Client()

for year in range(1960, 1979):
  c.retrieve("reanalysis-era5-single-levels-preliminary-back-extension",
             {
               "variable": "2m_temperature",
               "product_type": "reanalysis",
               "year": str(year),
               'month': ['01', '02', '03',
                         '04', '05', '06',
                         '07', '08', '09',
                         '10', '11', '12'],
               'day': ['01', '02', '03',
                       '04', '05', '06',
                       '07', '08', '09',
                       '10', '11', '12',
                       '13', '14', '15',
                       '16', '17', '18',
                       '19', '20', '21',
                       '22', '23', '24',
                       '25', '26', '27',
                       '28', '29', '30',
                       '31'],
               'time': ['00:00', '01:00', '02:00',
                        '03:00', '04:00', '05:00',
                        '06:00', '07:00', '08:00',
                        '09:00', '10:00', '11:00',
                        '12:00', '13:00', '14:00',
                        '15:00', '16:00', '17:00',
                        '18:00', '19:00', '20:00',
                        '21:00', '22:00', '23:00'],
               "area": "2/-82/-19/-66",  # North, West, South, East
               "format": "netcdf"
             },
             ".data/raw/gridded/ERA5/" + str(year) + ".nc")
for year in range(1979, 2020):
  c.retrieve("reanalysis-era5-single-levels",
             {
               "variable": "2m_temperature",
               "product_type": "reanalysis",
               "year": str(year),
               'month': ['01', '02', '03',
                         '04', '05', '06',
                         '07', '08', '09',
                         '10', '11', '12'],
               'day': ['01', '02', '03',
                       '04', '05', '06',
                       '07', '08', '09',
                       '10', '11', '12',
                       '13', '14', '15',
                       '16', '17', '18',
                       '19', '20', '21',
                       '22', '23', '24',
                       '25', '26', '27',
                       '28', '29', '30',
                       '31'],
               'time': ['00:00', '01:00', '02:00',
                        '03:00', '04:00', '05:00',
                        '06:00', '07:00', '08:00',
                        '09:00', '10:00', '11:00',
                        '12:00', '13:00', '14:00',
                        '15:00', '16:00', '17:00',
                        '18:00', '19:00', '20:00',
                        '21:00', '22:00', '23:00'],
               "area": "2/-82/-19/-66",  # North, West, South, East
               "format": "netcdf"
             },
             ".data/raw/gridded/ERA5/" + str(year) + ".nc")
for year in range(2020, 2021):
  c.retrieve("reanalysis-era5-single-levels",
             {
               "variable": "2m_temperature",
               "product_type": "reanalysis",
               "year": str(year),
               'month': ['01'],
               'day': ['01'],
               'time': ['00:00', '01:00', '02:00',
                        '03:00', '04:00', '05:00',
                        '06:00', '07:00', '08:00',
                        '09:00', '10:00', '11:00',
                        '12:00', '13:00', '14:00',
                        '15:00', '16:00', '17:00',
                        '18:00', '19:00', '20:00',
                        '21:00', '22:00', '23:00'],
               "area": "2/-82/-19/-66",  # North, West, South, East
               "format": "netcdf"
             },
             "./data/raw/gridded/ERA5/" + str(year) + ".nc")

cdo.cleanTempDir()

ERA5_t2m_temp_files = []
for nc_years in sorted(glob.glob("./data/raw/gridded/ERA5/*.nc")):
  ERA5_t2m = xr.open_dataset(nc_years)
  ERA5_t2m = ERA5_t2m - 273.15
  ERA5_t2m["time"] = date_range(nc_years.split("/")[2].split(".")[0] + '-01-01', freq="H", periods=len(ERA5_t2m.time)) - Timedelta(hours=5)
  ERA5_t2m_temp_files.append(cdo.cat(input=ERA5_t2m))

ERA5_t2m = cdo.cat(input=' '.join(ERA5_t2m_temp_files))
ERA5_t2m_tmax = cdo.daymax(input=ERA5_t2m)
ERA5_t2m_tmin = cdo.daymin(input=ERA5_t2m)

xr.open_dataset(ERA5_t2m_tmax).\
drop("time_bnds").\
sel(time=slice("1960-01-01", "2019-12-31")).\
to_netcdf("./data/processed/gridded/ERA5_t2m_tmax_1960_2019.nc")

xr.open_dataset(ERA5_t2m_tmin).\
drop("time_bnds").\
sel(time=slice("1960-01-01", "2019-12-31")).\
to_netcdf("./data/processed/gridded/ERA5_t2m_tmin_1960_2019.nc")