rm(list = ls())

# setting python venv 

reticulate::use_virtualenv("/home/adrian/Documents/Repos/prob_Budyko/venv", 
                           required = T)
reticulate::repl_python()

## from GEE
# MODIS terra v006

### python code ###

import ee

ee.Authenticate()
ee.Initialize()

region = ee.Geometry.Polygon(
  [[[-82, 2],
    [-82, -19],
    [-68, -19],
    [-68, 2]]])

lst_day = ee.ImageCollection('MODIS/006/MOD11A2').select("LST_Day_1km")
lst_night = ee.ImageCollection('MODIS/006/MOD11A2').select("LST_Night_1km")

months = ee.List.sequence(1, 12)

lst_day_mean_monthly = ee.ImageCollection.fromImages(months.map(
  lambda x: lst_day.filter(ee.Filter.calendarRange(x, x, 'month')).mean().set('month', x) ))

lst_night_mean_monthly = ee.ImageCollection.fromImages(months.map(
  lambda x: lst_night.filter(ee.Filter.calendarRange(x, x, 'month')).mean().set('month', x) ))

listOfImages_lst_day = lst_day_mean_monthly.toList(lst_day_mean_monthly.size())
listOfImages_lst_night = lst_night_mean_monthly.toList(lst_night_mean_monthly.size())

for i in range(12):
 task_config = {
    'scale': 1000,  
    'maxPixels' : 5265012984,
  }
 task_day = ee.batch.Export.image(ee.Image(listOfImages_lst_day.get(i)).clip(region), "MODIS_lst_day_" + str(i + 1).zfill(2), task_config)
 task_day.start()
 task_night = ee.batch.Export.image(ee.Image(listOfImages_lst_night.get(i)).clip(region), "MODIS_lst_night_" + str(i + 1).zfill(2), task_config)
 task_night.start()
 
exit


##### 

## from IRI data Library (expert mode) 
## MODIS terra v005

# SOURCES .USGS .LandDAAC .MODIS .1km .8day .version_005 .Terra .NSA .Night .LST
# Y (2N) (19S) RANGEEDGES
# X (82W) (68W) RANGEEDGES
# T 0.8 monthlyAverage
# T 12 splitstreamgrid
# [T2]average
## Link:
# https://iridl.ldeo.columbia.edu/SOURCES/.USGS/.LandDAAC/.MODIS/.1km/.8day/.version_005/.Terra/.NSA/.Night/.LST/Y/(2N)/(19S)/RANGEEDGES/X/(82W)/(68W)/RANGEEDGES/T/0.8/monthlyAverage/T/12/splitstreamgrid/%5BT2%5Daverage/datafiles.html
