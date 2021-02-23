## python-gee version

rm(list = ls())

# setting python venv 

reticulate::use_virtualenv("/home/adrian/Documents/Repos/prob_Budyko/venv", 
                           required = T)
reticulate::repl_python()

### python code ###

import ee

ee.Authenticate()
ee.Initialize()

dem = ee.Image('USGS/GMTED2010')

region = ee.Geometry.Polygon(
  [[[-82, 2],
    [-82, -19],
    [-68, -19],
    [-68, 2]]])

dem_clipped = dem.clip(region)

task_config = { 
  'scale': 1000,  #1km 
  'maxPixels' : 5265012984,
}

task = ee.batch.Export.image(dem_clipped, "ELV_GMTED2010", task_config)
task.start()

exit

### python code ###