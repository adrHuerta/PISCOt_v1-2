## r-gee version

rm(list = ls())

library(rgee)
library(raster)

reticulate::use_virtualenv("/home/adrian/Documents/Repos/prob_Budyko/venv", 
                           required = T)

ee_reattach()
ee_Initialize(email = "adrhuerta@gmail.com")

gmted2010 <- ee$Image('USGS/GMTED2010')

# adding 0.03° to avoid any error in the contours after regridding
pisco_area <-  ee$Geometry$Rectangle(-81.4 - 0.03, 
                                     -18.6 - 0.03, 
                                     -67.2 + 0.03, 
                                     1.2 + 0.03)

ee_image_as_raster(
    image = gmted2010,
    region = pisco_area,
    dsn = file.path(".", "data", "raw", "gridded", "ELV", "ELV.tif"),
    scale = 1000,
    via = "drive"
  )