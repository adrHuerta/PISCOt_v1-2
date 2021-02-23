rm(list = ls())

library(rgee)
library(raster)
source("./src/data/PISCOt-MOD11A2_NIGHT.R")

reticulate::use_virtualenv("/home/adrian/Documents/Repos/prob_Budyko/venv", 
                           required = T)

ee_reattach()
ee_Initialize(email = "adrhuerta@gmail.com")

for(month_n in 1:12){
  
  mod11a2 <- ee$ImageCollection("MODIS/006/MOD11A2")$
    filter(ee$Filter$date('2001-01-01', '2019-12-31'))$
    filter(ee$Filter$calendarRange(month_n, field = "month"))
  
  nimg <- mod11a2$size()$getInfo()
  
  # Raw Temperature MODIS LST day 1km
  mod11a2_raw <- mod11a2
  mod11a2_raw_npixels <- mod11a2_raw$map(count_pixels)$sum()
  mod11a2_composite_raw <- mod11a2$mean()$
    select("LST_Night_1km")$
    multiply(0.02)$
    subtract(273.15)
  
  # QA quality Temperature MODIS LST day 1km
  mod11a2_clean <- mod11a2$map(mod11A2_clean)
  mod11a2_clean_npixels <- mod11a2_clean$map(count_pixels)$sum()
  
  # at least 10% of pixels
  mask <- mod11a2_clean_npixels$gte(round(nimg*0.20))
  mod11a2_composite_clean <- mod11a2_clean$median()$updateMask(mask)
  
  pisco_area <-  ee$Geometry$Rectangle(-81.4 - 0.03,
                                       -18.75 - 0.03,
                                       -68.05 + 0.03,
                                       0.95 + 0.03)
  
    #mod11a2
    ee_image_as_raster(
    image = mod11a2_composite_clean,
    region = pisco_area,
    dsn = file.path(".", "data", "raw", "gridded", "LST", "NIGHT",
                    paste(formatC(month_n, width = 2, flag = "0"), ".tif", sep = "")),
    scale = 1000,
    via = "drive",
    container = "LST"
    )
    
      # mod11a2_npixels
      ee_image_as_raster(
      image = mod11a2_clean_npixels,
      region = pisco_area,
      dsn = file.path(".", "data", "raw", "gridded", "LST", "NIGHT", "npixels",
                      paste(formatC(month_n, width = 2, flag = "0"), ".tif", sep = "")),
      scale = 1000,
      via = "drive",
      container = "LST"
      )
      
}

