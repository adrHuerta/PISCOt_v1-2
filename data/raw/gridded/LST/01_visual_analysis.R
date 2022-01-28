rm(list = ls())

library(rgee)
library(raster)
source("./src/data/PISCOt-MOD11A2_DAY.R")

reticulate::use_virtualenv("/home/adrian/Documents/Repos/prob_Budyko/venv", 
                           required = T)

ee_Initialize()

# Read dataset
mod11a2 <- ee$ImageCollection("MODIS/006/MOD11A2")$
  filter(ee$Filter$date('2001-01-01', '2020-12-31'))$
  filter(ee$Filter$calendarRange(1, field = "month"))       

nimg <- mod11a2$size()$getInfo()

# Raw Temperature MODIS LST day 1km
mod11a2_raw <- mod11a2
mod11a2_raw_npixels <- mod11a2_raw$map(count_pixels)$sum()
mod11a2_composite_raw <- mod11a2$mean()$
  select("LST_Day_1km")$
  multiply(0.02)$
  subtract(273.15)

# QA quality Temperature MODIS LST day 1km
mod11a2_clean <- mod11a2$map(mod11A2_clean)
mod11a2_clean_npixels <- mod11a2_clean$map(count_pixels)$sum()

# at least 10% of pixels
mask <- mod11a2_clean_npixels$gte(round(nimg*0.20))
mod11a2_composite_clean <- mod11a2_clean$median()$updateMask(mask)

# GeoVIZ
lst_viz <- list(min = 15 , max = 30, palette = temperature_palette) #day 15 to 30 / night -15 to 30
npixel_viz <- list(min = 0 , max = nimg, palette = temperature_palette)

# Display maps
Map$setCenter(-74.16747, -10.42585, 5)
Map$addLayer(mod11a2_composite_raw, lst_viz, name = "raw_mod11") +
  Map$addLayer(mod11a2_raw_npixels, npixel_viz, name = "raw_npixels") +
  Map$addLayer(mod11a2_composite_clean, lst_viz, name = "clean_mod11") +
  Map$addLayer(mod11a2_clean_npixels, npixel_viz, name = "clean_npixels")

# #Donwload image
# pisco_area <-  ee$Geometry$Rectangle(-81.4 - 0.03,
#                                      -18.75 - 0.03,
#                                      -68.05 + 0.03,
#                                      0.95 + 0.03)
# mod11a2 <- ee_image_as_raster(
#   image = mod11a2_composite_clean,
#   region = pisco_area,
#   dsn = "mod11a2_12.tif",
#   scale = 1000,
#   via = "getInfo" # for large image use drive
# )
#
# mod11a2 <- ee_image_as_raster(
#   image = mod11a2_composite_clean,
#   region = pisco_area,
#   dsn = "mod11a2_12.tif", # change folder name
#   scale = 1000,
#   container = "LST",
#   via = "drive" # for large image use drive
# )

# ee_image_as_raster(
#     image = mod11a2_composite_raw,
#     region = pisco_area,
#     dsn = file.path(".", "data", "raw", "gridded", "LST", "original_grid.tif"), # change folder name
#     scale = 1000,
#     via = "drive" # for large image use drive
#   )

## analysis (example)
# lst_day
# january: changing 2 to 1 (emi.. and lst.. error) there is not too much difference (gaps in the Andes-Amazon area), number of pixels plays a major role (from 60 to 40 -> less gaps in the amazon and Andes-Amazon area)
#        : changing to 0, more empty pixel (close to the coast) -> bad option
#        : less pixels in the Andes-Amazon area -> cloudinnes matter
# june: more pixels in all the area of Peru -> dry season - no clouds (expcet coastal area (close to the sea))
#     : numbers of pixels does not play a major role, and changing 2 to 0, less pixels are found in the coastal area (more importantly in the northern)
#     : however, the number of gaps is less than january 
# lst_night
# january: more avaibility of pixel in the andeas area (north to south), northern coast and amazon less -> thus, number of pixels plays a major role
#        : from 0 to 2 (more data in the southern coastal area at 70% of data)
# june: similar to June (lst_day), but less pixels in ther nothern coastal area
#     : from 2 to 0 <- less data in the northern
#     : same, the number of gaps is less than january 

# decision: at least 20% of data at 2 or 1 (after imputation results) for both: day and night -> 
# reasons: +QC +climatology from LST values rather than gap-filled values (using 20%), to reduce outliers: MEDIAN rather than MEAN

