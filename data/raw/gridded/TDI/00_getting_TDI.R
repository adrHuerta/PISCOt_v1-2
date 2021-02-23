rm(list = ls())

library(raster)
source('./src/data/TDI_functions.R')

# using original DEM
elv = raster("./data/raw/gridded/ELV/ELV.tif")

# spatial window size of 3, 6, 9, 12, 15 km
# as in Oyler et al (2012)
lapply(c(3, 6, 9, 12, 15), 
       function(x){
         TDI_fun(raster_grid = elv,
                 moving_window = x)
       }) -> tdi

tdi = sum((brick(tdi)))

writeRaster(tdi,
            file.path(".", "data", "raw", "gridded", "TDI", "TDI.tif"))
