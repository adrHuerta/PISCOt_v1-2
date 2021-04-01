rm(list = ls())

library(raster)

tdi = (raster(file.path(".", "data", "raw", "gridded", "TDI", "TDI2.tif")))
tdi = flip(tdi, direction = 'y')

projection(tdi) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names(tdi) = "TDI"

writeRaster(tdi, 
            filename = file.path(".", "data", "processed", "gridded", "co_variables", "TDI.nc"),
            format = "CDF",
            overwrite = TRUE)
