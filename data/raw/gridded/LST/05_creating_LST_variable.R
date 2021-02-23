rm(list = ls())

library(raster)

lapply(seq(1, 12),
       function(z){
         grid = file.path(".", "data", "raw", "gridded", "LST", "DAY",
                          paste(formatC(z, width = 2, flag = "0"), "_2.tif", sep = ""))
         grid = raster(grid)
         flip(grid, direction = 'y')
       }) -> lst_day 

lapply(seq(1, 12),
       function(z){
         grid = file.path(".", "data", "raw", "gridded", "LST", "NIGHT",
                          paste(formatC(z, width = 2, flag = "0"), "_2.tif", sep = ""))
         grid = raster(grid)
         flip(grid, direction = 'y')
       }) -> lst_night 

lst_day = brick(lst_day)
projection(lst_day) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names(lst_day) = paste("LST_DAY_", formatC(1:12, width = 2, flag = "0"), sep = "")

lst_night = brick(lst_night)
projection(lst_night) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names(lst_night) = paste("LST_NIGHT_", formatC(1:12, width = 2, flag = "0"), sep = "")


writeRaster(lst_day, 
            filename = file.path(".", "data", "processed", "gridded", "LST_DAY.nc"),
            format = "CDF")

writeRaster(lst_night, 
            filename = file.path(".", "data", "processed", "gridded", "LST_NIGHT.nc"),
            format = "CDF")
