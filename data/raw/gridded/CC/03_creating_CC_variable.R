rm(list = ls())

library(raster)

lapply(seq(1, 12),
       function(z){
         grid = file.path(".", "data", "raw", "gridded", "CC", "CC_monthly",
                          paste("cloud_coverage_", formatC(z, width = 2, flag = "0"), "_2.tif", sep = ""))
         grid = raster(grid)
         flip(grid, direction = 'y')
       }) -> cloud_cover 

cloud_cover = brick(cloud_cover)
projection(cloud_cover) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names(cloud_cover) = paste("CC_", formatC(1:12, width = 2, flag = "0"), sep = "")


writeRaster(cloud_cover, 
            filename = file.path(".", "data", "processed", "gridded", "co_variables", "CC.nc"),
            format = "CDF",
            overwrite = TRUE)
