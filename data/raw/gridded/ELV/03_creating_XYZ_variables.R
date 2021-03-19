rm(list = ls())

library(raster)

elv = (raster(file.path(".", "data", "raw", "gridded", "ELV", "ELV2.tif")))
elv = flip(elv, direction = 'y')

projection(elv) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
names(elv) = "DEM"

xvar <- elv
xvar[, ] <- coordinates(xvar)[, 1]
names(xvar) = "X"

yvar <- elv
yvar[, ] <- coordinates(xvar)[, 2]
names(yvar) = "Y"


writeRaster(elv, 
            filename = file.path(".", "data", "processed", "gridded", "co_variables", "DEM.nc"),
            format = "CDF",
            overwrite = TRUE)

writeRaster(xvar,
            filename = file.path(".", "data", "processed", "gridded", "co_variables", "X.nc"),
            format = "CDF",
            overwrite = TRUE)

writeRaster(yvar, 
            filename = file.path(".", "data", "processed", "gridded", "co_variables", "Y.nc"),
            format = "CDF",
            overwrite = TRUE)
