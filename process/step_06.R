rm(list = ls())

library(raster)
library(gstat)
"%>%" = magrittr::`%>%`

source('./src/process/Merging/MG_make_covariables.R')
source('./src/process/Merging/MG_GWRK.R')

output_normals <- "./data/processed/gridded/Normals"


# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Normals_OBS.RDS")

# gridded
LST_day <- raster::brick("data/processed/gridded/co_variables/LST_DAY.nc")
LST_night <- raster::brick("data/processed/gridded/co_variables/LST_NIGHT.nc")
DEM <- raster::raster("data/processed/gridded/co_variables/DEM.nc")
X <- raster::raster("data/processed/gridded/co_variables/X.nc")
Y <- raster::raster("data/processed/gridded/co_variables/Y.nc")

# making list of covs
covs_list_tmax <- list(dynamic = list(LST = LST_day),
                       static = list(DEM = DEM, X = X, Y = Y))

covs_list_tmin <- list(dynamic = list(LST = LST_night),
                       static = list(DEM = DEM, X = X, Y = Y))


for(i in 1:12){
  
  tmax_i <- make_Normal_coVariables(month_value = i,
                                    var = "tmax",
                                    covs_list = covs_list_tmax,
                                    obs = qc_data)
  GWRK(obs_cov_data = tmax_i,
       resFitting = 10) %>%
    raster::writeRaster(x = ., 
                        filename = file.path(output_normals, 
                                             sprintf("%s/tmax_%02d.nc", "tmax",  i)))
  
  tmin_i <- make_Normal_coVariables(month_value = i,
                                    var = "tmin",
                                    covs_list = covs_list_tmin,
                                    obs = qc_data)
  GWRK(obs_cov_data = tmin_i,
       resFitting = 10) %>%
    raster::writeRaster(x = ., 
                        filename = file.path(output_normals, 
                                             sprintf("%s/tmin_%02d.nc", "tmin",  i)))
}
