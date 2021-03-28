rm(list = ls())

library(raster)
library(gstat)
"%>%" = magrittr::`%>%`

source('./src/process/Merging/MG_make_covariables.R')
source('./src/process/Merging/MG_RK.R')

output_anomalies <- "./data/processed/gridded/Anomalies"

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Anomalies_OBS.RDS")

# gridded
LST_day <- raster::brick("data/processed/gridded/co_variables/LST_DAY.nc")
LST_night <- raster::brick("data/processed/gridded/co_variables/LST_NIGHT.nc")
DEM <- raster::raster("data/processed/gridded/co_variables/DEM.nc")
X <- raster::raster("data/processed/gridded/co_variables/X.nc")
Y <- raster::raster("data/processed/gridded/co_variables/Y.nc")
tdi <- raster::raster("./data/processed/gridded/co_variables/TDI.nc")


# gridded
tmax_normals <- file.path("./data/processed/gridded/Normals", 
                          sprintf("%s/tmax_%02d.nc", "tmax",  1:12)) %>%
  lapply(function(x) raster::raster(x)) %>%
  raster::brick() 

tmin_normals <- file.path("./data/processed/gridded/Normals", 
                          sprintf("%s/tmin_%02d.nc", "tmin",  1:12)) %>%
  lapply(function(x) raster::raster(x)) %>%
  raster::brick()


# making list of covs
covs_list_tmax <- list(dynamic = list(LST = LST_day),
                       static = list(DEM = DEM, X = X, Y = Y, TDI = tdi))

covs_list_tmin <- list(dynamic = list(LST = LST_night),
                       static = list(DEM = DEM, X = X, Y = Y, TDI = tdi))


for(i in 1:10){
  
  date_i <- time(qc_data$values$tmax)[i]
  month_value_i <- as.numeric(format(as.Date(date_i), "%m"))
  
  tmax_i <- make_Anomaly_coVariables(day_date = date_i,
                                     var = "tmax",
                                     covs_list = covs_list_tmax,
                                     obs = qc_data)
  
  (RK(obs_cov_data = tmax_i, resFitting = 10) + tmax_normals[[month_value_i]]) %>%
    raster::writeRaster(x = ., 
                        filename = file.path(output_anomalies, 
                                             sprintf("%s/tmax_%s.nc", "tmax",  date_i)))
  
  
  
  tmin_i <- make_Anomaly_coVariables(day_date = date_i,
                                     var = "tmin",
                                     covs_list = covs_list_tmin,
                                     obs = qc_data)
  
  (RK(obs_cov_data = tmin_i, resFitting = 10) + tmin_normals[[month_value_i]]) %>%
    raster::writeRaster(x = ., 
                        filename = file.path(output_anomalies, 
                                             sprintf("%s/tmin_%s.nc", "tmin",  date_i)))
}