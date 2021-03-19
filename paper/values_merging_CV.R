rm(list = ls())

library(xts)
library(raster)
"%>%" = magrittr::`%>%`

source('./src/process/Merging/MG_make_covariables.R')
source('./src/process/Merging/MG_RK.R')

output_anomalies <- "./paper/others/values"

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Anomalies_OBS.RDS")

# gridded
tmax_normals <- file.path("./data/processed/gridded/Normals", 
                          sprintf("%s/tmax_%02d.nc", "tmax",  1:12)) %>%
  lapply(function(x) raster::raster(x)) %>%
  raster::brick() 

tmin_normals <- file.path("./data/processed/gridded/Normals", 
                          sprintf("%s/tmin_%02d.nc", "tmin",  1:12)) %>%
  lapply(function(x) raster::raster(x)) %>%
  raster::brick()

tdi <- raster::raster("./data/processed/gridded/co_variables/TDI.nc")

# making list of covs
covs_list_tmax <- list(dynamic = list(CL = tmax_normals),
                       static = list(TDI = tdi))

covs_list_tmin <- list(dynamic = list(CL = tmin_normals),
                       static = list(TDI = tdi))

#
stations_CV <- qc_data$xyz[qc_data$xyz@data$filter_qc70 != 0, ]$ID

#
for(i in 1:10){
  
  date_i <- time(qc_data$values$tmax)[i]
  
  parallel::mclapply(stations_CV,
                     function(cv_i){
                       
                       cv_i <- match(cv_i, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[cv_i,]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmax <- qc_data_cv$values$tmax[,-cv_i]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-cv_i,]
                       
                       tmax_i <- make_Anomaly_coVariables(day_date = date_i,
                                                          var = "tmax",
                                                          covs_list = covs_list_tmax,
                                                          obs = qc_data_cv)
                       
                       tmax_i_gridded <- RK(obs_cov_data = tmax_i, resFitting = 10) + tmax_i$covs$CL
                       raster::extract(tmax_i_gridded, to_extract_value)
                       
                     }, mc.cores = 5) -> tmax_cv_i
  
  saveRDS(object = unlist(tmax_cv_i),
          file = file.path(output_anomalies, sprintf("%s/tmax_%s.RDS", "tmax",  date_i)))
  
  
  parallel::mclapply(stations_CV,
                     function(cv_i){
                       
                       cv_i <- match(cv_i, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[cv_i,]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmin <- qc_data_cv$values$tmin[,-cv_i]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-cv_i,]
                       
                       tmin_i <- make_Anomaly_coVariables(day_date = date_i,
                                                          var = "tmin",
                                                          covs_list = covs_list_tmin,
                                                          obs = qc_data_cv)
                       
                       
                       tmin_i_gridded <- (RK(obs_cov_data = tmin_i, resFitting = 10) + tmin_i$covs$CL)
                       raster::extract(tmin_i_gridded, to_extract_value)
                       
                     }, mc.cores = 5) -> tmin_cv_i
  
  
  saveRDS(object = unlist(tmin_cv_i),
          file = file.path(output_anomalies, sprintf("%s/tmin_%s.RDS", "tmin",  date_i)))

}