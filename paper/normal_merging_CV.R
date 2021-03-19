rm(list = ls())

library(raster)
"%>%" = magrittr::`%>%`

source('./src/process/Merging/MG_make_covariables.R')
source('./src/process/Merging/MG_GWRK.R')

output_normals <- "./paper/others/normals"

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Normals_OBS.RDS")

# gridded
LST_day <- raster::brick("data/processed/gridded/co_variables/LST_DAY.nc")
LST_night <- raster::brick("data/processed/gridded/co_variables/LST_NIGHT.nc")
CC <- raster::brick("data/processed/gridded/co_variables/CC.nc")
DEM <- raster::raster("data/processed/gridded/co_variables/DEM.nc")
X <- raster::raster("data/processed/gridded/co_variables/X.nc")
Y <- raster::raster("data/processed/gridded/co_variables/Y.nc")

# making list of covs
covs_list_tmax <- list(dynamic = list(LST = LST_day),
                       static = list(DEM = DEM, X = X, Y = Y))

covs_list_tmin <- list(dynamic = list(LST = LST_night),
                       static = list(DEM = DEM, X = X, Y = Y))

#
stations_CV <- qc_data$xyz[qc_data$xyz@data$filter_qc70 != 0, ]$ID

# 
for(i in 1:12){
  
  parallel::mclapply(stations_CV,
                     function(cv_i){
                       
                       cv_i <- match(cv_i, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[cv_i,]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmax <- qc_data_cv$values$tmax[,-cv_i]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-cv_i,]
                       
                       tmax_i <- make_Normal_coVariables(month_value = i,
                                                         var = "tmax",
                                                         covs_list = covs_list_tmax,
                                                         obs = qc_data_cv)
                       
                       tmax_i_gridded <- GWRK(obs_cov_data = tmax_i, resFitting = 10)
                       raster::extract(tmax_i_gridded, to_extract_value)
                       
                     }, mc.cores = 5) -> tmax_cv_i
  
  saveRDS(object = unlist(tmax_cv_i),
          file = file.path(output_normals, sprintf("%s/tmax_%02d.RDS", "tmax",  i)))
  
  
  parallel::mclapply(stations_CV,
                     function(cv_i){
                       
                       cv_i <- match(cv_i, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[cv_i,]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmin <- qc_data_cv$values$tmin[,-cv_i]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-cv_i,]
                       
                       tmin_i <- make_Normal_coVariables(month_value = i,
                                                         var = "tmin",
                                                         covs_list = covs_list_tmin,
                                                         obs = qc_data_cv)
                       
                       tmin_i_gridded <- GWRK(obs_cov_data = tmin_i, resFitting = 10)
                       raster::extract(tmin_i_gridded, to_extract_value)
                       
                     }, mc.cores = 5) -> tmin_cv_i
  
  saveRDS(object = unlist(tmin_cv_i),
          file = file.path(output_normals, sprintf("%s/tmin_%02d.RDS", "tmin",  i)))
  
  }
