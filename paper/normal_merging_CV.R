rm(list = ls())

library(raster)
library(spatialsample)
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

# kfold validation (spatial)
set.seed(2020+1)
folds <- spatial_clustering_cv(qc_data$xyz@data, coords = c("LON", "LAT"), v = 10)

for(i in 1:12){
  
  parallel::mclapply(folds$splits,
                     function(cv_i){
                       
                       assessment_cv <- assessment(cv_i)$ID
                       assessment_cv <- match(assessment_cv, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[assessment_cv, ]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmax <- qc_data_cv$values$tmax[,-assessment_cv]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-assessment_cv,]
                       
                       tmax_i <- make_Normal_coVariables(month_value = i,
                                                         var = "tmax",
                                                         covs_list = covs_list_tmax,
                                                         obs = qc_data_cv)
                       
                       tmax_i_gridded <- GWRK(obs_cov_data = tmax_i, resFitting = 10)
                       
                       data.frame(ID = assessment(cv_i)$ID,
                                  model = raster::extract(tmax_i_gridded, to_extract_value),
                                  date = i)
                       
                     }, mc.cores = 10) -> tmax_cv_i
  
  saveRDS(object = do.call("rbind", tmax_cv_i),
          file = file.path(output_normals, sprintf("%s/tmax_spcv_%02d.RDS", "tmax",  i)))
  
  
  parallel::mclapply(folds$splits,
                     function(cv_i){
                       
                       assessment_cv <- assessment(cv_i)$ID
                       assessment_cv <- match(assessment_cv, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[assessment_cv, ]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmin <- qc_data_cv$values$tmin[,-assessment_cv]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-assessment_cv,]
                       
                       tmin_i <- make_Normal_coVariables(month_value = i,
                                                         var = "tmin",
                                                         covs_list = covs_list_tmin,
                                                         obs = qc_data_cv)
                       
                       tmin_i_gridded <- GWRK(obs_cov_data = tmin_i, resFitting = 10)
                       
                       data.frame(ID = assessment(cv_i)$ID,
                                  model = raster::extract(tmin_i_gridded, to_extract_value),
                                  date = i)
                       
                     }, mc.cores = 10) -> tmin_cv_i
  
  saveRDS(object = do.call("rbind", tmin_cv_i),
          file = file.path(output_normals, sprintf("%s/tmin_spcv_%02d.RDS", "tmin",  i)))
  
  }

# kfold validation (no spatial)
set.seed(2020+1)
folds <- rsample::vfold_cv(qc_data$xyz@data, v = 10)

for(i in 1:12){
  
  parallel::mclapply(folds$splits,
                     function(cv_i){
                       
                       assessment_cv <- assessment(cv_i)$ID
                       assessment_cv <- match(assessment_cv, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[assessment_cv, ]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmax <- qc_data_cv$values$tmax[,-assessment_cv]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-assessment_cv,]
                       
                       tmax_i <- make_Normal_coVariables(month_value = i,
                                                         var = "tmax",
                                                         covs_list = covs_list_tmax,
                                                         obs = qc_data_cv)
                       
                       tmax_i_gridded <- GWRK(obs_cov_data = tmax_i, resFitting = 10)
                       
                       data.frame(ID = assessment(cv_i)$ID,
                                  model = raster::extract(tmax_i_gridded, to_extract_value),
                                  date = i)
                       
                     }, mc.cores = 10) -> tmax_cv_i
  
  saveRDS(object = do.call("rbind", tmax_cv_i),
          file = file.path(output_normals, sprintf("%s/tmax_nospcv_%02d.RDS", "tmax",  i)))
  
  
  parallel::mclapply(folds$splits,
                     function(cv_i){
                       
                       assessment_cv <- assessment(cv_i)$ID
                       assessment_cv <- match(assessment_cv, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[assessment_cv, ]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmin <- qc_data_cv$values$tmin[,-assessment_cv]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-assessment_cv,]
                       
                       tmin_i <- make_Normal_coVariables(month_value = i,
                                                         var = "tmin",
                                                         covs_list = covs_list_tmin,
                                                         obs = qc_data_cv)
                       
                       tmin_i_gridded <- GWRK(obs_cov_data = tmin_i, resFitting = 10)
                       
                       data.frame(ID = assessment(cv_i)$ID,
                                  model = raster::extract(tmin_i_gridded, to_extract_value),
                                  date = i)
                       
                     }, mc.cores = 10) -> tmin_cv_i
  
  saveRDS(object = do.call("rbind", tmin_cv_i),
          file = file.path(output_normals, sprintf("%s/tmin_nospcv_%02d.RDS", "tmin",  i)))
  
}
