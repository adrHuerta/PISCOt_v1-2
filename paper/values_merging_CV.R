rm(list = ls())

library(xts)
library(raster)
library(spatialsample)
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

# kfold validation (spatial)
stations_CV <- qc_data$xyz[qc_data$xyz@data$filter_qc70 != 0, ]
set.seed(2020+1)
folds <- spatial_clustering_cv(stations_CV@data, coords = c("LON", "LAT"), v = 10)

#
for(i in 1:500){
  
  date_i <- time(qc_data$values$tmax)[i]
  
  parallel::mclapply(folds$splits,
                     function(cv_i){
                       
                       assessment_cv <- assessment(cv_i)$ID
                       assessment_cv <- match(assessment_cv, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[assessment_cv, ]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmax <- qc_data_cv$values$tmax[,-assessment_cv]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-assessment_cv,]

                       tmax_i <- make_Anomaly_coVariables(day_date = date_i,
                                                          var = "tmax",
                                                          covs_list = covs_list_tmax,
                                                          obs = qc_data_cv)
                       
                       # tmax_i_gridded <- RK(obs_cov_data = tmax_i, resFitting = 10) + tmax_i$covs$CL
                       tmax_i_gridded2 <- suppressWarnings(
                         RK(obs_cov_data = tmax_i, resFitting = 10,
                            extentArea = raster::extent(to_extract_value) + c(-1, 1, -1, 1)) + tmax_i$covs$CL
                         )
                       
                       extracted_valued <- raster::extract(tmax_i_gridded2, to_extract_value)
                       extracted_valued <- matrix(extracted_valued, nrow = 1, ncol = length(extracted_valued))
                       colnames(extracted_valued) <- assessment(cv_i)$ID
                       extracted_valued
                       
                     }, mc.cores = 10) -> tmax_cv_i
  
  saveRDS(object = do.call("cbind", tmax_cv_i) %>% .[, match(stations_CV@data$ID, colnames(.))],
          file = file.path(output_anomalies, sprintf("%s/tmax_spcv_%s.RDS", "tmax",  date_i)))
  
  
  parallel::mclapply(folds$splits,
                     function(cv_i){
                       
                       assessment_cv <- assessment(cv_i)$ID
                       assessment_cv <- match(assessment_cv, qc_data$xyz@data$ID)
                       to_extract_value  <- qc_data$xyz[assessment_cv, ]
                       
                       qc_data_cv <- qc_data
                       qc_data_cv$values$tmin <- qc_data_cv$values$tmin[,-assessment_cv]
                       qc_data_cv$xyz <- qc_data_cv$xyz[-assessment_cv,]
                       
                       tmin_i <- make_Anomaly_coVariables(day_date = date_i,
                                                          var = "tmin",
                                                          covs_list = covs_list_tmin,
                                                          obs = qc_data_cv)
                       
                       # tmin_i_gridded <- (RK(obs_cov_data = tmin_i, resFitting = 10) + tmin_i$covs$CL)
                       tmin_i_gridded2 <- suppressWarnings(
                         RK(obs_cov_data = tmin_i, resFitting = 10,
                            extentArea = raster::extent(to_extract_value) + c(-1, 1, -1, 1)) + tmin_i$covs$CL
                         )
                       
                       extracted_valued <- raster::extract(tmin_i_gridded2, to_extract_value)
                       extracted_valued <- matrix(extracted_valued, nrow = 1, ncol = length(extracted_valued))
                       colnames(extracted_valued) <- assessment(cv_i)$ID
                       extracted_valued
                       
                     }, mc.cores = 10) -> tmin_cv_i
  
  
  saveRDS(object =do.call("cbind", tmin_cv_i) %>% .[, match(stations_CV@data$ID, colnames(.))],
          file = file.path(output_anomalies, sprintf("%s/tmin_spcv_%s.RDS", "tmin",  date_i)))

}