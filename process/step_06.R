rm(list = ls())

library(raster)
"%>%" = magrittr::`%>%`

source('./src/process/Merging/MG_make_covariables.R')
source('./src/process/Merging/MG_GWRK.R')

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Normals_OBS.RDS")

# gridded
LST_day <- raster::brick("data/processed/gridded/LST_DAY.nc")
LST_night <- raster::brick("data/processed/gridded/LST_NIGHT.nc")
CC <- raster::brick("data/processed/gridded/CC.nc")
DEM <- raster::raster("data/processed/gridded/DEM.nc")
X <- raster::raster("data/processed/gridded/X.nc")
Y <- raster::raster("data/processed/gridded/Y.nc")

# making list of covs
covs_list_tmax <- list(dynamic = list(LST = LST_day, CC = CC),
                       static = list(DEM = DEM, X = X, Y = Y))

covs_list_tmin <- list(dynamic = list(LST = LST_night, CC = CC),
                       static = list(DEM = DEM, X = X, Y = Y))


make_Normal_coVariables(month_value = 5,
                        var = "tmin",
                        covs_list = covs_list_tmin,
                        obs = qc_data) -> exp

GWR_model_fitting(obs_cov_data = exp, resFitting = 10)










