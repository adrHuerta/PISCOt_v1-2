rm(list = ls())
"%>%" = magrittr::`%>%`

# n cores
n_cores = 6

source("./src/process/GapFilling/GF_dreqm.R")
source('./src/process/QC/QC_extreme_value_check.R')
source('./src/process/GapFilling/GF_daily_climatology_filling.R')

#
qc_data <- readRDS("./data/processed/obs/qc_output/QC_data.RDS")
era_qc_data_xy <- qc_data$xyz[qc_data$xyz$filter_qc == 1, ]
era_qc_data_xy$ID <- paste("ERA5", era_qc_data_xy$ID, sep = "_")
era_qc_data_xy$SRC <- "ERA5"

IDs_no_Peru <- qc_data$xyz[(qc_data$xyz$filter_qc == 1) & (qc_data$xyz$SRC != "SENAMHI"), ]$ID

# getting ERA5 time series from XY points

ERA5land_tmax <- raster::brick("./data/processed/gridded/co_variables/ERA5land_tmax_1981_2019.nc")
ERA5land_tmin <- raster::brick("./data/processed/gridded/co_variables/ERA5land_tmin_1981_2019.nc")

gridded_points <- raster::extract(ERA5land_tmax[[1]] + 0,
                                  era_qc_data_xy[, c("LON", "LAT")],
                                  cellnumbers = TRUE) %>% .[, "cells"]

ERA5land_tmax <- ERA5land_tmax[gridded_points] %>% 
  t %>%
  round(2) %>%
  xts::xts(., seq(as.Date("1981-01-01"), as.Date("2019-12-31"), by = "day")) %>%
  setNames(era_qc_data_xy$ID)

ERA5land_tmin <- ERA5land_tmin[gridded_points] %>% 
  t %>%
  round(2) %>%
  xts::xts(., seq(as.Date("1981-01-01"), as.Date("2019-12-31"), by = "day")) %>% 
  setNames(era_qc_data_xy$ID)
  
## Bias-Correction (BC) of ERA5land time series

ERA5land_tmax <- parallel::mclapply(ERA5land_tmax, function(time_serie){

  stations_x <- gsub("ERA5_", "", colnames(time_serie))
  obs_x <- qc_data$values$tmax[, stations_x]
  
  # BC
  model_qm <- daily_varying_anom_qmap(ts_obs = obs_x,
                                      ts_model = time_serie)
  
  # simple QC (QC 02) plus gap-filling (using daily climatology) 
  
  model_qm <- extVal_check(xts_obj = cbind(model_qm, NA) %>% setNames(c("tmax", "tmin")),
                           ext_lim_factor = 3)$qc$tmax
  model_qm <- daily_climatology_filling(ts_data = model_qm)
  colnames(model_qm) <- colnames(time_serie)
  
  # is it useful?
  Rcor <- cor(zoo::coredata(cbind(model_qm, obs_x)), use = "pairwise.complete.obs")[2]
  
  if((Rcor >= 0.6) | (stations_x %in% IDs_no_Peru)){
    
    model_qm
    
  } else {
    
    NULL
    
  }
  
  }, mc.cores = n_cores) %>% 
  do.call("cbind", .)
  
ERA5land_tmin <- parallel::mclapply(ERA5land_tmin, function(time_serie){
  
  stations_x <- gsub("ERA5_", "", colnames(time_serie))
  obs_x <- qc_data$values$tmin[, stations_x]
  
  # BC
  model_qm <- daily_varying_anom_qmap(ts_obs = obs_x,
                                      ts_model = time_serie)
  
  # simple QC (QC 02) plus gap-filling (using daily climatology) 
  model_qm <- extVal_check(xts_obj = cbind(NA, model_qm) %>% setNames(c("tmax", "tmin")),
                           ext_lim_factor = 3)$qc$tmin
  model_qm <- daily_climatology_filling(ts_data = model_qm)
  colnames(model_qm) <- colnames(time_serie)
  
  # is it useful?
  Rcor <- cor(zoo::coredata(cbind(model_qm, obs_x)), use = "pairwise.complete.obs")[2]
  
  if((Rcor >= 0.6) | (stations_x %in% IDs_no_Peru)){
    
    model_qm
    
  } else {
    
    NULL
    
  }
  
  }, mc.cores = n_cores) %>%
  do.call("cbind", .)

## merging ERA5land data
stations_filter <- Reduce(intersect, list(names(ERA5land_tmax), names(ERA5land_tmin))) # 0.6 <- 200
qc_data$values$tmax <- cbind(qc_data$values$tmax, ERA5land_tmax[, stations_filter]) 
qc_data$values$tmin <- cbind(qc_data$values$tmin, ERA5land_tmin[, stations_filter])
qc_data$xyz <- rbind(qc_data$xyz, era_qc_data_xy[match(stations_filter, era_qc_data_xy$ID), ])
rownames(qc_data$xyz) <- NULL


saveRDS(object = qc_data,
        file = "./data/processed/obs/qc_output/QC_(plusERA5)_data.RDS")
