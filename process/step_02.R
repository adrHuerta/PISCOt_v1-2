rm(list = ls())
"%>%" = magrittr::`%>%`

source("./src/process/GapFilling/GF_dreqm.R")
source('./src/process/QC/QC_extreme_value_check.R')
source('./src/process/GapFilling/GF_daily_climatology_filling.R')

#
qc_data <- readRDS("./data/processed/obs/QC_data.RDS")
era_qc_data_xy <- qc_data$xyz[qc_data$xyz$filter_qc == 1, ]
era_qc_data_xy$ID <- paste("ERA5", era_qc_data_xy$ID, sep = "_")
era_qc_data_xy$SRC <- "ERA5"

IDs_no_Peru <- qc_data$xyz[(qc_data$xyz$filter_qc == 1) & (qc_data$xyz$SRC != "SENAMHI"), ]$ID

# getting ERA5 time series from XY points

ERA5_tmax <- raster::brick("./data/processed/gridded/ERA5_t2m_tmax_1960_2019.nc")
ERA5_tmin <- raster::brick("./data/processed/gridded/ERA5_t2m_tmin_1960_2019.nc")

gridded_points <- raster::extract(ERA5_tmax[[1]] + 0,
                                  era_qc_data_xy[, c("LON", "LAT")],
                                  cellnumbers = TRUE) %>% .[, "cells"]

ERA5_tmax <- ERA5_tmax[gridded_points] %>% 
  t %>%
  round(2) %>%
  xts::xts(., seq(as.Date("1960-01-01"), as.Date("2019-12-31"), by = "day")) %>%
  .['1981/2019'] %>%
  setNames(era_qc_data_xy$ID)

ERA5_tmin <- ERA5_tmin[gridded_points] %>% 
  t %>%
  round(2) %>%
  xts::xts(., seq(as.Date("1960-01-01"), as.Date("2019-12-31"), by = "day")) %>% 
  .['1981/2019'] %>%
  setNames(era_qc_data_xy$ID)
  
## Bias-Correction (BC) of ERA5 time series

ERA5_tmax <- parallel::mclapply(ERA5_tmax, function(time_serie){

  stations_x <- gsub("ERA5_", "", colnames(time_serie))
  obs_x <- qc_data$values$tmax[, stations_x]
  
  # BC
  model_qm <- daily_varying_anom_qmap(ts_obs = obs_x,
                                      ts_model = time_serie)
  
  # simple QC (QC 02) plus gap-filling (using daily climatology) 
  
  model_qm <- extVal_check(xts_obj = cbind(model_qm, NA) %>% setNames(c("tmax", "tmin")))$qc$tmax
  model_qm <- daily_climatology_filling(ts_data = model_qm)
  colnames(model_qm) <- colnames(time_serie)
  
  # is it useful?
  Rcor <- cor(zoo::coredata(cbind(model_qm, obs_x)), use = "pairwise.complete.obs")[2]
  
  if((Rcor >= 0.6) | (stations_x %in% IDs_no_Peru)){
    
    model_qm
    
  } else {
    
    NULL
    
  }
  
  }, mc.cores = 5) %>% 
  do.call("cbind", .)
  
ERA5_tmin <- parallel::mclapply(ERA5_tmin, function(time_serie){
  
  stations_x <- gsub("ERA5_", "", colnames(time_serie))
  obs_x <- qc_data$values$tmin[, stations_x]
  
  # BC
  model_qm <- daily_varying_anom_qmap(ts_obs = obs_x,
                                      ts_model = time_serie)
  
  # simple QC (QC 02) plus gap-filling (using daily climatology) 
  model_qm <- extVal_check(xts_obj = cbind(NA, model_qm) %>% setNames(c("tmax", "tmin")))$qc$tmin
  model_qm <- daily_climatology_filling(ts_data = model_qm)
  colnames(model_qm) <- colnames(time_serie)
  
  # is it useful?
  Rcor <- cor(zoo::coredata(cbind(model_qm, obs_x)), use = "pairwise.complete.obs")[2]
  
  if((Rcor >= 0.6) | (stations_x %in% IDs_no_Peru)){
    
    model_qm
    
  } else {
    
    NULL
    
  }
  
}, mc.cores = 5) %>% 
  do.call("cbind", .)

## merging ERA5 data
stations_filter <- Reduce(intersect, list(names(ERA5_tmax), names(ERA5_tmin))) # 0.6 <- 188/ 0.7 <- 130
qc_data$values$tmax <- cbind(qc_data$values$tmax, ERA5_tmax[, stations_filter]) 
qc_data$values$tmin <- cbind(qc_data$values$tmin, ERA5_tmin[, stations_filter])
qc_data$xyz <- rbind(qc_data$xyz, era_qc_data_xy[match(stations_filter, era_qc_data_xy$ID), ])
rownames(qc_data$xyz) <- NULL


saveRDS(object = qc_data,
        file = "./data/processed/obs/QC_(plusERA5)_data.RDS")
