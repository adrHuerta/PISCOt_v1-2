rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

source("./src/process/GapFilling/GF_dreqm.R")

#
exp_data <- readRDS("./test/reanalysis_correction/exp_data.RDS")

# time series
lattice::xyplot(exp_data)

# correction:
# detrended non-parametric quantile mapping using robust empirical quantiles
ERA5_c <- daily_varying_anom_qmap(ts_obs = exp_data$OBS,
                                       ts_model = exp_data$ERA5)

# the idea is to get a new station
# but sometimes is not possible, r > .6 to be used as new station
lattice::xyplot(cbind(exp_data$OBS, exp_data$ERA5, ERA5_c))
psych::pairs.panels(zoo::coredata(cbind(exp_data$OBS, exp_data$ERA5, ERA5_c)))






########### if u have the dataset ###########
# rm(list = ls())
# 
# library(xts)
# "%>%" = magrittr::`%>%`
# 
# source("./src/process/GapFilling/GF_dreqm.R")
# 
# # obs_data
# qc_data <- readRDS("./data/processed/obs/QC_data.RDS")
# era_qc_data_xy <- qc_data$xyz[qc_data$xyz$filter_qc == 1, ]
# era_qc_data_xy <- era_qc_data_xy[60, ]
# OBS_tmax <- qc_data$values$tmax[, era_qc_data_xy$ID]
# 
# # reanalysis_data
# ERA5_tmax <- raster::brick("./data/processed/gridded/ERA5_t2m_tmax_1960_2019.nc")
# gridded_points <- raster::extract(ERA5_tmax[[1]] + 0,
#                                   era_qc_data_xy[, c("LON", "LAT")],
#                                   cellnumbers = TRUE) %>% .[, "cells"]
# 
# ERA5_tmax <- ERA5_tmax[gridded_points] %>%
#   as.numeric() %>%
#   round(2) %>%
#   xts::xts(., seq(as.Date("1960-01-01"), as.Date("2019-12-31"), by = "day")) %>%
#   .['1981/2019'] 
# 
# # time series
# lattice::xyplot(cbind(OBS_tmax, ERA5_tmax))
# 
# # correction: 
# # detrended non-parametric quantile mapping using robust empirical quantiles
# ERA5_tmax_c <- daily_varying_anom_qmap(ts_obs = OBS_tmax,
#                                        ts_model = ERA5_tmax)
# 
# # the idea is to get a new station
# # but sometimes is not possible, r > .6 to be used a new station
# lattice::xyplot(cbind(OBS_tmax, ERA5_tmax_c, ERA5_tmax))
# psych::pairs.panels(zoo::coredata(cbind(OBS_tmax, ERA5_tmax_c, ERA5_tmax)))
