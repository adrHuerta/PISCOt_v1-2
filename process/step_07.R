rm(list = ls())

library(xts)
library(raster)
"%>%" = magrittr::`%>%`

OBS_data <- readRDS("./data/processed/obs/qc_output/OBS.RDS")

# normals values based on grid
# to aproxximate the obs value as much as possible

tmax_normals <- file.path("./data/processed/gridded/Normals", 
                          sprintf("%s/tmax_%02d.nc", "tmax",  1:12)) %>%
  lapply(function(x) raster::raster(x)) %>%
  raster::brick() %>%
  raster::extract(x = ., y = OBS_data$xyz) %>%
  t()

tmin_normals <- file.path("./data/processed/gridded/Normals", 
                          sprintf("%s/tmin_%02d.nc", "tmin",  1:12)) %>%
  lapply(function(x) raster::raster(x)) %>%
  raster::brick() %>%
  raster::extract(x = ., y = OBS_data$xyz) %>%
  t()


# to anomaly values
#tmax

anomaly_tmax <- lapply(seq_len(ncol(OBS_data$values$tmax)),
                       function(x){
                         get_anomaly_values2(daily_time_serie = OBS_data$values$tmax[, x],
                                             monthly_values = tmax_normals[, x])
                       }) %>% 
  do.call("cbind", .)

#tmin

anomaly_tmin <- lapply(seq_len(ncol(OBS_data$values$tmin)),
                       function(x){
                         get_anomaly_values2(daily_time_serie = OBS_data$values$tmin[, x],
                                             monthly_values = tmin_normals[, x])
                       }) %>% 
  do.call("cbind", .)

# save data
saveRDS(object = list(values = list(tmax = anomaly_tmax,
                                    tmin = anomaly_tmin),
                      xyz = OBS_data$xyz),
        file = "./data/processed/obs/qc_output/Anomalies_OBS.RDS")
