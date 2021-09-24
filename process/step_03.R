rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

# n cores
n_cores = 10

source("./src/process/QC/QC_spatial_neighbors.R")
source("./src/process/GapFilling/GF_build_neigh_matrix.R")
source("./src/process/GapFilling/GF_std_dep_imputation_allModel.R")
source("./src/process/GapFilling/GF_daily_climatology_filling.R")
source("./src/process/GapFilling/GF_dreqm.R")


qc_data <- readRDS("./data/processed/obs/qc_output/QC_(plusERA5)_data.RDS")

# xyz data to be filled
obs_xyz <- qc_data$xyz[qc_data$xyz$SRC != "ERA5" ,]
obs_xyz_to_be_used_plusERA5 <- qc_data$xyz # stations + ERA5 (total stations with filter and no filter + ERA5 from filter)


#### gap-filling (1) ####

# data to be filled
qc_data_values_tmax_ERA5_filled <- qc_data$values$tmax
qc_data_values_tmin_ERA5_filled <- qc_data$values$tmin

# three spatial steps: 70/500/10, 70/1000/15, 100/1000/20
param_spt <- list(list(lmt_dist = 70, 
                       lmt_elv = 500,
                       lmt_n = 8),
                  list(lmt_dist = 100, 
                       lmt_elv = 500,
                       lmt_n = 8),
                  list(lmt_dist = 150, 
                       lmt_elv = 5500,
                       lmt_n = 8))

# gap-filling
for(xi in seq_along(param_spt)){
  
  parallel::mclapply(obs_xyz$ID,
                     function(station_j){
                       spt_neighrs(id_station = station_j,
                                   stations_database = obs_xyz_to_be_used_plusERA5,
                                   lmt_dist = param_spt[[xi]]$lmt_dist,
                                   lmt_elv = param_spt[[xi]]$lmt_elv,
                                   lmt_n = param_spt[[xi]]$lmt_n) %>%
                         build_matrix(id_stations = .,
                                      time_series_database = qc_data_values_tmax_ERA5_filled) %>%
                         std_dep_imputation(stat_data = .) %>%
                         .$filled # model + available data
                     }, mc.cores = n_cores) %>%
    do.call("cbind", .) %>%
    setNames(obs_xyz$ID) -> tmax_ERA5_to_be_filled
  
  qc_data_values_tmax_ERA5_filled[, colnames(tmax_ERA5_to_be_filled)] <- tmax_ERA5_to_be_filled
  
  
  parallel::mclapply(obs_xyz$ID,
                     function(station_j){
                       spt_neighrs(id_station = station_j,
                                   stations_database = obs_xyz_to_be_used_plusERA5,
                                   lmt_dist = param_spt[[xi]]$lmt_dist,
                                   lmt_elv = param_spt[[xi]]$lmt_elv,
                                   lmt_n = param_spt[[xi]]$lmt_n) %>%
                         build_matrix(id_stations = .,
                                      time_series_database = qc_data_values_tmin_ERA5_filled) %>%
                         std_dep_imputation(stat_data = .) %>%
                         .$filled # model + available data
                     }, mc.cores = n_cores) %>%
    do.call("cbind", .) %>%
    setNames(obs_xyz$ID) -> tmin_ERA5_to_be_filled
  
  qc_data_values_tmin_ERA5_filled[, colnames(tmin_ERA5_to_be_filled)] <- tmin_ERA5_to_be_filled
  
}

qc_data_tmax_ERA5_filled <- qc_data_values_tmax_ERA5_filled[, obs_xyz$ID]
qc_data_tmin_ERA5_filled <- qc_data_values_tmin_ERA5_filled[, obs_xyz$ID]


#### gap-filling (2) ####
# gap-filling using daily climatology
# (only in stations where there are at least 1% (or less) of NAs)

# tmax
for(station_j in colnames(qc_data_tmax_ERA5_filled)){
  
  sample_station_j <- qc_data_tmax_ERA5_filled[, station_j]
  size_percent_na <- sum(is.na(sample_station_j))*100/length(sample_station_j)
  
  if(size_percent_na <= 1 & size_percent_na > 0) {
    
    qc_data_tmax_ERA5_filled[, station_j] <- daily_climatology_filling(ts_data = sample_station_j)
  
    } else {
      
    next
  }
  
}
# tmin
for(station_j in colnames(qc_data_tmin_ERA5_filled)){
  
  sample_station_j <- qc_data_tmin_ERA5_filled[, station_j]
  size_percent_na <- sum(is.na(sample_station_j))*100/length(sample_station_j)
  
  if(size_percent_na <= 1 & size_percent_na > 0) {
    
    qc_data_tmin_ERA5_filled[, station_j] <- daily_climatology_filling(ts_data = sample_station_j)
    
  } else {
    
    next
  }
  
}


####
# using same stations in tmax/tmin (all data that has been filled)
stations_filter <- Reduce(intersect, 
                          list(colnames(qc_data_tmax_ERA5_filled[, colSums(is.na(qc_data_tmax_ERA5_filled)) == 0]),
                               colnames(qc_data_tmin_ERA5_filled[, colSums(is.na(qc_data_tmin_ERA5_filled)) == 0])))

## saving
qc_data$values$tmax <- qc_data_tmax_ERA5_filled[, stations_filter]
qc_data$values$tmin <- qc_data_tmin_ERA5_filled[, stations_filter]
qc_data$xyz <- obs_xyz[match(stations_filter, obs_xyz$ID), ]
rownames(qc_data$xyz) <- NULL

saveRDS(object = qc_data,
        file = "./data/processed/obs/qc_output/QC_GF_data.RDS")

source('./src/process/QC/QC_precision_and_variability_check.R')

qc_data$xyz$ID %>%
  lapply(function(x){
    
    ID_station <- x %>% as.character()
    plot_title = paste(ID_station, "-", qc_data$xyz$NAM[match(x, qc_data$xyz$ID)], sep = "")
    get_pRcs_temp(xts_obj = cbind(qc_data$values$tmax[, ID_station], qc_data$values$tmin[, ID_station]) %>%
                    setNames(c("tmax", "tmin"))) %>%
      enhanced_qc_plot(get_pRcs_temp_output = .,
                       title_plt = plot_title) %>%
      ggplot2::ggsave(filename = file.path("./data/processed/obs/enhanced_qc/plots", paste(plot_title, "_3.jpg", sep = "")),
                      plot = .,
                      width = 20, height = 7,
                      dpi = 150)
    
  })
