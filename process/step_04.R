rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

# n cores
n_cores = 10

source("./src/process/QC/QC_spatial_neighbors.R")
source('./src/process/QC/QC_int_consistency_check_temp.R')
source("./src/process/GapFilling/GF_build_neigh_matrix.R")
source("./src/process/GapFilling/GF_daily_climatology_filling.R")
source('./src/process/Homogenization/HG_pairwise_snht.R')
source('./src/process/Homogenization/HG_simple_snht.R')
source('./src/process/Homogenization/HG_hmgFactor2daily.R')

# data
qc_data <- readRDS("./data/processed/obs/qc_output/QC_GF_data.RDS")

# make monthly values
monthly_tmax <- do.call("cbind", lapply(qc_data$values$tmax, function(x) round(xts::apply.monthly(x, mean), 2)))
monthly_tmin <- do.call("cbind", lapply(qc_data$values$tmin, function(x) round(xts::apply.monthly(x, mean), 2)))

# data to be filled
qc_monthly_values_tmax_ERA5_hmg <- monthly_tmax
qc_monthly_values_tmin_ERA5_hmg <- monthly_tmin

# three spatial steps (just to make the loop)
param_spt <- list(list(lmt_dist = 1000, 
                       lmt_elv = 1000,
                       lmt_n = 8),
                  list(lmt_dist = 1000, 
                       lmt_elv = 1000,
                       lmt_n = 8),
                  list(lmt_dist = 1000, 
                       lmt_elv = 1000,
                       lmt_n = 8))

# monthly hmg 
for(xi in seq_along(param_spt)){
  
  # tmax
  parallel::mclapply(qc_data$xyz$ID,
                   function(station_j){
                     
                     ID_stat_s  <- spt_neighrs(id_station = station_j,
                                              stations_database = qc_data$xyz,
                                              lmt_dist = param_spt[[xi]]$lmt_dist,
                                              lmt_elv = param_spt[[xi]]$lmt_elv,
                                              lmt_n = param_spt[[xi]]$lmt_n) %>%
                      build_matrix(id_stations = ., 
                                   time_series_database = qc_monthly_values_tmax_ERA5_hmg)
                    
                    if(ncol(ID_stat_s) >= 4){
                      
                      response <- pha_hmg(ts_data = ID_stat_s)$hmg
                      
                    } else {
                      
                      response <- snht_hmg(ts_data = ID_stat_s[, 1])$hmg
                      
                    }
                    
                     response
                     
                     }, mc.cores = n_cores) -> qc_monthly_values_tmax_ERA5_hmg
  
  qc_monthly_values_tmax_ERA5_hmg <- do.call("cbind", qc_monthly_values_tmax_ERA5_hmg)
  colnames(qc_monthly_values_tmax_ERA5_hmg) <- qc_data$xyz$ID
  
  # tmin
  parallel::mclapply(qc_data$xyz$ID,
                     function(station_j){

                       ID_stat_s  <- spt_neighrs(id_station = station_j,
                                                 stations_database = qc_data$xyz,
                                                 lmt_dist = param_spt[[xi]]$lmt_dist,
                                                 lmt_elv = param_spt[[xi]]$lmt_elv,
                                                 lmt_n = param_spt[[xi]]$lmt_n) %>%
                         build_matrix(id_stations = .,
                                      time_series_database = qc_monthly_values_tmin_ERA5_hmg)

                       if(ncol(ID_stat_s) >= 4){

                         response <- pha_hmg(ts_data = ID_stat_s)$hmg

                       } else {

                         response <- snht_hmg(ts_data = ID_stat_s[, 1])$hmg

                       }

                       response
                       
                       }, mc.cores = n_cores) -> qc_monthly_values_tmin_ERA5_hmg

  qc_monthly_values_tmin_ERA5_hmg <- do.call("cbind", qc_monthly_values_tmin_ERA5_hmg)
  colnames(qc_monthly_values_tmin_ERA5_hmg) <- qc_data$xyz$ID
  

}


# daily correction

qc_daily_values_tmax_ERA5_hmg <- qc_data$values$tmax
qc_daily_values_tmin_ERA5_hmg <- qc_data$values$tmin

for(station_j in qc_data$xyz$ID){
  # tmax
  qc_daily_values_tmax_ERA5_hmg[, station_j] <- hmgFactor2daily(monthly_ts = monthly_tmax[, station_j],
                                                                monthly_ts_hmg = qc_monthly_values_tmax_ERA5_hmg[, station_j],
                                                                daily_ts = qc_data$values$tmax[, station_j])
  
  # tmin
  qc_daily_values_tmin_ERA5_hmg[, station_j] <- hmgFactor2daily(monthly_ts = monthly_tmin[, station_j],
                                                                monthly_ts_hmg = qc_monthly_values_tmin_ERA5_hmg[, station_j],
                                                                daily_ts = qc_data$values$tmin[, station_j])
}


# simple qc:
# i) tmax < tmin, 

for(station_j in qc_data$xyz$ID){
  
  ts_after_qc <- inCons_check(xts_obj = cbind(qc_daily_values_tmax_ERA5_hmg[, station_j],
                                              qc_daily_values_tmin_ERA5_hmg[, station_j]) %>% 
                                setNames(c("tmax", "tmin")))
  
  if(nrow(ts_after_qc$non_qc) != 0) {
  
    qc_daily_values_tmax_ERA5_hmg[, station_j] <- daily_climatology_filling(ts_data = ts_after_qc$qc$tmax)
    qc_daily_values_tmin_ERA5_hmg[, station_j] <- daily_climatology_filling(ts_data = ts_after_qc$qc$tmin)
    }
  }


all(qc_daily_values_tmax_ERA5_hmg > qc_daily_values_tmin_ERA5_hmg)

## saving
qc_data$values$tmax <- qc_daily_values_tmax_ERA5_hmg
qc_data$values$tmin <- qc_daily_values_tmin_ERA5_hmg
qc_data$xyz <- qc_data$xyz[match(colnames(qc_daily_values_tmax_ERA5_hmg), qc_data$xyz$ID), ]
rownames(qc_data$xyz) <- NULL

source('./src/process/QC/QC_precision_and_variability_check.R')

qc_data$xyz$ID %>%
  lapply(function(x){

    ID_station <- x %>% as.character()
    plot_title = paste(ID_station, "-", qc_data$xyz$NAM[match(x, qc_data$xyz$ID)], sep = "")
    get_pRcs_temp(xts_obj = cbind(qc_data$values$tmax[, ID_station], qc_data$values$tmin[, ID_station]) %>%
                    setNames(c("tmax", "tmin"))) %>%
      enhanced_qc_plot(get_pRcs_temp_output = .,
                       title_plt = plot_title) %>%
      ggplot2::ggsave(filename = file.path("./data/processed/obs/enhanced_qc/plots", paste(plot_title, "_4.jpg", sep = "")),
                      plot = .,
                      width = 20, height = 7,
                      dpi = 150)

  })

# time series were the method did not worked very well (visual inspection)
to_del <- c("PE103038", "PE103042", "PE105054", 
            "PE106109", "PE105076", "PE104090",
            "PE100136", "PE100081", "PE116052")

qc_data$values$tmax <- qc_data$values$tmax[, -match(to_del, colnames(qc_data$values$tmax))]
qc_data$values$tmin <- qc_data$values$tmin[, -match(to_del, colnames(qc_data$values$tmin))]
qc_data$xyz <- qc_data$xyz[-match(to_del, qc_data$xyz$ID), ]
rownames(qc_data$xyz) <- NULL

saveRDS(object = qc_data,
        file = "./data/processed/obs/qc_output/QC_GF_HG_data.RDS")

