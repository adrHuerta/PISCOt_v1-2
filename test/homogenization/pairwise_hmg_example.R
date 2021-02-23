rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

source('./src/process/Homogenization/HG_pairwise_snht.R')
source('./src/process/Homogenization/HG_hmgFactor2daily.R')

exp_data_daily_target <- readRDS("./test/homogenization/exp_data_daily_target.RDS")
exp_data_monthly <- readRDS("./test/homogenization/exp_data_monthly.RDS")

# target daily vs monthly
lattice::xyplot(exp_data_daily_target)
lattice::xyplot(exp_data_monthly[, 1])

# applying pha (at monthly scale)
pha_res_example <- pha_hmg(ts_data = exp_data_monthly)
# breaks 
pha_res_example$breaks
# raw vs hmg
lattice::xyplot(cbind(pha_res_example$original, pha_res_example$hmg))
# applied shift
lattice::xyplot(pha_res_example$original - pha_res_example$hmg)

# making pha factor to daily scale
# i) each monthly value is set to the 15th day of each month
# ii) the NA days are estimated based on a 1d linear interpolation
# iii) to avoid abrupt changes in the limits of step change

hmgFactor2daily(monthly_ts = pha_res_example$original,
                monthly_ts_hmg = pha_res_example$hmg,
                daily_ts = exp_data_daily_target) -> dailyCorrected

# raw vs hmg (with daily factor) daily
lattice::xyplot(cbind(exp_data_daily_target, dailyCorrected))
# applied shift (it should be similar to the monthly scale)
lattice::xyplot(exp_data_daily_target -  dailyCorrected)
# what about at monthly scale (it should be the same as well)
lattice::xyplot(xts::apply.monthly(exp_data_daily_target, mean) -
                  xts::apply.monthly(dailyCorrected, mean))





########### if u have the dataset ###########
# rm(list = ls())
# 
# library(xts)
# "%>%" = magrittr::`%>%`
# 
# source("./src/process/QC/QC_spatial_neighbors.R")
# source("./src/process/GapFilling/GF_build_neigh_matrix.R")
# source('./src/process/Homogenization/HG_pairwise_snht.R')
# source('./src/process/Homogenization/HG_hmgFactor2daily.R')
# 
# # data
# qc_data <- readRDS("./data/processed/obs/QC_GF_data.RDS")
# 
# # make monthly values
# monthly_tmax <- do.call("cbind", lapply(qc_data$values$tmax, function(x) round(xts::apply.monthly(x, mean), 2)))
# monthly_tmin <- do.call("cbind", lapply(qc_data$values$tmin, function(x) round(xts::apply.monthly(x, mean), 2)))
# 
# ### pha
# # getting neighbouring time_series from target
# spt_neighrs(id_station = qc_data$xyz$ID[9],
#             stations_database = qc_data$xyz, lmt_n = 8, lmt_dist = 100) %>%
#   build_matrix(id_stations = .,
#                time_series_database = monthly_tmax) -> ID_plut_n_stations 
# 
# # applying pha (at monthly scale)
# pha_res_example <- pha_hmg(ts_data = ID_plut_n_stations)
# 
# # breaks 
# pha_res_example$breaks
# # raw vs hmg
# lattice::xyplot(cbind(pha_res_example$original, pha_res_example$hmg))
# # applied shift
# lattice::xyplot(pha_res_example$original - pha_res_example$hmg)
# 
# # making pha factor to daily scale
# # i) each monthly value is set to the 15th day of each month
# # ii) the NA days are estimated based on a 1d linear interpolation
# # iii) to avoid abrupt changes in the limits of step change
# 
# hmgFactor2daily(monthly_ts = pha_res_example$original,
#                 monthly_ts_hmg = pha_res_example$hmg,
#                 daily_ts = qc_data$values$tmax[, qc_data$xyz$ID[9]]) -> dailyCorrected
# 
# # raw vs hmg (with daily factor) daily
# lattice::xyplot(cbind(qc_data$values$tmax[, qc_data$xyz$ID[9]], dailyCorrected))
# # applied shift (it should be similar to the monthly scale)
# lattice::xyplot(qc_data$values$tmax[, qc_data$xyz$ID[9]] -  dailyCorrected)
# # what about at monthly scale (it should be the same as well)
# lattice::xyplot(xts::apply.monthly(qc_data$values$tmax[, qc_data$xyz$ID[9]], mean) -  
#                   xts::apply.monthly(dailyCorrected, mean))