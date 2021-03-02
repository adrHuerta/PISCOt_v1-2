rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

source("./src/process/GapFilling/GF_std_dep_imputation.R")
source('./src/process/GapFilling/GF_daily_climatology_filling.R')

# best case
exp_data <- readRDS("./test/gap_filling/exp_data_bestcase.RDS")
lattice::xyplot(exp_data)
lattice::xyplot(std_dep_imputation(stat_data = exp_data))

# worst case 0
exp_data <- readRDS("./test/gap_filling/exp_data_worstcase.RDS")
lattice::xyplot(exp_data)
lattice::xyplot(std_dep_imputation(stat_data = exp_data))

# worst case 1
exp_data <- readRDS("./test/gap_filling/exp_data_worstcase.RDS")[, 1]
lattice::xyplot(exp_data)
lattice::xyplot(std_dep_imputation(stat_data = exp_data))


# using daily climatology
# only used if there are < 1% of NA data, here just an example

lattice::xyplot(daily_climatology_filling(ts_data = exp_data) %>%
                  window(start = "2015-01-01",
                         end = "2018-12-31"))

lattice::xyplot(daily_climatology_filling(ts_data = exp_data) %>%
                  window(start = "1980-01-01",
                         end = "1982-12-31"))


########### if u have the dataset ###########
# rm(list = ls())
# 
# library(xts)
# "%>%" = magrittr::`%>%`
# 
# source("./src/process/QC/QC_spatial_neighbors.R")
# source("./src/process/GapFilling/GF_build_neigh_matrix.R")
# source("./src/process/GapFilling/GF_std_dep_imputation.R")
# 
# # data
# qc_data <- readRDS("./data/processed/obs/qc_output/QC_(plusERA5)_data.RDS")
# 
# # basic case: there are available neighbours stations to fill target
# spt_neighrs(id_station = qc_data$xyz$ID[50],
#             stations_database = qc_data$xyz,
#             lmt_n = 10) %>%
#   build_matrix(id_stations = .,
#                time_series_database = qc_data$values$tmax) %>%
#   std_dep_imputation(stat_data = .) %>%
#   lattice::xyplot()
# 
# 
# # worst case: there are not available neighbours stations to fill target
# spt_neighrs(id_station = qc_data$xyz$ID[4],
#             stations_database = qc_data$xyz,
#             lmt_n = 1) %>% # as example
#   build_matrix(id_stations = .,
#                time_series_database = qc_data$values$tmax) %>%
#   std_dep_imputation(stat_data = .) %>%
#   lattice::xyplot()
# 
# spt_neighrs(id_station = qc_data$xyz$ID[4],
#             stations_database = qc_data$xyz,
#             lmt_n = 1) %>% .[1] %>% # as example
#   build_matrix(id_stations = .,
#                time_series_database = qc_data$values$tmax) %>%
#   std_dep_imputation(stat_data = .) %>%
#   lattice::xyplot()
# 
# #lmt_dist/lmt_elv 70/500, 70/1000, 100/1000