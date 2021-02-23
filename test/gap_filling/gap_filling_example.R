rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

source("./src/process/GapFilling/GF_std_dep_imputation.R")

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
# qc_data <- readRDS("./data/processed/obs/QC_(plusERA5)_data.RDS")
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
#   #std_dep_imputation(stat_data = .) %>%
#   lattice::xyplot()
# 
# 
# #lmt_dist/lmt_elv 70/500, 70/1000, 100/1000