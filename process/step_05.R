rm(list = ls())

library(xts)
library(raster)
"%>%" = magrittr::`%>%`

source('./src/process/Merging/MG_make_single_point.R')
source('./src/process/Merging/MG_normal_anomaly_values.R')

# data
qc_data <- readRDS("./data/processed/obs/qc_output/QC_GF_HG_data.RDS")

# grid data
gridded_data <- raster::brick("./data/processed/gridded/co_variables/DEM.nc")[[1]]
gridded_data_a <- raster::aggregate(gridded_data, 5)

# data to sp
xyz_sp <- sp::SpatialPointsDataFrame(coords = qc_data$xyz[, c("LON", "LAT")],
                                     data = qc_data$xyz,
                                     proj4string = sp::CRS(raster::projection(gridded_data)))

# point that are in the same
points_to_be_merged <- make_single_point(pts = xyz_sp,
                                         rgrid = gridded_data_a)

# getting single points 
new_xyz <- points_to_be_merged$xyz

# points to be merged
id_to_merge <- points_to_be_merged$no_single_points

# single ts
new_value_tmax <- qc_data$value$tmax[, -match(unlist(id_to_merge), colnames(qc_data$value$tmax))]
new_value_tmin <- qc_data$value$tmin[, -match(unlist(id_to_merge), colnames(qc_data$value$tmin))]

# merging no-single ts
id_to_merge_tmax <- id_to_merge %>%
  setNames(id_to_merge %>% lapply(function(x) paste(x, collapse = "_"))) %>%
  lapply(function(x){
    qc_data$value$tmax[, x] %>% apply(1, mean, na.rm = TRUE) %>% round(2)
  }) %>%
  do.call("cbind", .)

id_to_merge_tmax <- xts::xts(id_to_merge_tmax, time(qc_data$value$tmax))


id_to_merge_tmin <- id_to_merge %>%
  setNames(id_to_merge %>% lapply(function(x) paste(x, collapse = "_"))) %>%
  lapply(function(x){
    qc_data$value$tmin[, x] %>% apply(1, mean, na.rm = TRUE) %>% round(2)
  }) %>%
  do.call("cbind", .)

id_to_merge_tmin <- xts::xts(id_to_merge_tmin, time(qc_data$value$tmin))

# single + new single ts
new_value_tmax <- cbind(new_value_tmax, id_to_merge_tmax)
new_value_tmin <- cbind(new_value_tmin, id_to_merge_tmin)

# are ID same?
all(colnames(new_value_tmax) == new_xyz@data$ID)
all(colnames(new_value_tmin) == new_xyz@data$ID)

# getting normal/daily anomalies

#tmax
normal_tmax <- lapply(new_value_tmax, function(x) get_monthly_normals(daily_time_serie = x)) %>%
  do.call("cbind", .)

# anomaly_tmax <- lapply(new_value_tmax, function(x) get_anomaly_values(daily_time_serie = x)) %>%
#   do.call("cbind", .)

#tmin
normal_tmin <- lapply(new_value_tmin, function(x) get_monthly_normals(daily_time_serie = x)) %>%
  do.call("cbind", .)

# anomaly_tmin <- lapply(new_value_tmin, function(x) get_anomaly_values(daily_time_serie = x)) %>%
#   do.call("cbind", .)

# save data

saveRDS(object = list(values = list(tmax = normal_tmax,
                                    tmin = normal_tmin),
                      xyz = new_xyz),
        file = "./data/processed/obs/qc_output/Normals_OBS.RDS")

saveRDS(object = list(values = list(tmax = new_value_tmax,
                                    tmin = new_value_tmin),
                      xyz = new_xyz),
        file = "./data/processed/obs/qc_output/OBS.RDS")
