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


# adding artificial point in Amazon area
# 5 points
# plot(new_xyz[, "ALT"])
# IDs <- select(new_xyz, use = "pol")
# click()
point_01 <- list(LAT = -12.85401, LON = -70.5373, STATIONS = c("BO001006", "BR082915"))
point_02 <- list(LAT = -11.93921, LON = -71.18508, STATIONS = c("BO001006", "BR082807", "BR082915"))
point_03 <- list(LAT = -11.02441, LON = -72.85083, STATIONS = c("BO001006", "BR082610", "BR082704"))
point_04 <- list(LAT = -5.640112, LON = -71.9132, STATIONS = c("BR082410", "BR082610", "BR082704", "BR082807", "CO48015050"))
point_05 <- list(LAT = -4.437839, LON = -76.85839, STATIONS = c("PE100128", "PE103046", "PE103048", "PE104071", "PE105095"))

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

# making virtual stations

virtual_tmax <- lapply(list(point_01, point_02, point_03, point_04, point_05),
                       function(x){
                         round(apply(normal_tmax[, x$STATIONS], 1, mean), 2)
                       }) %>% do.call(cbind, .)
colnames(virtual_tmax) <- c("point_01", "point_02", "point_03", "point_04", "point_05")

virtual_tmin <- lapply(list(point_01, point_02, point_03, point_04, point_05),
                       function(x){
                         round(apply(normal_tmin[, x$STATIONS], 1, mean), 2)
                       }) %>% do.call(cbind, .)
colnames(virtual_tmin) <- c("point_01", "point_02", "point_03", "point_04", "point_05")


virtual_xyz <- lapply(list(point_01, point_02, point_03, point_04, point_05), function(x){
  data.frame(ID = NA, NAM = NA, LON = x$LON, LAT = x$LAT, ALT = NA, 
             SRC = "VIRTUAL", filter_qc = NA, filter_qc70 = NA)
}) %>% do.call(rbind, .)
virtual_xyz$ID = virtual_xyz$NAM = c("point_01", "point_02", "point_03", "point_04", "point_05")

# adding virtual data

normal_tmax <- cbind(normal_tmax, virtual_tmax)
normal_tmin <- cbind(normal_tmin, virtual_tmin)

new_xyz_clim <- rbind(new_xyz@data, virtual_xyz)
new_xyz_clim <- sp::SpatialPointsDataFrame(coords = new_xyz_clim[, c("LON", "LAT")],
                                           data = new_xyz_clim,
                                           proj4string = sp::CRS(sp::proj4string(xyz_sp)))

# save data

saveRDS(object = list(values = list(tmax = normal_tmax,
                                    tmin = normal_tmin),
                      xyz = new_xyz_clim),
        file = "./data/processed/obs/qc_output/Normals_OBS.RDS")

saveRDS(object = list(values = list(tmax = new_value_tmax,
                                    tmin = new_value_tmin),
                      xyz = new_xyz),
        file = "./data/processed/obs/qc_output/OBS.RDS")
