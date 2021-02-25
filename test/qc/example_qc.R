rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

source('./src/process/QC/QC_rclimdex_cleaning.R')
source('./src/process/QC/QC_extreme_value_check.R')
source('./src/process/QC/QC_int_consistency_check_temp.R')
source('./src/process/QC/QC_temporal_coherence_check.R')
source('./src/process/QC/QC_spatial_neighbors.R')
source('./src/process/QC/QC_spatial_coherence_check.R')
source('./src/process/QC/QC_precision_and_variability_check.R')
source('./src/process/QC/QC_apply_visual_qc.R')

# example data
xyz <- read.table("./test/qc/xyz_example.txt", header = TRUE, stringsAsFactors = FALSE)
ID_example <- xyz$ID[1]
print(ID_example)
exp_data_rclimdex <- read.table(gsub("X", ID_example, "./test/qc/data_exp/X.txt"), sep = " ")

# QC 00: inspection of lat, lon, elevation values
# only applied in xyz

# QC 01: transforming rclimdex file to xts obj (plus basic qc)
qc01 <- rclimdex_2_xts(data_frame = exp_data_rclimdex)
qc01$non_qc

# QC 02: extreme values check
qc02 <- extVal_check(xts_obj = qc01$qc)
qc02$non_qc

# QC 03: inconsistency check
qc03 <- inCons_check(xts_obj = qc02$qc)
qc03$non_qc

# QC 04: temporal coherence check
qc04 <- temCoh_check(xts_obj = qc03$qc)
qc04$non_qc

# QC 05: spatial coherence check
# building list of xts objs
# i) all the xts time series should be here, and 
# at this stage they should have passed the previous QCs

list_of_xts_obj <- 
  dir("./test/qc/data_exp", full.names = TRUE) %>%
  setNames(xyz$ID) %>%
  lapply(function(z){
    
    # only QC1 is performed (just for example)
    exp_data_rclimdex <- read.table(z, sep = " ")
    rclimdex_2_xts(data_frame = exp_data_rclimdex)
    
  })

# ii) as the example ID (target) has passed for QC, it can be replaced
list_of_xts_obj <- lapply(list_of_xts_obj, `[[`, "qc")
list_of_xts_obj[[ID_example]] <- qc04$qc

qc05 <- spt_neighrs(id_station = ID_example,           # i) seek of neighbouring stations
                    stations_database = xyz) %>%
  sptCohrc_check(nghrs_stations = .,                   # ii) applying qc
                 xts_list_database = list_of_xts_obj)  
qc05$non_qc


# QC 06: visual inspection
# the idea in this stage is to delete/modify as much as is possible the time series,
# after that you should apply the "correction", following main ideas (see function details)

# first visual inspection
plot_title = paste(ID_example, "-", xyz$NAM[match(ID_example, xyz$ID)], sep = "")

qc06 <- get_pRcs_temp(xts_obj = list_of_xts_obj[[ ID_example ]]) %>% # computing distribution of decimal values (precision)
  enhanced_qc_plot(get_pRcs_temp_output = ., title_plt = plot_title) # plotting time series plus precision

qc06

# after visualization
# list of visual inspection
list_of_visual_qc <- list(PE100120 = c("tmin['2005/2006'] <- NA", # just an example
                                       "tmax['2010'] <- NA"))
# applying visual qc
qc06_after_visual_qc <- apply_visual_qc(data_list = list_of_xts_obj,
                                        list_of_qc_expr = list_of_visual_qc)$qc
# second visual inspection
qc06 <- get_pRcs_temp(xts_obj = qc06_after_visual_qc[[ ID_example ]]) %>% # computing distribution of decimal values (precision)
  enhanced_qc_plot(get_pRcs_temp_output = ., title_plt = plot_title) # plotting time series plus precision

qc06
