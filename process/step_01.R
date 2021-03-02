# library(xts)
"%>%" = magrittr::`%>%`
list.files("./src/process/QC", full.names = TRUE) %>%
  sapply(source) %>% 
  invisible()

# output 
enhanced_qc_path = "./data/processed/obs/enhanced_qc/plots"
non_qc_out_path = "./data/processed/obs/non_qc_output"
qc_out_path = "./data/processed/obs/qc_output"

# uploading raw observed data
raw_obs_dir = "./data/raw/obs/data"
raw_xy_file = read.table("./data/raw/obs/xyz.txt",
                         sep = " ", 
                         header = TRUE, 
                         stringsAsFactors = FALSE)


# same order: file names and xyz data?
identical(dir(raw_obs_dir) %>% gsub(".txt", "", .), raw_xy_file$ID)


# QC 00

dem_data <- raster::raster("./data/raw/gridded/ELV/ELV.tif")

Zobs_2_Zdem(rxyz = raw_xy_file,
            z_dem = dem_data,
            Diff = 200) -> QC_data

# It is replaced all the stations where more that 200m is found. Although some differences are acceptable
# I manually checked where at least 1000m is found. From this, a total of six (6) stations were checked
# Two (2) of them have not ALT information from EXPLORADOR_CLIMATICO, Four (4) from SENAMHI had bad information in ALT
# But only one (1), from SENAMHI, had bad coordinates LON, and LAT. This was compared from the DECADE dataset, and replaced

saveRDS(object = QC_data[["non_qc"]],
        file = file.path(non_qc_out_path, "non_QC00.RDS"))

raw_xy_file <- QC_data[["qc"]]


# QC 01

dir(raw_obs_dir, full.names = TRUE) %>%
  setNames(raw_xy_file$ID) %>%
  parallel::mclapply(function(x){
  
    rclimdex_2_xts(data_frame = read.table(x, sep = " "))
     
  }, mc.cores = 6) -> QC_data


saveRDS(object = lapply(QC_data, `[[`, "non_qc") %>% .[lapply(., nrow) != 0],
        file = file.path(non_qc_out_path, "non_QC01.RDS"))

saveRDS(object = list(values = lapply(QC_data, `[[`, "qc"), xyz = raw_xy_file),
        file = file.path(qc_out_path, "RAW(QC01)_data.RDS"))

QC_data <- lapply(QC_data, `[[`, "qc")


# QC 02
QC_data %>%
  parallel::mclapply(function(x){
    
    extVal_check(xts_obj = x)
    
  }, mc.cores = 6) -> QC_data

saveRDS(object = lapply(QC_data, `[[`, "non_qc") %>% .[lapply(., nrow) != 0],
        file = file.path(non_qc_out_path, "non_QC02.RDS"))

QC_data <- lapply(QC_data, `[[`, "qc")


# QC 03

QC_data %>%
  parallel::mclapply(function(x){
    
    inCons_check(xts_obj = x)
    
    }, mc.cores = 6) -> QC_data

saveRDS(object = lapply(QC_data, `[[`, "non_qc") %>% .[lapply(., nrow) != 0],
        file = file.path(non_qc_out_path, "non_QC03.RDS"))

QC_data <- lapply(QC_data, `[[`, "qc")


# QC 04

QC_data %>%
  parallel::mclapply(function(x){
    
    temCoh_check(xts_obj = x)
    
  }, mc.cores = 6) -> QC_data

saveRDS(object = lapply(QC_data, `[[`, "non_qc") %>% .[lapply(., nrow) != 0],
        file = file.path(non_qc_out_path, "non_QC04.RDS"))

QC_data <- lapply(QC_data, `[[`, "qc")


# QC 05

raw_xy_file$ID %>% 
  setNames(., raw_xy_file$ID) %>%
  lapply(function(x){
      spt_neighrs(id_station = x,
                  stations_database = raw_xy_file)
    })  %>%
  parallel::mclapply(function(z){
    
    sptCohrc_check(nghrs_stations = z,
                   xts_list_database = QC_data)
    
  }, mc.cores = 6) -> QC_data


saveRDS(object = lapply(QC_data, `[[`, "non_qc") %>% .[lapply(., nrow) != 0],
        file = file.path(non_qc_out_path, "non_QC05.RDS"))

QC_data <- lapply(QC_data, `[[`, "qc")

# QC 06

# creating enhanced qc plots
# raw_xy_file$ID %>%
#   lapply(function(x){
# 
#     ID_station <- x %>% as.character()
#     plot_title = paste(ID_station, "-", raw_xy_file$NAM[match(x, raw_xy_file$ID)], sep = "")
# 
#     get_pRcs_temp(xts_obj = QC_data[[ ID_station ]]) %>%
#       enhanced_qc_plot(get_pRcs_temp_output = .,
#                        title_plt = plot_title) %>%
#       ggplot2::ggsave(filename = file.path(enhanced_qc_path, paste(plot_title, ".jpg", sep = "")),
#                       plot = .,
#                       width = 20, height = 7,
#                       dpi = 150)
# 
#   })

# order to be applied: visual inspection 
visual_enhanced_qc <- source("./data/processed/obs/enhanced_qc/visual_enhanced_qc.R")$value

QC_data <- apply_visual_qc(data_list = QC_data,
                           list_of_qc_expr = visual_enhanced_qc)

saveRDS(object = QC_data[["non_qc"]] %>% .[sapply(., nrow) %>% unlist() != 0],
        file = file.path(non_qc_out_path, "non_QC06.RDS"))

# are changes ok?
# raw_xy_file$ID %>%
#   lapply(function(x){
# 
#     ID_station <- x %>% as.character()
#     plot_title = paste(ID_station, "-", raw_xy_file$NAM[match(x, raw_xy_file$ID)], sep = "")
#     get_pRcs_temp(xts_obj = QC_data[["qc"]][[ ID_station ]]) %>%
#       enhanced_qc_plot(get_pRcs_temp_output = .,
#                        title_plt = plot_title) %>%
#       ggplot2::ggsave(filename = file.path(enhanced_qc_path, paste(plot_title, "_2.jpg", sep = "")),
#                       plot = .,
#                       width = 20, height = 7,
#                       dpi = 150)
# 
#   })

QC_data <- QC_data[["qc"]]

saveRDS(object = list(values = QC_data, xyz = raw_xy_file),
        file = file.path(qc_out_path, "RAW(QC06)_data.RDS"))


# Filter

# Date range PISCOt: i) 1981-2019
#                    ii) 5 years with 365 days (except 02-29)
# Date range for reanalysis bias-correction: i) 1981-2019 
#                                            ii) 5 years with 365 days (except 02-29)
#                                            iii) minimum length size of 10 years of data
#                                            iv) month: 20 days, year: 12 months
# Date range for cross-validation analysis: i) 1981-2019 
#                                           ii) 5 years with 365 days (except 02-29)
#                                           iii) minimum length size of 75% of data
#                                           iv) month: 20 days, year: 12 months

filter_stations <- get_stations_from_filter(data_list = QC_data,
                                            length_of_years = 10)

filter_stations_more_data <- get_stations_from_filter(data_list = QC_data,
                                                      length_of_years = round((2019-1981+1)*75/100, 0))

all(filter_stations_more_data %in% filter_stations) # it should be TRUE (to avoid new columns in xyz)

raw_xy_file <- transform(raw_xy_file, 
                         filter_qc = ifelse(ID %in% filter_stations, 1, 0),
                         filter_qc70 = ifelse(ID %in% filter_stations_more_data, 1, 0))

saveRDS(object = list(values = list(tmax = lapply(QC_data, function(x) x[, "tmax"]) %>%
                                      do.call(cbind, .) %>%
                                      setNames(raw_xy_file$ID) %>%
                                      .["1981/2019"],
                                    tmin = lapply(QC_data, function(x) x[, "tmin"]) %>%
                                      do.call(cbind, .) %>%
                                      setNames(raw_xy_file$ID) %>%
                                      .["1981/2019"]),
                      xyz = raw_xy_file),
        file = file.path(qc_out_path, "QC_data.RDS"))

