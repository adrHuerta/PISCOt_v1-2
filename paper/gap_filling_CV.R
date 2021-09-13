rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

source("./src/process/QC/QC_spatial_neighbors.R")
source("./src/process/GapFilling/GF_build_neigh_matrix.R")
source("./src/process/GapFilling/GF_std_dep_imputation.R")

n_cores = 10

qc_data <- readRDS("./data/processed/obs/qc_output/QC_(plusERA5)_data.RDS")

# data to be filled
obs_xyz <- qc_data$xyz[qc_data$xyz$SRC != "ERA5" ,]
obs_xyz_to_be_used <- qc_data$xyz[qc_data$xyz$SRC != "ERA5" ,] # just stations (total stations with filter and no filter)
obs_xyz_to_be_used_plusERA5 <- qc_data$xyz # stations + ERA5 (total stations with filter and no filter, ERA5 from filter)

####### Validation #######

# Using all data - comparing all obs data

# qc_data_values_tmax_filled <- qc_data$values$tmax
# qc_data_values_tmin_filled <- qc_data$values$tmin
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

for(xi in seq_along(param_spt)){
  
  # only stations
  
  # stations + ERA5
  
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
                         .$model_bc
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
                         .$model_bc
                     }, mc.cores = n_cores) %>%
    do.call("cbind", .) %>%
    setNames(obs_xyz$ID) -> tmin_ERA5_to_be_filled
  
  qc_data_values_tmin_ERA5_filled[, colnames(tmin_ERA5_to_be_filled)] <- tmin_ERA5_to_be_filled
  
  
}

# all_tmax_filled <- qc_data_values_tmax_filled[, obs_xyz$ID]
# all_tmin_filled <- qc_data_values_tmin_filled[, obs_xyz$ID]
all_tmax_ERA5_filled <- qc_data_values_tmax_ERA5_filled[, obs_xyz$ID]
all_tmin_ERA5_filled <- qc_data_values_tmin_ERA5_filled[, obs_xyz$ID]

all_tmax_ERA5_filled_full <- all_tmax_ERA5_filled[, colSums(is.na(all_tmax_ERA5_filled)) == 0]
lapply(colnames(all_tmax_ERA5_filled_full), 
       function(x){
         
         df_data <- data.frame(original = as.numeric(qc_data$values$tmax[, x]), 
                               model = as.numeric(all_tmax_ERA5_filled_full[, x]), 
                               date = time(all_tmax_ERA5_filled_full))
         df_data <- df_data[complete.cases(df_data),]
         
         openair::modStats(df_data, 
                           mod = "model", obs = "original",
                           statistic = c("MB", "MGE", "IOA"), type = "month") %>%
           .[, -1] %>% colMeans() %>% round(., 2)
         
       }) %>%
  do.call("rbind", .) %>% 
  data.frame(ID = colnames(all_tmax_ERA5_filled_full), Var = "Tmax", Type = "AV") -> AV_tmax

all_tmin_ERA5_filled_full <- all_tmin_ERA5_filled[, colSums(is.na(all_tmin_ERA5_filled)) == 0]
lapply(colnames(all_tmin_ERA5_filled_full), 
       function(x){
         
         df_data <- data.frame(original = as.numeric(qc_data$values$tmin[, x]), 
                               model = as.numeric(all_tmin_ERA5_filled_full[, x]), 
                               date = time(all_tmin_ERA5_filled_full))
         df_data <- df_data[complete.cases(df_data),]
         
         openair::modStats(df_data, 
                           mod = "model", obs = "original",
                           statistic = c("MB", "MGE", "IOA"), type = "month") %>%
           .[, -1] %>% colMeans() %>% round(., 2)
         
       }) %>%
  do.call("rbind", .) %>%
  data.frame(ID = colnames(all_tmin_ERA5_filled_full), Var = "Tmin", Type = "AV") -> AV_tmin



######## Cross-Validation ########

# Using 10 years (reducing data) of data in stations (cross-validation) with more that 70% years
obs_xyz <- qc_data$xyz[qc_data$xyz$filter_qc == 1 & qc_data$xyz$SRC != "ERA5" ,]
obs_xyz_10y <- qc_data$xyz[qc_data$xyz$filter_qc70 == 1 & qc_data$xyz$SRC != "ERA5", ]
obs_xyz_to_be_used <- qc_data$xyz[qc_data$xyz$SRC != "ERA5" ,] # just stations (total stations with filter and no filter)
obs_xyz_to_be_used_plusERA5 <- qc_data$xyz  # stations + ERA5 (total stations with filter and no filter, ERA5 from filter)

obs_xyz_values_10y <- qc_data$values
obs_xyz_values_10y <- lapply(qc_data$values,
                             function(z){
                               
                               years_to_be_preserved <- lapply(obs_xyz_10y$ID %>%
                                                                 setNames(obs_xyz_10y$ID),
                                                               function(x){
                                                                 TeachingDemos::char2seed(x)
                                                                 response <- sample(1981:2019, 10, replace=FALSE) 
                                                                 response
                                                               })
                               
                               
                               for(names_stat in names(years_to_be_preserved)){
                                 
                                 z[, names_stat][format(time(z), "%Y") %in% as.character(years_to_be_preserved[[names_stat]]) ]  <- NA
                               }
                               
                               z
                               
                             })

obs_xyz_values_no10y <- qc_data$values
obs_xyz_values_no10y <- lapply(obs_xyz_values_no10y,
                               function(z){
                                 
                                 years_to_be_preserved <- lapply(obs_xyz_10y$ID %>%
                                                                   setNames(obs_xyz_10y$ID),
                                                                 function(x){
                                                                   TeachingDemos::char2seed(x)
                                                                   response <- sample(1981:2019, 10, replace=FALSE)
                                                                   response
                                                                   
                                                                 })
                                 
                                 for(names_stat in names(years_to_be_preserved)){
                                   
                                   z[, names_stat][!(format(time(z), "%Y") %in% as.character(years_to_be_preserved[[names_stat]]))]  <- NA
                                   
                                 }
                                 
                                 z
                                 
                               })

qc_data_values_tmax_ERA5_filled <- obs_xyz_values_no10y$tmax
qc_data_values_tmin_ERA5_filled <- obs_xyz_values_no10y$tmin

# tree fases: 70/500, 70/1000, 100/1000
param_spt <- list(list(lmt_dist = 70, 
                       lmt_elv = 500,
                       lmt_n = 8),
                  list(lmt_dist = 100, 
                       lmt_elv = 500,
                       lmt_n = 8),
                  list(lmt_dist = 150, 
                       lmt_elv = 5500,
                       lmt_n = 8))

for(xi in seq_along(param_spt)){
  
  # only stations
  
  # stations + ERA5
  
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
                         .$model_bc
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
                         .$model_bc
                     }, mc.cores = n_cores) %>%
    do.call("cbind", .) %>%
    setNames(obs_xyz$ID) -> tmin_ERA5_to_be_filled
  
  qc_data_values_tmin_ERA5_filled[, colnames(tmin_ERA5_to_be_filled)] <- tmin_ERA5_to_be_filled
  
}

all_tmax_ERA5_filled <- qc_data_values_tmax_ERA5_filled[, obs_xyz_10y$ID]
all_tmin_ERA5_filled <- qc_data_values_tmin_ERA5_filled[, obs_xyz_10y$ID]


all_tmax_ERA5_filled_full <- all_tmax_ERA5_filled[, colSums(is.na(all_tmax_ERA5_filled)) == 0]
lapply(colnames(all_tmax_ERA5_filled_full), 
       function(x){
         
         df_data <- data.frame(original = as.numeric(obs_xyz_values_10y$tmax[, x]), 
                               model = as.numeric(all_tmax_ERA5_filled_full[, x]), 
                               date = time(all_tmax_ERA5_filled_full))
         df_data <- df_data[complete.cases(df_data),]
         
         openair::modStats(df_data, 
                           mod = "model", obs = "original",
                           statistic = c("MB", "MGE", "IOA"), type = "month") %>%
           .[, -1] %>% colMeans() %>% round(., 2)
         
       }) %>%
  do.call("rbind", .) %>% 
  data.frame(ID = colnames(all_tmax_ERA5_filled_full), Var = "Tmax", Type = "CV") -> CV_tmax


all_tmin_ERA5_filled_full <- all_tmin_ERA5_filled[, colSums(is.na(all_tmin_ERA5_filled)) == 0]
lapply(colnames(all_tmin_ERA5_filled_full), 
       function(x){
         
         df_data <- data.frame(original = as.numeric(obs_xyz_values_10y$tmin[, x]), 
                               model = as.numeric(all_tmin_ERA5_filled_full[, x]), 
                               date = time(all_tmin_ERA5_filled_full))
         df_data <- df_data[complete.cases(df_data),]
         
         openair::modStats(df_data, 
                           mod = "model", obs = "original",
                           statistic = c("MB", "MGE", "IOA"), type = "month") %>%
           .[, -1] %>% colMeans() %>% round(., 2)
         
       }) %>%
  do.call("rbind", .) %>%
  data.frame(ID = colnames(all_tmin_ERA5_filled_full), Var = "Tmin", Type = "CV") -> CV_tmin



######## Table and Plot ########

rbind(AV_tmax,
      AV_tmin,
      CV_tmax,
      CV_tmin) -> results

results$Type <- factor(results$Type,
                       levels = c("AV", "CV"),
                       labels = c("Available data", "10-years data"))

results3 <- aggregate(results, list(Variables = results$Var, 
                                    Experiment = results$Type), function(x) round(mean(x), 2))
results3_2 <- aggregate(results, list(Variables = results$Var, 
                                    Experiment = results$Type), function(x) length(x))

results3$Ndata <- results3_2$Type

results3 <- results3[, c("Variables","Experiment","Ndata", "MB", "MGE", "IOA")]
colnames(results3)[4:6] <- c("bias", "MAE", "dr") 

results3[results3$Variables == "Tmax",]
results3[results3$Variables == "Tmin",]

merge(results3[results3$Variables == "Tmax",],
      results3[results3$Variables == "Tmin",],
      by = c("Experiment")) %>%
  write.csv(file.path(".", "paper", "output", "Tab_gap_filling_CV.csv"))

# library(lattice)
# library(sp)

merge(results,
      obs_xyz[, c("ID", "LON", "LAT")],
      by = "ID") %>%
  transform(IOA_cut = cut(IOA, 
                          breaks = c(-Inf, .5, .6, .7, .8, .9, Inf),
                          labels = c("< .5", ".5 - .6", ".6 - .7", ".7 - .8", ".8 - .9","> .9"),
                          right = FALSE))-> results2


shp_peru = file.path(".", "data", "raw", "vectorial", "Departamentos.shp") %>% 
  raster::shapefile() 

shp_sa = file.path(".", "data", "raw", "vectorial", "Sudamérica.shp") %>% 
  raster::shapefile()

# font.settings <- list(fontfamily = "helvetica")
# mytheme <- list(strip.background = list(col = 'gray95'), 
#                 strip.border = list(col = 'black'),
#                 par.xlab.text = font.settings,
#                 par.ylab.text = font.settings,
#                 axis.text = font.settings,
#                 sub.text = font.settings,
#                 add.text = font.settings)

# a1 <- spplot(subset(results2, Var == "Tmax" & Type == "AV"), c("IOA_cut"), cex = .75, key.space = list(x = 1.125, y = 0.5, corner = c(.5,.5)))
# a2 <- spplot(subset(results2, Var == "Tmin" & Type == "AV"), c("IOA_cut"), cex = .75,key.space = list(x = 1.125, y = 0.5, corner = c(.5,.5)))
# a3 <- spplot(subset(results2, Var == "Tmax" & Type == "CV"), c("IOA_cut"), cex = .75, key.space = list(x = 1.125, y = 0.5, corner = c(.5,.5)))
# a4 <- spplot(subset(results2, Var == "Tmin" & Type == "CV"), c("IOA_cut"), cex = .75, key.space = list(x = 1.125, y = 0.5, corner = c(.5,.5)))

# c("CV - Tmax" = a3, "CV - Tmin" = a4, "Tmax" = a1, "Tmin" = a2, layout=c(2,2)) %>%
#   update(par.settings = mytheme, ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175)) %>%
#   update(par.settings = list(layout.widths =  list(right.padding = 12))) + 
#   latticeExtra::layer(sp.polygons(shp_peru, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE) + 
#   latticeExtra::layer(sp.polygons(shp_sa, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE) -> plot_dr


# cols <- rev(colorRampPalette(ochRe::ochre_palettes$healthy_reef)(6))
# 
# plot_dr <- xyplot(LAT ~ LON | Type + Var, group = IOA_cut, data = results2,
#                   key=list(space = "right",
#                            points = list(col = cols, pch = 20, cex = 2),
#                            text =list(levels(results2$IOA_cut))),
#                   par.settings = list(
#                     superpose.symbol = list(pch = 20,
#                                             cex = 1.25,
#                                             col = cols))) %>%
#   update(par.settings = mytheme, ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175), xlab = "", ylab = "") +
#   latticeExtra::layer(sp.polygons(shp_peru, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE) +
#   latticeExtra::layer(sp.polygons(shp_sa, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE)
# 
# 
# jpeg(filename = file.path(".", "paper", "output", "Fig_gap_filling_CV.jpg"),
#      width = 1300, height = 1700, units = "px",
#      res = 200)
# print(plot_dr)
# dev.off()

# stats
colnames(results3)[c(1, 2)] <- c("Var", "Type")

library(ggplot2)

# plots
cols3 <- rev(colorRampPalette(c("#4682B4", "gray90", "#B47846"))(6))

plt_dr <- results2 %>%
  ggplot() + 
  geom_polygon(data = shp_sa %>% broom::tidy(),
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_polygon(data = shp_peru %>% broom::tidy(),
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) + 
  geom_point(aes(x = LON, y = LAT, color = IOA_cut), shape = 19, size = 2) + 
  facet_grid(Type~Var, switch = "y") + 
  #scale_fill_manual(values = cols1) + 
  scale_color_manual(values = cols3, drop = FALSE) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(size = 12),
        legend.position="bottom",
        legend.spacing.x = unit(.1, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5),
        legend.text = element_text(
          margin = margin(l = 3, r = 3, unit = "pt"),
          hjust = 0)
  ) +
  guides(color = guide_legend(override.aes = list(size = 3),
                              nrow = 1, byrow = TRUE,
                              label.position = "bottom",
                              title = expression(italic(d[r])))) +
  xlab("") + ylab("") + 
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175)) + 
  
  geom_text(
    data    = results3 %>% .[, c("Type","Var", "dr")],
    mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", dr, sep = " ")), size = 4,
    hjust   = -0.1,
    vjust   = -1
  )

plt_dr
ggsave(filename = file.path(".", "paper", "output", "Fig_gap_filling_CV.jpg"),
       dpi = 250, scale = 1,
       width = 7, height = 7, units = "in")

