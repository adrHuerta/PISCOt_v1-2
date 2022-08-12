rm(list = ls())

library(raster)
"%>%" = magrittr::`%>%`

source('./src/process/Merging/MG_make_covariables.R')
source('./src/process/Merging/MG_GWRK.R')

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Normals_OBS.RDS")
stats_above_alt <- qc_data$xyz@data[qc_data$xyz@data$ALT > 100, "ID"]
stats_above_alt <- stats_above_alt[!is.na(stats_above_alt)]

# gridded
LST_day <- raster::brick("data/processed/gridded/co_variables/LST_DAY.nc")
LST_night <- raster::brick("data/processed/gridded/co_variables/LST_NIGHT.nc")
# CC <- raster::brick("data/processed/gridded/co_variables/CC.nc")
DEM <- raster::raster("data/processed/gridded/co_variables/DEM.nc")
X <- raster::raster("data/processed/gridded/co_variables/X.nc")
Y <- raster::raster("data/processed/gridded/co_variables/Y.nc")

# making list of covs
covs_list_tmax <- list(dynamic = list(LST = LST_day),
                       static = list(DEM = DEM, X = X, Y = Y))

covs_list_tmin <- list(dynamic = list(LST = LST_night),
                       static = list(DEM = DEM, X = X, Y = Y))

lapply(1:12, function(months_i){
  
  data_obss_covs <- make_Normal_coVariables(month_value = months_i,
                                            var = "tmax",
                                            covs_list = covs_list_tmax,
                                            obs = qc_data)
  
  bw <- GWR_model_fitting(obs_cov_data = data_obss_covs, resFitting = 10)$bw
  p_cov <- raster::extract(data_obss_covs$covs, 
                           data_obss_covs$obs, 
                           cellnumber = FALSE, sp = TRUE) %>%
    as.data.frame(xy = TRUE)
  
  p_cov <- p_cov[match(stats_above_alt, rownames(p_cov)), ]
  
  lapply(seq_len(nrow(p_cov)), function(x){
    
    data_target <- p_cov
    data_target$DIST <- geosphere::distGeo(p_cov[x, c("LON", "LAT")],
                                           p_cov[, c("LON", "LAT")])
    
    data_target <- data_target[order(data_target$DIST, decreasing = FALSE), ]
    data_target <- data_target[1:bw, 1:6]
    
    relaimpo::calc.relimp(data_obss_covs$formula_lm, data = data_target, type = "lmg")@lmg
    
  }) %>% do.call("rbind", .) %>% apply(., 2, mean) -> tmax_relimp
  
  tmax_relimp
  
}) %>% do.call("rbind", .) -> tmax_monthly_relimp

lapply(1:12, function(months_i){
  
  data_obss_covs <- make_Normal_coVariables(month_value = months_i,
                                            var = "tmin",
                                            covs_list = covs_list_tmin,
                                            obs = qc_data)
  
  bw <- GWR_model_fitting(obs_cov_data = data_obss_covs, resFitting = 10)$bw
  p_cov <- raster::extract(data_obss_covs$covs, 
                           data_obss_covs$obs, 
                           cellnumber = FALSE, sp = TRUE) %>%
    as.data.frame(xy = TRUE)
  
  p_cov <- p_cov[match(stats_above_alt, rownames(p_cov)), ]
  
  lapply(seq_len(nrow(p_cov)), function(x){
    
    data_target <- p_cov
    data_target$DIST <- geosphere::distGeo(p_cov[x, c("LON", "LAT")],
                                           p_cov[, c("LON", "LAT")])
    
    data_target <- data_target[order(data_target$DIST, decreasing = FALSE), ]
    data_target <- data_target[1:bw, 1:6]
    
    relaimpo::calc.relimp(data_obss_covs$formula_lm, data = data_target, type = "lmg")@lmg
    
    }) %>% do.call("rbind", .) %>% apply(., 2, mean) -> tmin_relimp
  
  tmin_relimp
  
  }) %>% do.call("rbind", .) -> tmin_monthly_relimp

# plotting


all_propor <- rbind(data.frame(reshape2::melt(data.frame(data.frame(tmax_monthly_relimp,
                                                                    ALL = apply(tmax_monthly_relimp, 1, sum)), id = 1:12), id  = "id"), var = "Tmax"),
                    data.frame(reshape2::melt(data.frame(data.frame(tmin_monthly_relimp,
                                                                    ALL = apply(tmin_monthly_relimp, 1, sum)), id = 1:12), id  = "id"), var = "Tmin"))

months_labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

library(ggplot2)

plot_all_pro <- ggplot(subset(all_propor, variable == "ALL"), aes(x = id, y = value, colour = var)) + 
  geom_line() + 
  geom_point(size = 2.2) + 
  scale_colour_manual("",values = c("#ff5454ff", "#3395efff")) + 
  ylab("R²") + xlab("") + theme_bw() + 
  scale_x_continuous(limits = c(1,12), breaks = 1:12, labels = months_labels) + 
  scale_y_continuous(limits = c(.6, 1), breaks = seq(.6, 1, .1)) + 
  theme(legend.position= c(.1,.25),
        legend.background=element_blank(),
        legend.key = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

# tmax/tmin (order of levels)
plot_pro_by_cov <- ggplot(subset(all_propor, variable != "ALL"), aes(x = id, y = value, colour = variable)) + 
  geom_line() + 
  geom_point(size = 2.2) + 
  ylab("Proportion of R²") + xlab("") + theme_bw() + 
  scale_x_continuous(limits = c(1,12), breaks = 1:12, labels = months_labels) + 
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, .6, .1)) + 
  theme(legend.position= c(.75,.37)) + 
  theme(legend.title=element_blank())+
  facet_wrap(~var) + 
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.background = element_blank(), 
        legend.direction = "horizontal",
        legend.key = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm")) 

cowplot::plot_grid(plot_all_pro, plot_pro_by_cov,
                   ncol = 1)

ggsave(file.path(".", "paper", "output", "Figure_05_contributions_of_covs.pdf"),
       device = "pdf",
       dpi = 500, scale = 0.75,
       width = 9, height = 6, units = "in")
