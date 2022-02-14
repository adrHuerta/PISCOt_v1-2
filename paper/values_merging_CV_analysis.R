rm(list = ls())

library(xts)
library(spatialsample)
"%>%" = magrittr::`%>%`

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/OBS.RDS")

# shps
shp_peru = file.path(".", "data", "raw", "vectorial", "SEC_CLIM.shp") %>%
  shapefile()
shp_peru@data$MAIREG = transform(shp_peru@data, MAIREG = ifelse(MAC_REG == "CO", "CO", ifelse(MAC_REG == "SEA", "SE", ifelse(MAC_REG == "SEB", "SE", ifelse(MAC_REG == "SIOC", "AN", "AN")))))$MAIREG
shp_peru <- shp_peru %>% raster::aggregate(., by = 'MAIREG') %>%  
  broom::tidy(region = "MAIREG") %>%
  transform(., id2 = ifelse(id == "CO", "PC", ifelse(id == "SE", "AZ", "AN")))
shp_peru$id2 <- factor(shp_peru$id2, 
                       levels = c("PC", "AN", "AZ"), 
                       labels = c("Pacific Coast", "Andes", "Amazon"))

shp_sa = file.path(".", "data", "raw", "vectorial", "Sudamérica.shp") %>% 
  raster::shapefile() %>% broom::tidy()

# stations for cv
stations_CV <- qc_data$xyz[qc_data$xyz@data$filter_qc70 != 0, ]

####### points for spcv/nospcv #######
set.seed(2020+1)
folds_spcv <- spatial_clustering_cv(stations_CV@data, coords = c("LON", "LAT"), v = 10)
folds_nospcv <- rsample::vfold_cv(stations_CV@data, v = 10)


lapply(seq_len(length(folds_spcv$splits)), function(x){
  
  data_sp <- qc_data$xyz@data[, c("ID", "LAT", "LON")]
  data_sp$spcv <- TRUE
  data_sp$nospcv <- TRUE
  data_sp[match(assessment(folds_spcv$splits[[x]])$ID, data_sp$ID), "spcv"] <- FALSE
  data_sp[match(assessment(folds_nospcv$splits[[x]])$ID, data_sp$ID), "nospcv"] <- FALSE
  data_sp$kfold <- x
  data_sp
  
}) -> exp

plt1 <- lattice::xyplot(LAT ~ LON, groups = kfold, data = do.call("rbind", exp) %>% subset(spcv == FALSE),
                        pch  =c(1:10), xlab = "", ylab = "")
plt2 <- lattice::xyplot(LAT ~ LON, groups = kfold, data = do.call("rbind", exp) %>% subset(nospcv == FALSE),
                        pch  =c(1:10), xlab = "", ylab = "")

# c(plt1, plt2) %>%
#   update(ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175), xlab = "", ylab = "") +
#   latticeExtra::layer(sp::sp.polygons(shp_peru, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE) +
#   latticeExtra::layer(sp::sp.polygons(shp_sa, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE)
plt2 %>%
  update(ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175), xlab = "", ylab = "") +
  latticeExtra::layer(sp::sp.polygons(shp_peru, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE) +
  latticeExtra::layer(sp::sp.polygons(shp_sa, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE)

####### statistics for spcv/nospcv #######

tmax_obs <- qc_data$values$tmax[, stations_CV@data$ID]
tmin_obs <- qc_data$values$tmin[, stations_CV@data$ID]

# cv
output_anomalies <- "./paper/others/values"

# tmax_spcv <- file.path(output_anomalies, sprintf("%s/tmax_spcv_%s.RDS", "tmax",  time(tmax_obs))) %>%
#   lapply(function(x) readRDS(x)) %>% do.call("rbind", .)
# 
# tmin_spcv <- file.path(output_anomalies, sprintf("%s/tmin_spcv_%s.RDS", "tmin",  time(tmax_obs))) %>%
#   lapply(function(x) readRDS(x)) %>% do.call("rbind", .)

tmax_nospcv <- file.path(output_anomalies, sprintf("%s/tmax_nospcv_%s.RDS", "tmax",  time(tmax_obs))) %>%
  lapply(function(x) readRDS(x)) %>% do.call("rbind", .)

tmin_nospcv <- file.path(output_anomalies, sprintf("%s/tmin_nospcv_%s.RDS", "tmin",  time(tmax_obs))) %>%
  lapply(function(x) readRDS(x)) %>% do.call("rbind", .)

# statistics
cold_months <- which(format(time(tmax_obs), "%m") %in% c("04", "05", "06", "07", "08", "09"))
warm_months <- which(format(time(tmax_obs), "%m") %in% c("10", "11", "12", "01", "02", "03"))

# # spcv
# tmax_spcv_stat_warm <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model, date = time(tmax_obs[warm_months, ])), 
#                     statistic = c("MB", "MGE", "IOA"), type = "month") %>%
#     .[, -1] %>% colMeans() %>% round(., 2)
#   
# }, 
# obs = data.frame(tmax_obs[warm_months, ]),
# model = data.frame(tmax_spcv[warm_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmax", cv = "spcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])
# 
# tmax_spcv_stat_cold <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model, date = time(tmax_obs[cold_months, ])), 
#                     statistic = c("MB", "MGE", "IOA"), type = "month") %>%
#     .[, -1] %>% colMeans() %>% round(., 2)
#   
# }, 
# obs = data.frame(tmax_obs[cold_months, ]),
# model = data.frame(tmax_spcv[cold_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmax", cv = "spcv", season = "Apr.-Sep.")  %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])
# 
# tmin_spcv_stat_warm <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model, date = time(tmin_obs[warm_months, ])), 
#                     statistic = c("MB", "MGE", "IOA"), type = "month") %>%
#     .[, -1] %>% colMeans() %>% round(., 2)
#   
# }, 
# obs = data.frame(tmin_obs[warm_months, ]),
# model = data.frame(tmin_spcv[warm_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmin", cv = "spcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])
# 
# tmin_spcv_stat_cold <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model, date = time(tmin_obs[cold_months, ])), 
#                     statistic = c("MB", "MGE", "IOA"), type = "month") %>%
#     .[, -1] %>% colMeans() %>% round(., 2)
#   
# }, 
# obs = data.frame(tmin_obs[cold_months, ]),
# model = data.frame(tmin_spcv[cold_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmin", cv = "spcv", season = "Apr.-Sep.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

# nospcv
tmax_nospcv_stat_warm <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model, date = time(tmax_obs[warm_months, ])), 
                    statistic = c("MB", "MGE", "IOA"), type = "month") %>%
    .[, -1] %>% colMeans() %>% round(., 2)
  }, 
  obs = data.frame(tmax_obs[warm_months, ]),
  model = data.frame(tmax_nospcv[warm_months, ]), 
  SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmax", cv = "nospcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

tmax_nospcv_stat_cold <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model, date = time(tmax_obs[cold_months, ])), 
                    statistic = c("MB", "MGE", "IOA"), type = "month") %>%
    .[, -1] %>% colMeans() %>% round(., 2)
  }, 
  obs = data.frame(tmax_obs[cold_months, ]),
  model = data.frame(tmax_nospcv[cold_months, ]),
  SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmax", cv = "nospcv", season = "Apr.-Sep.")  %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

tmin_nospcv_stat_warm <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model, date = time(tmin_obs[warm_months, ])), 
                    statistic = c("MB", "MGE", "IOA"), type = "month") %>%
    .[, -1] %>% colMeans() %>% round(., 2)
  }, 
  obs = data.frame(tmin_obs[warm_months, ]),
  model = data.frame(tmin_nospcv[warm_months, ]),
  SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmin", cv = "nospcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

tmin_nospcv_stat_cold <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model, date = time(tmin_obs[cold_months, ])), 
                    statistic = c("MB", "MGE", "IOA"), type = "month") %>%
    .[, -1] %>% colMeans() %>% round(., 2)
  }, 
  obs = data.frame(tmin_obs[cold_months, ]),
  model = data.frame(tmin_nospcv[cold_months, ]),
  SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmin", cv = "nospcv", season = "Apr.-Sep.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])


values_cv <-
  rbind(
    # tmax_spcv_stat_warm,
    # tmax_spcv_stat_cold,
    # tmin_spcv_stat_warm,
    # tmin_spcv_stat_cold,
    tmax_nospcv_stat_warm,
    tmax_nospcv_stat_cold,
    tmin_nospcv_stat_warm,
    tmin_nospcv_stat_cold) %>%
  transform(ID = rownames(.),
            bias = cut(MB, 
                       breaks = c(-Inf, -3, -2, -1, 1, 2, 3, Inf),
                       labels = c("< -3", "-3 - -2", "-2 - -1", "-1 - 1", "1 - 2", "2 - 3", "> 3"),
                       right = FALSE),
            MAE = cut(MGE, 
                      breaks = c(0, .5, 1, 1.5, 2, 2.5, 3, Inf),
                      labels = c("0 - .5", ".5 - 1", "1 - 1.5", "1.5 - 2", "2 - 2.5", "2.5 - 3", "> 3"), 
                      right = FALSE),
            dr = cut(IOA, 
                     breaks = c(-Inf, .5, .6, .7, .8, .9, Inf),
                     labels = c("< .5", ".5 - .6", ".6 - .7", ".7 - .8", ".8 - .9","> .9"), 
                     right = FALSE)) 

# stats
overall_values <- aggregate(. ~ season + var + cv, data = values_cv[, c(1, 2, 3, 4, 5, 6)], FUN = function(x) round(mean(x), 2))

# plots
cols1 <- colorRampPalette(c("#4682B4", "gray90", "#B47846"))(7)
cols2 <- colorRampPalette(c("#4682B4", "gray90", "#B47846"))(7)
cols3 <- rev(colorRampPalette(c("#4682B4", "gray90", "#B47846"))(6))


library(ggplot2)

# nospcv
plt_nospcv_bias <- values_cv %>% subset(cv == "nospcv") %>%
  ggplot() + 
  geom_polygon(data = shp_sa,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) + 
  geom_point(aes(x = LON, y = LAT, color = bias), shape = 19, size = 2) + 
  facet_grid(season~var, switch = "y") + 
  #scale_fill_manual(values = cols1) + 
  scale_color_manual(values = cols1, drop = FALSE) +
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
          margin = margin(l = 4, r = 4, unit = "pt"),
          hjust = 0)
  ) +
  guides(color = guide_legend(override.aes = list(size = 3),
                              nrow = 1, byrow = TRUE,
                              label.position = "bottom",
                              title="bias\n(°C)")) +
  xlab("") + ylab("") + 
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175))  + 
  geom_text(
    data    = overall_values %>% subset(cv == "nospcv") %>% .[, c("season","var", "MB")],
    mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", MB, "°C", sep = " ")), size = 4,
    hjust   = -0.1,
    vjust   = -1
  )


plt_nospcv_MAE <- values_cv %>% subset(cv == "nospcv") %>%
  ggplot() + 
  geom_polygon(data = shp_sa,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) + 
  geom_point(aes(x = LON, y = LAT, color = MAE), shape = 19, size = 2) + 
  facet_grid(season~var, switch = "y") + 
  #scale_fill_manual(values = cols2) + 
  scale_color_manual(values = cols2, drop = FALSE) +
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
          margin = margin(l = 4, r = 4, unit = "pt"),
          hjust = 0)
  ) +
  guides(color = guide_legend(override.aes = list(size = 3),
                              nrow = 1, byrow = TRUE,
                              label.position = "bottom",
                              title="MAE\n(°C)")) +
  xlab("") + ylab("") + 
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175))   + 
  geom_text(
    data    = overall_values %>% subset(cv == "nospcv") %>% .[, c("season","var", "MGE")],
    mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", MGE, "°C", sep = " ")), size = 3.5,
    hjust   = -0.1,
    vjust   = -1
  )

plt_nospcv_dr <- values_cv %>% subset(cv == "nospcv") %>%
  ggplot() + 
  geom_polygon(data = shp_sa,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) + 
  geom_point(aes(x = LON, y = LAT, color = dr), shape = 19, size = 2) + 
  facet_grid(season~var, switch = "y") + 
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
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175))   + 
  geom_text(
    data    = overall_values %>% subset(cv == "nospcv") %>% .[, c("season","var", "IOA")],
    mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", IOA, "°C", sep = " ")), size = 3.5,
    hjust   = -0.1,
    vjust   = -1
  )

# spcv
# plt_spcv_bias <- values_cv %>% subset(cv == "spcv") %>%
#   ggplot() + 
#   geom_polygon(data = shp_sa %>% broom::tidy(),
#                aes(x = long, y = lat, group = group),
#                fill = NA, colour = "gray20", size = 0.3) +
#   geom_polygon(data = shp_peru %>% broom::tidy(),
#                aes(x = long, y = lat, group = group),
#                fill = NA, colour = "gray20", size = 0.3) + 
#   geom_point(aes(x = LON, y = LAT, color = bias), shape = 19, size = 2) + 
#   facet_grid(season~var, switch = "y") + 
#   #scale_fill_manual(values = cols1) + 
#   scale_color_manual(values = cols1, drop = FALSE) +
#   theme_bw() + 
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA),
#         strip.text = element_text(size = 12),
#         legend.position="bottom",
#         legend.spacing.x = unit(.1, 'cm'),
#         legend.spacing.y = unit(.05, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-5,-5,-5,-5),
#         legend.text = element_text(
#           margin = margin(l = 4, r = 4, unit = "pt"),
#           hjust = 0)
#   ) +
#   guides(color = guide_legend(override.aes = list(size = 3),
#                               nrow = 1, byrow = TRUE,
#                               label.position = "bottom",
#                               title="bias\n(°C)")) +
#   xlab("") + ylab("") + 
#   coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175))  + 
#   geom_text(
#     data    = overall_values %>% subset(cv == "spcv") %>% .[, c("season","var", "MB")],
#     mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", MB, "°C", sep = " ")), size = 4,
#     hjust   = -0.1,
#     vjust   = -1
#   )
# 
# 
# plt_spcv_MAE <- values_cv %>% subset(cv == "spcv") %>%
#   ggplot() + 
#   geom_polygon(data = shp_sa %>% broom::tidy(),
#                aes(x = long, y = lat, group = group),
#                fill = NA, colour = "gray20", size = 0.3) +
#   geom_polygon(data = shp_peru %>% broom::tidy(),
#                aes(x = long, y = lat, group = group),
#                fill = NA, colour = "gray20", size = 0.3) + 
#   geom_point(aes(x = LON, y = LAT, color = MAE), shape = 19, size = 2) + 
#   facet_grid(season~var, switch = "y") + 
#   #scale_fill_manual(values = cols2) + 
#   scale_color_manual(values = cols2, drop = FALSE) +
#   theme_bw() + 
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA),
#         strip.text = element_text(size = 12),
#         legend.position="bottom",
#         legend.spacing.x = unit(.1, 'cm'),
#         legend.spacing.y = unit(.05, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-5,-5,-5,-5),
#         legend.text = element_text(
#           margin = margin(l = 4, r = 4, unit = "pt"),
#           hjust = 0)
#   ) +
#   guides(color = guide_legend(override.aes = list(size = 3),
#                               nrow = 1, byrow = TRUE,
#                               label.position = "bottom",
#                               title="MAE\n(°C)")) +
#   xlab("") + ylab("") + 
#   coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175))   + 
#   geom_text(
#     data    = overall_values %>% subset(cv == "spcv") %>% .[, c("season","var", "MGE")],
#     mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", MGE, "°C", sep = " ")), size = 3.5,
#     hjust   = -0.1,
#     vjust   = -1
#   )
# 
# plt_spcv_dr <- values_cv %>% subset(cv == "spcv") %>%
#   ggplot() + 
#   geom_polygon(data = shp_sa %>% broom::tidy(),
#                aes(x = long, y = lat, group = group),
#                fill = NA, colour = "gray20", size = 0.3) +
#   geom_polygon(data = shp_peru %>% broom::tidy(),
#                aes(x = long, y = lat, group = group),
#                fill = NA, colour = "gray20", size = 0.3) + 
#   geom_point(aes(x = LON, y = LAT, color = dr), shape = 19, size = 2) + 
#   facet_grid(season~var, switch = "y") + 
#   scale_color_manual(values = cols3, drop = FALSE) +
#   theme_bw() + 
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA),
#         strip.text = element_text(size = 12),
#         legend.position="bottom",
#         legend.spacing.x = unit(.1, 'cm'),
#         legend.spacing.y = unit(.05, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-5,-5,-5,-5),
#         legend.text = element_text(
#           margin = margin(l = 3, r = 3, unit = "pt"),
#           hjust = 0)
#   ) +
#   guides(color = guide_legend(override.aes = list(size = 3),
#                               nrow = 1, byrow = TRUE,
#                               label.position = "bottom",
#                               title = expression(italic(d[r])))) +
#   xlab("") + ylab("") + 
#   coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175))   + 
#   geom_text(
#     data    = overall_values %>% subset(cv == "spcv") %>% .[, c("season","var", "IOA")],
#     mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", IOA, "°C", sep = " ")), size = 3.5,
#     hjust   = -0.1,
#     vjust   = -1
#   )

library(patchwork)

(plt_nospcv_bias +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm")))  +  
  (plt_nospcv_MAE + 
     theme(strip.background.y = element_blank(), strip.text.y = element_blank()) + 
     theme(plot.margin=grid::unit(c(0,0,0,0), "mm")))

ggsave(file.path(".", "paper", "output", "Figure_05_values_nospcv_bias_mae.tiff"),
       device = "tiff",
       dpi = 500, scale = 1,
       width = 9.5, height = 7, units = "in")

plt_nospcv_dr

ggsave(file.path(".", "paper", "output", "Figure_06_values_nospcv_dr.tiff"),
       dpi = 500, scale = 1,
       width = 7, height = 7, units = "in")
