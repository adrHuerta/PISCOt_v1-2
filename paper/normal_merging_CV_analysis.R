rm(list = ls())

library(spatialsample)
"%>%" = magrittr::`%>%`

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Normals_OBS.RDS")

shp_peru = file.path(".", "data", "raw", "vectorial", "Departamentos.shp") %>% 
  raster::shapefile() 

shp_sa = file.path(".", "data", "raw", "vectorial", "Sudamérica.shp") %>% 
  raster::shapefile()

stations_CV <- qc_data$xyz

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
output_normals <- "./paper/others/normals"
# tmax_spcv <- file.path(output_normals, sprintf("%s/tmax_spcv_%02d.RDS", "tmax",  1:12)) %>%
#   lapply(function(x) readRDS(x)) %>% do.call("rbind", .)
# 
# tmin_spcv <- file.path(output_normals, sprintf("%s/tmin_spcv_%02d.RDS", "tmin",  1:12)) %>%
#   lapply(function(x) readRDS(x)) %>% do.call("rbind", .)

tmax_nospcv <- file.path(output_normals, sprintf("%s/tmax_nospcv_%02d.RDS", "tmax",  1:12)) %>%
  lapply(function(x) readRDS(x)) %>% do.call("rbind", .)

tmin_nospcv <- file.path(output_normals, sprintf("%s/tmin_nospcv_%02d.RDS", "tmin",  1:12)) %>%
  lapply(function(x) readRDS(x)) %>% do.call("rbind", .)

# statistics
cold_months <- c(4, 5, 6, 7, 8, 9)
warm_months <- c(10, 11, 12, 1, 2, 3)

# # spcv
# tmax_spcv_stat_warm <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model), 
#                     statistic = c("MB", "MGE"))
# }, 
# obs = data.frame(tmax_obs[warm_months, ]),
# model = data.frame(tmax_spcv[warm_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmax", cv = "spcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])
# 
# tmax_spcv_stat_cold <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model), 
#                     statistic = c("MB", "MGE"))
# }, 
# obs = data.frame(tmax_obs[cold_months, ]),
# model = data.frame(tmax_spcv[cold_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmax", cv = "spcv", season = "Apr.-Sep.")  %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])
# 
# tmin_spcv_stat_warm <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model), 
#                     statistic = c("MB", "MGE"))
# }, 
# obs = data.frame(tmin_obs[warm_months, ]),
# model = data.frame(tmin_spcv[warm_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmin", cv = "spcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])
# 
# tmin_spcv_stat_cold <- parallel::mcmapply(function(obs, model){
#   
#   openair::modStats(mod = "model", obs = "obs",
#                     mydata = data.frame(obs = obs, model = model), 
#                     statistic = c("MB", "MGE"))
# }, 
# obs = data.frame(tmin_obs[cold_months, ]),
# model = data.frame(tmin_spcv[cold_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
#   do.call("rbind", .) %>%
#   data.frame(., var = "Tmin", cv = "spcv", season = "Apr.-Sep.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])


# nospcv
tmax_nospcv_stat_warm <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model), 
                    statistic = c("MB", "MGE"))
  }, 
  obs = data.frame(tmax_obs[warm_months, ]),
  model = data.frame(tmax_nospcv[warm_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmax", cv = "nospcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

tmax_nospcv_stat_cold <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model), 
                    statistic = c("MB", "MGE"))
}, 
obs = data.frame(tmax_obs[cold_months, ]),
model = data.frame(tmax_nospcv[cold_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmax", cv = "nospcv", season = "Apr.-Sep.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

tmin_nospcv_stat_warm <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model), 
                    statistic = c("MB", "MGE"))
}, 
obs = data.frame(tmin_obs[warm_months, ]),
model = data.frame(tmin_nospcv[warm_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmin", cv = "nospcv", season = "Oct.-Mar.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

tmin_nospcv_stat_cold <- parallel::mcmapply(function(obs, model){
  
  openair::modStats(mod = "model", obs = "obs",
                    mydata = data.frame(obs = obs, model = model), 
                    statistic = c("MB", "MGE"))
}, 
obs = data.frame(tmin_obs[cold_months, ]),
model = data.frame(tmin_nospcv[cold_months, ]), SIMPLIFY = FALSE, mc.cores = 5) %>%
  do.call("rbind", .) %>%
  data.frame(., var = "Tmin", cv = "nospcv", season = "Apr.-Sep.") %>% cbind(., stations_CV@data[, c("ID", "LON", "LAT")])

normals_cv <-
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
                      right = FALSE)) 

# stats
overall_values <- aggregate(. ~ season + var + cv, data = normals_cv[, c(2, 3, 4, 5, 6)], FUN = function(x) round(mean(x), 2))


# plots
cols1 <- colorRampPalette(c("#4682B4", "gray90", "#B47846"))(7)
cols2 <- colorRampPalette(c("#4682B4", "gray90", "#B47846"))(7)

library(ggplot2)
 
plt_nospcv_bias <- normals_cv %>% subset(cv == "nospcv") %>%
  ggplot() + 
  geom_polygon(data = shp_sa %>% broom::tidy(),
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_polygon(data = shp_peru %>% broom::tidy(),
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
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175)) + 
  geom_text(
    data    = overall_values %>% subset(cv == "nospcv") %>% .[, c("season","var", "MB")],
    mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", MB, "°C", sep = " ")), size = 4,
    hjust   = -0.1,
    vjust   = -1
  )


plt_nospcv_MAE <- normals_cv %>% subset(cv == "nospcv") %>%
  ggplot() + 
  geom_polygon(data = shp_sa %>% broom::tidy(),
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_polygon(data = shp_peru %>% broom::tidy(),
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
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175))  + 
  geom_text(
    data    = overall_values %>% subset(cv == "nospcv") %>% .[, c("season","var", "MGE")],
    mapping = aes(x = -Inf, y = -Inf, label = paste("Overall:", MGE, "°C", sep = " ")), size = 4,
    hjust   = -0.1,
    vjust   = -1
  )

# plt_spcv_bias <- normals_cv %>% subset(cv == "spcv") %>%
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
# plt_spcv_MAE <- normals_cv %>% subset(cv == "spcv") %>%
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

library(patchwork)
(plt_nospcv_bias +
    theme(plot.margin=margin(l=-0.8,unit="cm")))  +  
  (plt_nospcv_MAE + theme(strip.background.y = element_blank(), strip.text.y = element_blank()) + 
     theme(plot.margin=margin(l=-0.8,unit="cm")))

ggsave(file.path(".", "paper", "output", "Fig_normal_nospcv.jpg"),
       dpi = 250, scale = 1,
       width = 9.5, height = 7, units = "in")

# (plt_spcv_bias +
#     theme(plot.margin=margin(l=-0.8,unit="cm")))  +  
#   (plt_spcv_MAE + theme(strip.background.y = element_blank(), strip.text.y = element_blank()) + 
#      theme(plot.margin=margin(l=-0.8,unit="cm")))
# 
# ggsave(file.path(".", "paper", "output", "Fig_normal_spcv.jpg"),
#        dpi = 250, scale = 1,
#        width = 9.5, height = 7, units = "in")
