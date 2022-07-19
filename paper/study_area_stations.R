rm(list = ls())
"%>%" = magrittr::`%>%`

library(raster)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rworldmap)
library(dplyr)
library(geosphere)

theme_set(theme_bw() + theme(text = element_text(family = 'helvetica')))

###

dem = file.path(".", "data", "processed", "gridded", "co_variables", "DEM.nc") %>% 
  raster() %>%
  {./1000}

shp_peru = file.path(".", "data", "raw", "vectorial", "SEC_CLIM.shp") %>%
  shapefile()
shp_peru@data$MAIREG = transform(shp_peru@data, MAIREG = ifelse(MAC_REG == "CO", "CO", ifelse(MAC_REG == "SEA", "SE", ifelse(MAC_REG == "SEB", "SE", ifelse(MAC_REG == "SIOC", "AN", "AN")))))$MAIREG
shp_peru <- shp_peru %>% raster::aggregate(., by = 'MAIREG') %>%  
  broom::tidy(region = "MAIREG") %>%
  transform(., id2 = ifelse(id == "CO", "PC", ifelse(id == "SE", "AZ", "AN")))
shp_peru$id2 <- factor(shp_peru$id2, 
                         levels = c("PC", "AN", "AZ"), 
                         labels = c("Pacific Coast", "Andes", "Amazon"))

shp_lakes = file.path(".", "data", "raw", "vectorial", "Lagos_lagunas_Project.shp") %>% 
  shapefile()  %>% 
  .[.@data$Rasgo_Secu == "Perenne", ] %>%
  .[.@data$are > 1000, ]

shp_sa <- file.path(".", "data", "raw", "vectorial", "Sudamérica.shp") %>% 
  shapefile()
  
dem <- raster::mask(raster::crop(dem, shp_sa), shp_sa)
shp_sa <- shp_sa %>%
  broom::tidy()
# qc 01

qc01 <- file.path(".", "data", "processed", "obs", "qc_output", "RAW(QC01)_data.RDS") %>% 
  readRDS()

# qc data

# qc_data <- file.path(".", "data", "processed", "obs", "qc_output", "QC_data.RDS") %>% 
#   readRDS()

qc_data <- file.path(".", "data", "processed", "obs", "qc_output", "OBS.RDS") %>% 
  readRDS() 
qc_data$xyz <- qc_data$xyz@data

#### 
# spatial variability

qc01$xyz$ALT <- qc01$xyz$ALT / 1000
qc_data$xyz$ALT <- qc_data$xyz$ALT / 1000

## p1 

# dem_lat_studyarea_mean <- raster::zonal(x = dem, 
#                                         z = raster::init(dem, v='x'), 
#                                         fun = "mean", 
#                                         digits = 3) %>%
#   data.frame() %>%
#   transform(data = "Study Area") %>%
#   setNames(c("lat_seq", "value", "data"))
# 
# dem_lat_peru_mean <- raster::zonal(x = raster::mask(dem, shp_peru), 
#                                    z = raster::init(raster::mask(dem, shp_peru), v='x'), 
#                                    fun = "mean", 
#                                    digits = 3)%>%
#   data.frame() %>%
#   transform(data = "Peru") %>%
#   setNames(c("lat_seq", "value", "data"))
# 
# dem_lat_peru_sd <- raster::zonal(x = raster::mask(dem, shp_peru), 
#                                  z = raster::init(raster::mask(dem, shp_peru), v='x'), 
#                                  fun = "sd", 
#                                  digits = 3) %>%
#   data.frame() %>%
#   setNames(c("lat_seq", "value")) %>%
#   transform(value_max = (dem_lat_peru_mean$value + value) %>% {ifelse(. >= 0, ., 0)},
#             value_min = (dem_lat_peru_mean$value - value) %>% {ifelse(. >= 0, ., 0)})
# 
# 
# p1 <-
#   rbind(dem_lat_studyarea_mean,
#         dem_lat_peru_mean) %>%
#   ggplot() +
#   geom_line(aes(x = lat_seq, y = value, group = data, colour = data, size = data)) +
#   scale_colour_manual(values = c("gray60", "black")) +
#   scale_size_manual(values = c(.5, .75)) + 
#   geom_ribbon(data = dem_lat_peru_sd,
#               aes(x = lat_seq, ymin=value_min, ymax=value_max), fill="gray", alpha=.4) + 
#   geom_point(data = qc01$xyz, aes(x = LON, y = ALT), 
#              shape = 19, size = .25, color = "gray15", alpha = .5) + 
#   geom_point(data = qc_data$xyz[qc_data$xyz$filter_qc != 0, ], 
#              aes(x = LON, y = ALT), 
#              shape = 19, size = .25, color = "black") + 
#   scale_x_continuous(expand = c(0, 0), position = "top") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 5500/1000)) +
#   labs(y = "Elevation (km)", x = "Latitude (°)") + 
#   theme(axis.title = element_text(size = 8.5),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         # axis.text.y = element_text(size = 9),
#         # axis.title.y = element_blank(),
#         legend.justification = c(0, 0),
#         legend.position = "none",
#         legend.background = element_blank())
# 
# ## p2
# 
# dem_lat_studyarea_mean <- raster::zonal(x = dem, 
#                                         z = raster::init(dem, v='y'), 
#                                         fun = "mean", 
#                                         digits = 3) %>%
#   data.frame() %>%
#   transform(data = "Study Area") %>%
#   setNames(c("lat_seq", "value", "data"))
# 
# dem_lat_peru_mean <- raster::zonal(x = raster::mask(dem, shp_peru), 
#                                    z = raster::init(raster::mask(dem, shp_peru), v='y'), 
#                                    fun = "mean", 
#                                    digits = 3)%>%
#   data.frame() %>%
#   transform(data = "Peru") %>%
#   setNames(c("lat_seq", "value", "data"))
# 
# dem_lat_peru_sd <- raster::zonal(x = raster::mask(dem, shp_peru), 
#                                  z = raster::init(raster::mask(dem, shp_peru), v='y'), 
#                                  fun = "sd", 
#                                  digits = 3) %>%
#   data.frame() %>%
#   setNames(c("lat_seq", "value")) %>%
#   transform(value_max = (dem_lat_peru_mean$value + value) %>% {ifelse(. >= 0, ., 0)},
#             value_min = (dem_lat_peru_mean$value - value) %>% {ifelse(. >= 0, ., 0)})
# 
# 
# p2 <-
#   rbind(dem_lat_studyarea_mean,
#         dem_lat_peru_mean) %>%
#   ggplot() +
#   geom_line(aes(x = lat_seq, y = value, group = data, colour = data, size = data)) +
#   scale_colour_manual("", values = c("gray60", "black")) +
#   scale_size_manual("", values = c(.5, .75)) + 
#   geom_ribbon(data = dem_lat_peru_sd,
#               aes(x = lat_seq, ymin=value_min, ymax=value_max), fill="grey", alpha=.4) + 
#   geom_point(data = qc01$xyz, aes(x = LAT, y = ALT), 
#              shape = 19, size = .25, color = "gray15", alpha = .5) + 
#   geom_point(data = qc_data$xyz[qc_data$xyz$filter_qc != 0, ], 
#              aes(x = LAT, y = ALT), 
#              shape = 19, size = .25, color = "black") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 5500/1000), position = "right") +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = "Elevation (km)", x = "Latitude (°)") + 
#   coord_flip() +
#   theme(axis.title = element_text(size = 8.5),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         legend.justification = c(0, 0), 
#         legend.position = c(.175, .8),
#         legend.background = element_blank(),
#         legend.text=element_text(size=6))


## p3

dem <- as.data.frame(dem, xy = TRUE) %>%
  .[complete.cases(.), ]
shp_peru <- broom::tidy(shp_peru)

qc_points <- rbind(data.frame(qc01$xyz[, colnames(qc01$xyz)], Stations = "Raw"),
                   data.frame(qc_data$xyz[, colnames(qc01$xyz)], Stations = "To interpolate"))

df_countries <- data.frame(LON = c(-78, -72, -70, -67.4),
                           LAT = c(-1,   0,  -7, -15),
                           label = c("Ecuador", "Colombia", "Brazil", "Bolivia"))
df_chile <- data.frame(LON = c(-69.6),
                       LAT = c(-18.4),
                       label = c("Chile"))

p3 <- ggplot() + 
  geom_raster(data = dem,
            aes(x = x, y = y, fill = DEM), alpha = .8) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$dead_reef)(10) %>% rev(),
                       na.value= "lightblue",
                       " Elevation (km)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              barheight = 5,
                                              title.theme = element_text(size = 11,
                                                                         angle = 90,
                                                                         vjust = 0.5))) +
  geom_polygon(data = shp_sa,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.5) +
  geom_polygon(data = shp_lakes, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "skyblue", size = 0.3) +
  # geom_polygon(data = shp_peru,
  #              aes(x = long, y = lat, group = group),
  #              fill = NA, colour = "gray20", size = 0.3) +
  geom_point(data = qc_points,
             aes(x = LON, y = LAT, shape = Stations, size = Stations), colour = "gray15", alpha = .5) +
  scale_shape_manual(values = c(3, 20),
                     ) + 
  scale_size_manual(values = c(1, 3)) + 
  # scale_x_continuous(position = "top") + # to be used with the other subplots
  # geom_label_repel(data = df_countries, aes(x = LON, y = LAT, label = label),
  #                 fill = "white", size = 2,
  #                 box.padding = 0.01, alpha = .5) +  
  # geom_label_repel(data = df_chile, aes(x = LON, y = LAT, label = label),
  #                  fill = "white", size = 2,
  #                  box.padding = 0.01, alpha = .5,
  #                  min.segment.length = unit(0, 'lines'),
  #                  nudge_x = 1, nudge_y = 1) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-81.325, -67.175)) + 
  #coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175)) + 
  #coord_quickmap(expand = F, ylim = c(-18.5, -15), xlim = c(-77, -72)) + 
  labs(x = "", y = "") +
  #theme_linedraw() + 
  theme_bw() + 
  geom_rect(aes(xmin = -73.5, xmax = -68, ymin = -18.5, ymax = -12),
            fill = "transparent", color = "red", size = .75) +
  theme(axis.title = element_blank(),
        #axis.title.x = element_text(size = 15),
        # axis.text.x = element_blank(),
        # axis.title.y = element_text(size = 15),
        #axis.text.y = element_blank(),
        legend.box = 'vertical',
        legend.justification = c(0, 0), legend.position = c(0, 0),
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"))
## p4


# p4 <- 
#   qc01$xyz %>%
#   transform(ALT = ifelse(ALT < 0, NA, ALT)) %>%
#   ggplot(aes(x = ALT)) +
#   geom_histogram(aes(y = stat(count) * 100 / sum(count)), bins = 20) + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 18)) + 
#   labs(y = "Relative frequency (%)", x = "Elevation (km)") +
#   theme(axis.title = element_text(size = 6.65),
#         # axis.text.x = element_blank(),
#         # axis.text.y = element_blank(),
#         legend.justification = c(0, 0), legend.position = c(.15, .15),
#         legend.background = element_blank())


## merging plots
# ggdraw() +
#   draw_plot(p3, x = 0, y = .2, width = .65, height = .8) + 
#   draw_plot(p1, x = 0.12, y = .0375, width = .4125, height = .165) + 
#   draw_plot(p2 , x = .53, y = .2, width = .125, height = .8) + 
#   draw_plot(p4 , x = .53, y = .0375, width = .125, height = .165) + 
#   ggsave(
#     file.path(".", "paper", "output", "Fig_study_area_stations.jpg"),
#     dpi = 250, width = 7.25, scale = 1.35)

worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  geom_polygon(data = world.df[world.df$group == "Peru.1",], aes(x = long, y = lat, group = group), fill = "red", colour = "red") +
  coord_map("ortho", orientation=c(-24.35, -70, 0))
worldmap <- worldmap + theme_bw() +  theme(axis.title=element_blank(),
                               axis.text=element_blank(),
                               axis.ticks=element_blank(),
                               panel.grid.major = element_line(colour = "gray70"),
                               plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  theme(
    panel.background = element_rect(fill = "white"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

p4 <- ggplot() + 
  # geom_raster(data = raster::as.data.frame(dem_area, xy = TRUE) %>%
  #               .[complete.cases(.),] %>% subset(DEM >= 3000),
  #             aes(x = x, y = y), fill = "black", alpha = .5) +
  geom_polygon(data = shp_sa,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray40", size = 0.8) +
  # geom_polygon(data = shp_peru,
  #              aes(x = long, y = lat, group = group),
  #              fill = NA, colour = "gray40", size = 0.5) +
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group, fill = id2),
               colour = "gray50", size = 0.25, alpha = .4) +
  scale_fill_manual(values = c("#4682B4", "#B47846", "#4BB446")) + 
  # scale_fill_gradientn(colors = colorRampPalette(rev(ochRe::ochre_palettes$olsen_seq))(10)[4:10],
  #                      na.value= "lightblue",
  #                      "             Elevation (km)",
  #                      guide = guide_colorbar(frame.colour = "black",
  #                                             ticks.colour = "black",
  #                                             title.position = "left",
  #                                             barheight = 8,
  #                                             title.theme = element_text(size = 9,
  #                                                                        angle = 90,
  #                                                                        vjust = 0.5))) +
  geom_label_repel(data = df_countries, aes(x = LON, y = LAT, label = label),
                   fill = "white", size = 2,
                   box.padding = 0.01, alpha = .5) +  
  geom_label_repel(data = df_chile, aes(x = LON, y = LAT, label = label),
                   fill = "white", size = 2,
                   box.padding = 0.01, alpha = .5,
                   min.segment.length = unit(0, 'lines'),
                   nudge_x = 1, nudge_y = 1) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.265), xlim = c(-81.325, -67.175)) + 
  #coord_quickmap(expand = F, ylim = c(-18.5, -15), xlim = c(-77, -72)) + 
  labs(x = "", y = "") +
  #theme_linedraw() + 
  geom_polygon(data = shp_lakes,
               aes(x = long, y = lat, group = group),
               fill = "lightblue2", colour = "lightblue2", size = 0.5) +
  guides(fill=guide_legend(title="Main regions")) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        #axis.title.x = element_text(size = 15),
        # axis.text.x = element_blank(),
        # axis.title.y = element_text(size = 15),
        #axis.text.y = element_blank(),
        legend.box = 'vertical',
        legend.justification = c(0, 0), legend.position = c(0, 0),
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"))

library(egg)

p4 +
  annotation_custom(
    ggplotGrob(worldmap),
    xmin = -67.175, xmax = -70.75, ymin = -5, ymax = 4
  ) -> p4

cowplot::plot_grid(p4, p3, ncol = 2)

ggsave(file.path(".", "paper", "output", "Figure_02_study_area_stations.pdf"),
       device = "pdf",
       dpi = 500, scale = 1,
       width = 8, height = 6, units = "in")
