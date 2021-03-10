rm(list = ls())
"%>%" = magrittr::`%>%`

library(raster)
library(ggplot2)
library(cowplot)

theme_set(theme_bw() + theme(text = element_text(family = 'helvetica')))

###

dem = file.path(".", "data", "processed", "gridded", "DEM.nc") %>% 
  raster() %>%
  {./1000}

shp_peru = file.path(".", "data", "raw", "vectorial", "Departamentos.shp") %>% 
  shapefile() 

shp_sa = file.path(".", "data", "raw", "vectorial", "Sudamérica.shp") %>% 
  shapefile() %>%
  broom::tidy()

# qc 01

qc01 <- file.path(".", "data", "processed", "obs", "qc_output", "RAW(QC01)_data.RDS") %>% 
  readRDS()

# qc data

qc_data <- file.path(".", "data", "processed", "obs", "qc_output", "QC_data.RDS") %>% 
  readRDS()

#### 
# spatial variability

qc01$xyz$ALT <- qc01$xyz$ALT / 1000
qc_data$xyz$ALT <- qc_data$xyz$ALT / 1000

## p1 

dem_lat_studyarea_mean <- raster::zonal(x = dem, 
                                        z = raster::init(dem, v='x'), 
                                        fun = "mean", 
                                        digits = 3) %>%
  data.frame() %>%
  transform(data = "Study Area") %>%
  setNames(c("lat_seq", "value", "data"))

dem_lat_peru_mean <- raster::zonal(x = raster::mask(dem, shp_peru), 
                                   z = raster::init(raster::mask(dem, shp_peru), v='x'), 
                                   fun = "mean", 
                                   digits = 3)%>%
  data.frame() %>%
  transform(data = "Peru") %>%
  setNames(c("lat_seq", "value", "data"))

dem_lat_peru_sd <- raster::zonal(x = raster::mask(dem, shp_peru), 
                                 z = raster::init(raster::mask(dem, shp_peru), v='x'), 
                                 fun = "sd", 
                                 digits = 3) %>%
  data.frame() %>%
  setNames(c("lat_seq", "value")) %>%
  transform(value_max = (dem_lat_peru_mean$value + value) %>% {ifelse(. >= 0, ., 0)},
            value_min = (dem_lat_peru_mean$value - value) %>% {ifelse(. >= 0, ., 0)})


p1 <-
  rbind(dem_lat_studyarea_mean,
        dem_lat_peru_mean) %>%
  ggplot() +
  geom_line(aes(x = lat_seq, y = value, group = data, colour = data, size = data)) +
  scale_colour_manual(values = c("gray60", "black")) +
  scale_size_manual(values = c(.5, .75)) + 
  geom_ribbon(data = dem_lat_peru_sd,
              aes(x = lat_seq, ymin=value_min, ymax=value_max), fill="gray", alpha=.4) + 
  geom_point(data = qc01$xyz, aes(x = LON, y = ALT), 
             shape = 19, size = .25, color = "gray15", alpha = .5) + 
  geom_point(data = qc_data$xyz[qc_data$xyz$filter_qc != 0, ], 
             aes(x = LON, y = ALT), 
             shape = 19, size = .25, color = "black") + 
  scale_x_continuous(expand = c(0, 0), position = "top") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5500/1000)) +
  labs(y = "Elevation (km)", x = "Latitude (°)") + 
  theme(axis.title = element_text(size = 8.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.y = element_text(size = 9),
        # axis.title.y = element_blank(),
        legend.justification = c(0, 0),
        legend.position = "none",
        legend.background = element_blank())

## p2

dem_lat_studyarea_mean <- raster::zonal(x = dem, 
                                        z = raster::init(dem, v='y'), 
                                        fun = "mean", 
                                        digits = 3) %>%
  data.frame() %>%
  transform(data = "Study Area") %>%
  setNames(c("lat_seq", "value", "data"))

dem_lat_peru_mean <- raster::zonal(x = raster::mask(dem, shp_peru), 
                                   z = raster::init(raster::mask(dem, shp_peru), v='y'), 
                                   fun = "mean", 
                                   digits = 3)%>%
  data.frame() %>%
  transform(data = "Peru") %>%
  setNames(c("lat_seq", "value", "data"))

dem_lat_peru_sd <- raster::zonal(x = raster::mask(dem, shp_peru), 
                                 z = raster::init(raster::mask(dem, shp_peru), v='y'), 
                                 fun = "sd", 
                                 digits = 3) %>%
  data.frame() %>%
  setNames(c("lat_seq", "value")) %>%
  transform(value_max = (dem_lat_peru_mean$value + value) %>% {ifelse(. >= 0, ., 0)},
            value_min = (dem_lat_peru_mean$value - value) %>% {ifelse(. >= 0, ., 0)})


p2 <-
  rbind(dem_lat_studyarea_mean,
        dem_lat_peru_mean) %>%
  ggplot() +
  geom_line(aes(x = lat_seq, y = value, group = data, colour = data, size = data)) +
  scale_colour_manual("", values = c("gray60", "black")) +
  scale_size_manual("", values = c(.5, .75)) + 
  geom_ribbon(data = dem_lat_peru_sd,
              aes(x = lat_seq, ymin=value_min, ymax=value_max), fill="grey", alpha=.4) + 
  geom_point(data = qc01$xyz, aes(x = LAT, y = ALT), 
             shape = 19, size = .25, color = "gray15", alpha = .5) + 
  geom_point(data = qc_data$xyz[qc_data$xyz$filter_qc != 0, ], 
             aes(x = LAT, y = ALT), 
             shape = 19, size = .25, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5500/1000), position = "right") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Elevation (km)", x = "Latitude (°)") + 
  coord_flip() +
  theme(axis.title = element_text(size = 8.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.justification = c(0, 0), 
        legend.position = c(.175, .8),
        legend.background = element_blank(),
        legend.text=element_text(size=6))


## p3

dem <- as.data.frame(dem, xy = TRUE) %>%
  .[complete.cases(.), ]
shp_peru <- broom::tidy(shp_peru)

p3 <- ggplot() + 
  geom_raster(data = dem,
            aes(x = x, y = y, fill = DEM)) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$dead_reef)(10) %>% rev(),
                       na.value= "lightblue",
                       " Elevation (km)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              title.theme = element_text(angle = 90,
                                                                         vjust = 0.5))) +
  geom_polygon(data = shp_sa,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "gray20", size = 0.3) +
  geom_point(data = qc01$xyz,
             aes(x = LON, y = LAT), colour = "gray15", size = .75, alpha = .5, shape = 19) +
  geom_point(data = qc_data$xyz[qc_data$xyz$filter_qc != 0, ],
             aes(x = LON, y = LAT), colour = "black", size = .75, shape = 19) +
  scale_colour_discrete("Stations",
                        guide = guide_colorbar(frame.colour = "black",
                                               ticks.colour = "black",
                                               #title.position = "left",
                                               title.theme = element_text(angle = 0,
                                                                          vjust = 0.5))) +
  # scale_x_continuous(position = "top") + # to be used with the other subplots
  coord_quickmap(expand = c(0, 0), ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175)) + 
  #coord_quickmap(expand = F, ylim = c(-18.5, -15), xlim = c(-77, -72)) + 
  labs(x = "Longitude (°)", y = "Latitude (°)") +
  #theme_linedraw() + 
  theme(axis.title = element_text(size = 8.5),
        # axis.title.x = element_text(size = 15),
        # axis.text.x = element_blank(),
        # axis.title.y = element_text(size = 15),
        axis.text.y = element_text(angle = 90),
        legend.justification = c(0, 0), legend.position = c(0, 0),
        legend.background = element_blank())


## p4


p4 <- 
  qc01$xyz %>%
  transform(ALT = ifelse(ALT < 0, NA, ALT)) %>%
  ggplot(aes(x = ALT)) +
  geom_histogram(aes(y = stat(count) * 100 / sum(count)), bins = 20) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18)) + 
  labs(y = "Relative frequency (%)", x = "Elevation (km)") +
  theme(axis.title = element_text(size = 6.65),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        legend.justification = c(0, 0), legend.position = c(.15, .15),
        legend.background = element_blank())


## merging plots
# 9.79 x 6.99
# ggdraw() +
#   draw_plot(p3, x = 0, y = .2, width = .65, height = .8) + 
#   draw_plot(p1, x = 0.12, y = .0375, width = .4125, height = .165) + 
#   draw_plot(p2 , x = .53, y = .2, width = .125, height = .8) + 
#   draw_plot(p4 , x = .53, y = .0375, width = .125, height = .165) + 
#   ggsave(
#     file.path(".", "paper", "output", "Fig_study_area_stations.jpg"),
#     dpi = 250, width = 7.25, scale = 1.35)

# Saving 9.06 x 5.46 in image
p3
ggsave(file.path(".", "paper", "output", "Fig_study_area_stations.jpg"),
       dpi = 250, scale = 1)
