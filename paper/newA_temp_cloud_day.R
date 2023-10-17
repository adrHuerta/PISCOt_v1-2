library(ggplot2)
library(raster)

"%>%" = magrittr::`%>%`

shp_peru = file.path(".", "data", "raw", "vectorial", "SEC_CLIM.shp") %>%
  shapefile()
shp_peru@data$MAIREG = transform(shp_peru@data, MAIREG = ifelse(MAC_REG == "CO", "CO", ifelse(MAC_REG == "SEA", "SE", ifelse(MAC_REG == "SEB", "SE", ifelse(MAC_REG == "SIOC", "AN", "AN")))))$MAIREG
shp_peru <- shp_peru %>% raster::aggregate(., by = 'MAIREG') %>%  
  ggplot2::fortify(shp_peru) %>%
  transform(., id2 = ifelse(id == "CO", "PC", ifelse(id == "SE", "AZ", "AN")))
shp_peru$id2 <- factor(shp_peru$id2, 
                       levels = c("PC", "AN", "AZ"), 
                       labels = c("Pacific Coast", "Andes", "Amazon"))

data_sc_sfc = read.csv("paper/others/temp_cloud_day/sc_sfc_df.csv", header = TRUE)
data_sc_sfc$data = factor(data_sc_sfc$data, 
                          levels = c("Fog-Covered day", "Clear day", "Clear day - Fog-Covered day")) 
data_sc_sfc$elevation = data_sc_sfc$elevation/1000

data_sc_sfc_nc = read.csv("paper/others/temp_cloud_day/sc_sfc_df_nc.csv", header = TRUE)
data_sc_sfc_nc$data = factor(data_sc_sfc_nc$data, 
                             levels = c("Fog-Covered day", "Clear day", "Clear day - Fog-Covered day")) 

band_1_clear_day = raster::raster("paper/others/temp_cloud_day/band1_2006-08-24_area.tif") * 0.0001
band_1_fog_covered_day = raster::raster("paper/others/temp_cloud_day/band1_2007_08_25_area.tif") * 0.0001
band_1_difference = band_1_clear_day - band_1_fog_covered_day

band_1_clear_day = as.data.frame(band_1_clear_day, xy = TRUE)
band_1_clear_day = band_1_clear_day[complete.cases(band_1_clear_day), ]
band_1_fog_covered_day = as.data.frame(band_1_fog_covered_day, xy = TRUE)
band_1_fog_covered_day = band_1_fog_covered_day[complete.cases(band_1_fog_covered_day), ]
band_1_difference = as.data.frame(band_1_difference, xy = TRUE)
band_1_difference = band_1_difference[complete.cases(band_1_difference), ]


ggplot(data = band_1_clear_day,
       aes(x = x, y = y, fill = band1_2006.08.24_area)) + 
  geom_raster() + 
  scale_fill_distiller(type = "seq",
                       direction = -1,
                       "  Sref (unitless)",
                       palette = "Greys",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left")) + 
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "red", size = 0.35) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.25),
        legend.box.background = element_rect(color = "black", size = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 10, angle = 90),
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 9),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm'),
        plot.title = element_text(hjust = 0.5))  +
  coord_quickmap(expand = c(0, 0), ylim = c(-10, -3.4), xlim = c(-82, -75.5)) + 
  ggtitle("Cloud-Free day") -> cc_clear_day


ggplot(data = band_1_fog_covered_day,
       aes(x = x, y = y, fill = band1_2007_08_25_area)) + 
  geom_raster() + 
  scale_fill_distiller(type = "seq",
                       direction = -1,
                       "  Sref (unitless)",
                       palette = "Greys",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left")) + 
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "red", size = 0.35) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.25),
        legend.box.background = element_rect(color = "black", size = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 10, angle = 90),
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 9),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm'),
        plot.title = element_text(hjust = 0.5))  +
  coord_quickmap(expand = c(0, 0), ylim = c(-10, -3.4), xlim = c(-82, -75.5))  + 
  ggtitle("Fog-Covered day") -> cc_fog_covered_day


ggplot(data = band_1_difference,
       aes(x = x, y = y, fill = layer)) + 
  geom_raster() +
  scale_fill_gradient2("Diff. Sref (unitless)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left")) +
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.35) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.25),
        legend.box.background = element_rect(color = "black", size = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 9, angle = 90),
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 9),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm'),
        plot.title = element_text(hjust = 0.5)) +
  coord_quickmap(expand = c(0, 0), ylim = c(-10, -3.4), xlim = c(-82, -75.5)) + 
  ggtitle("Cloud-Free day - Fog-Covered day") -> cc_clear_day_minus_fog_covered_day

ggplot(data = data_sc_sfc,
       aes(x=values, y=elevation)) +
  geom_bin2d(bins = 12, drop = 5000) +
  scale_fill_continuous(type = "viridis", direction = -1,
                        " Number of grid cells",
                        guide = guide_colorbar(frame.colour = "black",
                                               ticks.colour = "black",
                                               title.position = "left",
                                               barheight = 6)) +
  scale_y_continuous(breaks = seq(from = 0, to = 6.5, by = 0.5)) + 
  ylab("Elevation (km asl)") + xlab("Air temperature (°C)")  +
  facet_wrap(~data, scales = "free") +
  theme_bw() + 
  theme(legend.position = c(0.05, 0.225),
        legend.background = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text =  element_blank(),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 10, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 9),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> phist

ggplot(data = subset(data_sc_sfc_nc, data == "Clear day"),
       aes(x=longitude, y=latitude, fill = values)) + 
  geom_raster() + 
  facet_grid(~data, scales = "free") +
  scale_fill_gradientn("    Tmean (°C) ",
                       colors = colorRampPalette(ochRe::ochre_palettes$olsen_seq)(15) %>% rev(),
                       limits = c(-1.5, 27),
                       breaks = c(0, 5, 10, 15, 20, 25),
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left")) + 
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.2) +
  coord_quickmap(expand = c(0, 0), ylim = c(-10, -3.4), xlim = c(-82, -75.5)) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.25),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.title = element_text(size = 10, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 9),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> p_cd


ggplot(data = subset(data_sc_sfc_nc, data == "Fog-Covered day"),
       aes(x=longitude, y=latitude, fill = values)) + 
  geom_raster() + 
  geom_raster() + 
  facet_grid(~data,, scales = "free") +
  scale_fill_gradientn("    Tmean (°C) ",
                       colors = colorRampPalette(ochRe::ochre_palettes$olsen_seq)(15) %>% rev(),
                       limits = c(-1.5, 27),
                       breaks = c(0, 5, 10, 15, 20, 25),
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left")) + 
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.2) +
  coord_quickmap(expand = c(0, 0), ylim = c(-10, -3.4), xlim = c(-82, -75.5)) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.25),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.title = element_text(size = 10, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 9),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> p_fcd

ggplot(data = subset(data_sc_sfc_nc, data == "Clear day - Fog-Covered day"),
       aes(x=longitude, y=latitude, fill = values)) + 
  geom_raster() + 
  geom_raster() + 
  facet_grid(~data,, scales = "free") +
  scale_fill_gradient2("    Diff. Tmean (°C) ",
                       limits = c(-2.5, 6),
                       breaks = c(-2, -1, 0, 1, 2, 4, 6),
                       midpoint = 0,
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left")) + 
  geom_polygon(data = shp_peru,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.2) +
  coord_quickmap(expand = c(0, 0), ylim = c(-10, -3.4), xlim = c(-82, -75.5)) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.25),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.title = element_text(size = 10, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 9),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> p_cfcd


library(patchwork)
final_plot <- (cc_fog_covered_day + cc_clear_day + cc_clear_day_minus_fog_covered_day ) / (p_fcd + p_cd + p_cfcd ) / phist

ggsave(file.path(".", "paper", "output", "newA_Figure_temp_cloud_day.pdf"),
       final_plot,
       device = "pdf",
       dpi = 600, scale = .9,
       width = 12, height = 12, units = "in")
