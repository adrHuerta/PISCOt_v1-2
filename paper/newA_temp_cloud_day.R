library(ggplot2)
library(raster)
"%>%" = magrittr::`%>%`

shp_peru = file.path(".", "data", "raw", "vectorial", "SEC_CLIM.shp") %>%
  shapefile()
shp_peru@data$MAIREG = transform(shp_peru@data, MAIREG = ifelse(MAC_REG == "CO", "CO", ifelse(MAC_REG == "SEA", "SE", ifelse(MAC_REG == "SEB", "SE", ifelse(MAC_REG == "SIOC", "AN", "AN")))))$MAIREG
shp_peru <- shp_peru %>% raster::aggregate(., by = 'MAIREG') %>%  
  broom::tidy(region = "MAIREG") %>%
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

ggplot(data = data_sc_sfc,
       aes(x=values, y=elevation)) +
  geom_bin2d(bins = 30) +
  scale_fill_continuous(type = "viridis", breaks = c(1, 2000, 4000, 6000),
                        "Density (count/100)",
                        labels = function(x){x / 100},
                        guide = guide_colorbar(frame.colour = "black",
                                               ticks.colour = "black",
                                               title.position = "left")) +
  scale_y_continuous(breaks = seq(from = 0, to = 6.5, by = 0.5)) + 
  ylab("Elevation (km asl)") + xlab("Air temperature (°C)")  +
  facet_wrap(~data, scales = "free") +
  theme_bw() + 
  theme(legend.position = c(0.05, 0.275),
        legend.background = element_blank(),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        strip.text =  element_blank(),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 8, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 7),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> phist

ggplot(data = subset(data_sc_sfc_nc, data == "Clear day"),
       aes(x=longitude, y=latitude, fill = values)) + 
  geom_raster() + 
  facet_grid(~data,, scales = "free") +
  scale_fill_gradientn("      Tmean (°C) ",
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
        strip.text = element_text(size = 10),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 8, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 7),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> p_cd


ggplot(data = subset(data_sc_sfc_nc, data == "Fog-Covered day"),
       aes(x=longitude, y=latitude, fill = values)) + 
  geom_raster() + 
  geom_raster() + 
  facet_grid(~data,, scales = "free") +
  scale_fill_gradientn("      Tmean (°C) ",
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
        strip.text = element_text(size = 10),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 8, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 7),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> p_fcd

ggplot(data = subset(data_sc_sfc_nc, data == "Clear day - Fog-Covered day"),
       aes(x=longitude, y=latitude, fill = values)) + 
  geom_raster() + 
  geom_raster() + 
  facet_grid(~data,, scales = "free") +
  scale_fill_gradient2("   Diff. Tmean (°C) ",
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
        strip.text = element_text(size = 10),
        strip.text.y = element_blank(),
        legend.title = element_text(size = 8, angle = 90),
        legend.margin=margin(t=0),
        legend.text = element_text(size = 7),
        legend.key.height =  unit(.5, 'cm'),
        legend.key.width = unit(.4, 'cm')) -> p_cfcd


library(patchwork)
final_plot <- (p_fcd + p_cd + p_cfcd ) / phist

ggsave(file.path(".", "paper", "output", "newA_Figure_temp_cloud_day.pdf"),
       final_plot,
       device = "pdf",
       dpi = 600, scale = 1,
       width = 8, height = 6, units = "in")
