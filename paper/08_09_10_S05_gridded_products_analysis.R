rm(list = ls())
"%>%" = magrittr::`%>%`

library(ggplot2)

# 
shp_lakes = file.path(".", "data", "raw", "vectorial", "Lagos_lagunas_Project.shp") %>% 
  raster::shapefile() %>% 
  .[.@data$Rasgo_Secu == "Perenne", ] %>%
  .[.@data$are > 1000, ] 

shp_sa <- file.path(".", "data", "raw", "vectorial", "Sudamérica.shp") %>% 
  raster::shapefile()

shp_sa_gg <- shp_sa %>% broom::tidy()

###### mean annual mean #######

grid_files <- list.files(file.path(".", "paper", "others", "global_gridded_products"),
                              pattern = "mean_*", full.names = TRUE) 

grid_df_files <- grid_files %>%
  setNames(., .) %>%
  lapply(function(x){
    
    rs <- raster::raster(x)
    names(rs) <- strsplit(x, "/")[[1]][5]
    rs
    
  })

grid_df_files[[15]] <- raster::t(raster::flip(grid_df_files[[15]], direction = 2))
names(grid_df_files[[15]]) <- strsplit(grid_files[15], "/")[[1]][5]

grid_df_files[[16]] <- raster::t(raster::flip(grid_df_files[[16]], direction = 2))
names(grid_df_files[[16]]) <- strsplit(grid_files[16], "/")[[1]][5]


grid_df_files <- 
  grid_df_files %>%
  lapply(function(x){
    rs <- raster::mask(raster::mask(x, shp_lakes[1,], inverse = TRUE), shp_sa)
    rs <- raster::as.data.frame(rs, xy = TRUE)
    rs$Product <- strsplit(colnames(rs)[3], "_")[[1]][2]
    rs$Variable <- strsplit(strsplit(colnames(rs)[3], "_")[[1]][3], ".nc")[[1]][1]
    colnames(rs) <- c("x", "y", "value", "Product", "Variable")
    rs[complete.cases(rs), ]
    
  }) %>% do.call("rbind", .)

grid_df_files <- rbind(grid_df_files, 
                       data.frame(x = c(-68.62500, -68.62500), 
                                  y = c(-13.825, -13.825),
                                  value = c(NA, NA), 
                                  Product = c("VS2018", "TerraClimate"), 
                                  Variable = c("FD", "FD")))
rownames(grid_df_files) <- NULL
grid_df_files$Product <- factor(grid_df_files$Product, 
                                level = c("PISCOt.v1.2", "PISCOt.v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5.Land"),
                                labels = c("PISCOt v1.2", "PISCOt v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5-Land"))



library(ggplot2)

ggplot() + 
  geom_raster(data = grid_df_files[grid_df_files$Variable == "MTmax",], aes(x = x, y = y, fill = value)) +
  facet_grid(Variable~Product) + 
  geom_polygon(data = shp_sa, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.5) +
  geom_polygon(data = shp_lakes, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "black", size = 0.5) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$olsen_seq)(15) %>% rev(),
                       limits = c(5, 35),
                       "     MTmax (°C)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              barwidth = 0.5, barheight = 4,
                                              label.theme = element_text(size = 5),
                                              title.theme = element_text(size = 7,
                                                                         angle = 90,
                                                                         vjust = 2))) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.5, -12), xlim = c(-73.5, -68)) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.box = 'vertical',
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9),
        strip.text.y = element_blank(),
        legend.spacing.x = unit(.01, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        legend.margin=margin(t=0)) + 
    labs(x = "", y = "") -> mean_MTmax

ggsave(file.path(".", "paper", "output", "Figure_08_mean_MTmax.pdf"),
       device = "pdf",
       dpi = 300, scale = .75,
       width = 10, height = 6, units = "in")


ggplot() + 
  geom_raster(data = grid_df_files[grid_df_files$Variable == "MTmin",], aes(x = x, y = y, fill = value)) +
  facet_grid(Variable~Product) + 
  geom_polygon(data = shp_sa, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.5) +
  geom_polygon(data = shp_lakes, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "black", size = 0.5) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$olsen_seq)(15) %>% rev(),
                       limits = c(-5, 20),
                       "     MTmin (°C)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              barwidth = 0.5, barheight = 4,
                                              label.theme = element_text(size = 5),
                                              title.theme = element_text(size = 7,
                                                                         angle = 90,
                                                                         vjust = 2))) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.5, -12), xlim = c(-73.5, -68)) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.box = 'vertical',
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.spacing.x = unit(.01, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        legend.margin=margin(t=0)) + 
  labs(x = "", y = "") -> mean_MTmin

ggsave(file.path(".", "paper", "output", "Figure_08_mean_MTmin.pdf"),
       device = "pdf",
       dpi = 300, scale = .75,
       width = 10, height = 6, units = "in")

ggplot() + 
  geom_raster(data = grid_df_files[grid_df_files$Variable == "FD",], aes(x = x, y = y, fill = value)) +
  facet_grid(Variable~Product) + 
  geom_polygon(data = shp_sa, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.5) +
  geom_polygon(data = shp_lakes, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "black", size = 0.5) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$williams_pilbara)(15),
                       limits = c(0, 100),
                       "         FD (%)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              barwidth = 0.5, barheight = 4,
                                              label.theme = element_text(size = 4),
                                              title.theme = element_text(size = 6,
                                                                         angle = 90,
                                                                         vjust = 2))) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.5, -12), xlim = c(-73.5, -68)) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.box = 'vertical',
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.spacing.x = unit(.01, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        legend.margin=margin(t=0)) + 
  labs(x = "", y = "") -> mean_FD

library(grid)

g = ggplotGrob(mean_FD)
g$layout
pos <- grepl(pattern = c("panel-1-3"), g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
pos <- grepl(pattern = c("panel-1-4"), g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()


ggsave(file.path(".", "paper", "output", "Figure_08_mean_FD.pdf"),
       plot = grid.draw(g),
       device = "pdf",
       dpi = 300, scale = .75,
       width = 10, height = 6, units = "in")


###### window moving trend ###### 

dem = file.path(".", "data", "processed", "gridded", "co_variables", "DEM.nc") %>% 
  raster::raster()

# PISCOt v1.1

trend_PISCOtv11 <- list.files(file.path(".", "paper", "others", "global_gridded_products", "PISCOt_v1.1"),
                              full.names = TRUE) %>%
  setNames(., .) %>%
  lapply(function(x){
    
    
    rs <- raster::brick(x)
    rs <- raster::mask(raster::mask(rs, shp_lakes[1,], inverse = TRUE), shp_sa)
    dem_rs <- raster::crop(raster::resample(dem, rs), rs)
    dem_rs[dem_rs < 2000] <- NA; dem_rs[dem_rs >= 2000] <- 1  
    rs <- raster::cellStats(rs*dem_rs, mean)
    rs_mk <- zoo::rollapply(rs, 10, function(y){trend::mk.test(y)$p.value}, fill = NA)
    rs_sen <- zoo::rollapply(rs, 10, function(y){trend::sens.slope(y)$estimates}, fill = NA)
    
    trs <- data.frame(sen_value = as.numeric(rs_sen),
                      mk_pvalue = as.numeric(rs_mk),
                      Product = strsplit(x, "/")[[1]][5],
                      Variable = strsplit(strsplit(x, "/")[[1]][6], ".nc")[[1]][1],
                      Year = as.numeric(substr(names(raster::brick(x)), 2, 5)))
    
    trs

  }) %>% do.call(rbind, .)

# PISCOt v1.2

trend_PISCOtv12 <- list.files(file.path(".", "paper", "others", "global_gridded_products", "PISCOt_v1.2"),
                              full.names = TRUE) %>%
  setNames(., .) %>%
  lapply(function(x){
    
    
    rs <- raster::brick(x)
    rs <- raster::mask(raster::mask(rs, shp_lakes[1,], inverse = TRUE), shp_sa)
    dem_rs <- raster::crop(raster::resample(dem, rs), rs)
    dem_rs[dem_rs < 2000] <- NA; dem_rs[dem_rs >= 2000] <- 1  
    rs <- raster::cellStats(rs*dem_rs, mean)
    rs_mk <- zoo::rollapply(rs, 10, function(y){trend::mk.test(y)$p.value}, fill = NA)
    rs_sen <- zoo::rollapply(rs, 10, function(y){trend::sens.slope(y)$estimates}, fill = NA)
    
    trs <- data.frame(sen_value = as.numeric(rs_sen),
                      mk_pvalue = as.numeric(rs_mk),
                      Product = strsplit(x, "/")[[1]][5],
                      Variable = strsplit(strsplit(x, "/")[[1]][6], ".nc")[[1]][1],
                      Year = as.numeric(substr(names(raster::brick(x)), 2, 5)))
    
    trs
    
  }) %>% do.call(rbind, .)

# CHIRTS

trend_CHIRTS <- list.files(file.path(".", "paper", "others", "global_gridded_products", "CHIRTS"),
                              full.names = TRUE) %>%
  setNames(., .) %>%
  lapply(function(x){
    
    
    rs <- raster::brick(x)
    rs <- raster::mask(raster::mask(rs, shp_lakes[1,], inverse = TRUE), shp_sa)
    dem_rs <- raster::crop(raster::resample(dem, rs), rs)
    dem_rs[dem_rs < 2000] <- NA; dem_rs[dem_rs >= 2000] <- 1  
    rs <- raster::cellStats(rs*dem_rs, mean)
    rs_mk <- zoo::rollapply(rs, 10, function(y){trend::mk.test(y)$p.value}, fill = NA)
    rs_sen <- zoo::rollapply(rs, 10, function(y){trend::sens.slope(y)$estimates}, fill = NA)
    
    trs <- data.frame(sen_value = as.numeric(rs_sen),
                      mk_pvalue = as.numeric(rs_mk),
                      Product = strsplit(x, "/")[[1]][5],
                      Variable = strsplit(strsplit(x, "/")[[1]][6], ".nc")[[1]][1],
                      Year = as.numeric(substr(names(raster::brick(x)), 2, 5)))
    
    trs
    
  }) %>% do.call(rbind, .)

# VS2018

trend_VS2018 <- list.files(file.path(".", "paper", "others", "global_gridded_products", "VS2018"),
                              full.names = TRUE) %>%
  setNames(., .) %>%
  lapply(function(x){
    
    
    rs <- raster::brick(x)
    rs <- raster::t(raster::flip(rs, direction = 2))
    rs <- raster::mask(raster::mask(rs, shp_lakes[1,], inverse = TRUE), shp_sa)
    dem_rs <- raster::crop(raster::resample(dem, rs), rs)
    dem_rs[dem_rs < 2000] <- NA; dem_rs[dem_rs >= 2000] <- 1  
    rs <- raster::cellStats(rs*dem_rs, mean)
    rs_mk <- zoo::rollapply(rs, 10, function(y){trend::mk.test(y)$p.value}, fill = NA)
    rs_sen <- zoo::rollapply(rs, 10, function(y){trend::sens.slope(y)$estimates}, fill = NA)
    
    trs <- data.frame(sen_value = as.numeric(rs_sen),
                      mk_pvalue = as.numeric(rs_mk),
                      Product = strsplit(x, "/")[[1]][5],
                      Variable = strsplit(strsplit(x, "/")[[1]][6], ".nc")[[1]][1],
                      Year = as.numeric(substr(names(raster::brick(x)), 2, 5)))
    
    trs
    
  }) %>% do.call(rbind, .)

# TerraClimate

trend_terraclimate <- list.files(file.path(".", "paper", "others", "global_gridded_products", "TerraClimate"),
                              full.names = TRUE) %>%
  setNames(., .) %>%
  lapply(function(x){
    
    
    rs <- raster::brick(x)
    rs <- raster::mask(raster::mask(rs, shp_lakes[1,], inverse = TRUE), shp_sa)
    dem_rs <- raster::crop(raster::resample(dem, rs), rs)
    dem_rs[dem_rs < 2000] <- NA; dem_rs[dem_rs >= 2000] <- 1  
    rs <- raster::cellStats(rs*dem_rs, mean)
    rs_mk <- zoo::rollapply(rs, 10, function(y){trend::mk.test(y)$p.value}, fill = NA)
    rs_sen <- zoo::rollapply(rs, 10, function(y){trend::sens.slope(y)$estimates}, fill = NA)
    
    trs <- data.frame(sen_value = as.numeric(rs_sen),
                      mk_pvalue = as.numeric(rs_mk),
                      Product = strsplit(x, "/")[[1]][5],
                      Variable = strsplit(strsplit(x, "/")[[1]][6], ".nc")[[1]][1],
                      Year = as.numeric(substr(names(raster::brick(x)), 2, 5)))
    
    trs
    
  }) %>% do.call(rbind, .)

# ERA5-Land

trend_ERA5land <- list.files(file.path(".", "paper", "others", "global_gridded_products", "ERA5_Land"),
                              full.names = TRUE) %>%
  setNames(., .) %>%
  lapply(function(x){
    
    
    rs <- raster::brick(x)
    rs <- raster::mask(raster::mask(rs, shp_lakes[1,], inverse = TRUE), shp_sa)
    dem_rs <- raster::crop(raster::resample(dem, rs), rs)
    dem_rs[dem_rs < 2000] <- NA; dem_rs[dem_rs >= 2000] <- 1  
    rs <- raster::cellStats(rs*dem_rs, mean)
    rs_mk <- zoo::rollapply(rs, 10, function(y){trend::mk.test(y)$p.value}, fill = NA)
    rs_sen <- zoo::rollapply(rs, 10, function(y){trend::sens.slope(y)$estimates}, fill = NA)
    
    trs <- data.frame(sen_value = as.numeric(rs_sen),
                      mk_pvalue = as.numeric(rs_mk),
                      Product = strsplit(x, "/")[[1]][5],
                      Variable = strsplit(strsplit(x, "/")[[1]][6], ".nc")[[1]][1],
                      Year = as.numeric(substr(names(raster::brick(x)), 2, 5)))
    
    trs
    
  }) %>% do.call(rbind, .)

trend_data <- rbind(trend_PISCOtv11,
                    trend_PISCOtv12,
                    trend_CHIRTS,
                    trend_VS2018,
                    trend_terraclimate,
                    trend_ERA5land)

rownames(trend_data) <- NULL
trend_data$Variable <- factor(trend_data$Variable,
                              level = c("MTmax", "MTmin", "FD"))
trend_data$Product <- factor(trend_data$Product,
                             level = c("PISCOt_v1.2", "PISCOt_v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5_Land"),
                             labels = c("PISCOt v1.2", "PISCOt v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5-Land"))

pallete_colors = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#0072B2", "#CC79A7")

ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20", size=.5) +
  geom_line(data = trend_data, 
            aes(x = Year, y = sen_value*10, colour = Product), 
            alpha = .75, size = 1.25) +
  geom_point(data = trend_data, 
            aes(x = Year, y = sen_value*10, colour = Product, shape = mk_pvalue < 0.05, fill = mk_pvalue < 0.05), 
            alpha = .75, size = 3,
            show.legend = FALSE) +
  scale_colour_manual(values = pallete_colors) +
  scale_shape_manual(values = c(NA, 1)) +
  ylab("Sen trend slope [°C / decade]") + xlab("") +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) + 
  theme_bw() + 
  theme(legend.background = element_blank(),
        strip.background = element_blank(),
        legend.position = c(0.8, 0.225),
        plot.title=element_text(size=20))

ggsave(file.path(".", "paper", "output", "Figure_09_sen_trend_by_time.pdf"),
       device = "pdf",
       dpi = 300, scale = 1,
       width = 7, height = 5, units = "in")


#### trend slope vs altitude ####

# mean annual mean

grid_files <- list.files(file.path(".", "paper", "others", "global_gridded_products"),
                         pattern = "slope_*", full.names = TRUE) 

grid_df_files <- grid_files %>%
  setNames(., .) %>%
  lapply(function(x){
    
    rs <- raster::raster(x)
    names(rs) <- strsplit(x, "/")[[1]][5]
    rs
    
  })

grid_df_files[[15]] <- raster::t(raster::flip(grid_df_files[[15]], direction = 2))
names(grid_df_files[[15]]) <- strsplit(grid_files[15], "/")[[1]][5]

grid_df_files[[16]] <- raster::t(raster::flip(grid_df_files[[16]], direction = 2))
names(grid_df_files[[16]]) <- strsplit(grid_files[16], "/")[[1]][5]

grid_df_files_rs <- grid_df_files %>%
  lapply(function(x){
    
    rs <- raster::mask(raster::mask(x, shp_lakes[1,], inverse = TRUE), shp_sa)
    rs <- raster::as.data.frame(rs, xy = TRUE)
    rs$Product <- strsplit(colnames(rs)[3], "_")[[1]][2]
    rs$Variable <- strsplit(strsplit(colnames(rs)[3], "_")[[1]][3], ".nc")[[1]][1]
    colnames(rs) <- c("x", "y", "value", "Product", "Variable")
    rs[complete.cases(rs), ]
    
  }) %>% do.call(rbind, .)

grid_df_files <- 
  grid_df_files %>%
  lapply(function(x){
    rs <- raster::mask(raster::mask(x, shp_lakes[1,], inverse = TRUE), shp_sa)
    
    dem_rs <- raster::crop(raster::resample(dem, rs), rs)
    dem_rs_500 <- dem_rs; dem_rs_500[dem_rs_500 > 500] <- NA; dem_rs_500[dem_rs_500 <= 500] <- 1  
    dem_rs_500_1000 <- dem_rs; dem_rs_500_1000[dem_rs_500_1000 > 1000] <- NA; dem_rs_500_1000[dem_rs_500_1000 < 500] <- NA; raster::values(dem_rs_500_1000)[raster::values(dem_rs_500_1000) > 0] = 1 
    dem_rs_1000_1500 <- dem_rs; dem_rs_1000_1500[dem_rs_1000_1500 > 1500] <- NA; dem_rs_1000_1500[dem_rs_1000_1500 < 1000] <- NA; raster::values(dem_rs_1000_1500)[raster::values(dem_rs_1000_1500) > 0] = 1 
    dem_rs_1500_2000 <- dem_rs; dem_rs_1500_2000[dem_rs_1500_2000 > 2000] <- NA; dem_rs_1500_2000[dem_rs_1500_2000 < 1500] <- NA; raster::values(dem_rs_1500_2000)[raster::values(dem_rs_1500_2000) > 0] = 1     
    dem_rs_2000_2500 <- dem_rs; dem_rs_2000_2500[dem_rs_2000_2500 > 2500] <- NA; dem_rs_2000_2500[dem_rs_2000_2500 < 2000] <- NA; raster::values(dem_rs_2000_2500)[raster::values(dem_rs_2000_2500) > 0] = 1 
    dem_rs_2500_3000 <- dem_rs; dem_rs_2500_3000[dem_rs_2500_3000 > 3000] <- NA; dem_rs_2500_3000[dem_rs_2500_3000 < 2500] <- NA; raster::values(dem_rs_2500_3000)[raster::values(dem_rs_2500_3000) > 0] = 1 
    dem_rs_3000_3500 <- dem_rs; dem_rs_3000_3500[dem_rs_3000_3500 > 3500] <- NA; dem_rs_3000_3500[dem_rs_3000_3500 < 3000] <- NA; raster::values(dem_rs_3000_3500)[raster::values(dem_rs_3000_3500) > 0] = 1     
    dem_rs_3500_4000 <- dem_rs; dem_rs_3500_4000[dem_rs_3500_4000 > 4000] <- NA; dem_rs_3500_4000[dem_rs_3500_4000 < 3500] <- NA; raster::values(dem_rs_3500_4000)[raster::values(dem_rs_3500_4000) > 0] = 1 
    dem_rs_4000_4500 <- dem_rs; dem_rs_4000_4500[dem_rs_4000_4500 > 4500] <- NA; dem_rs_4000_4500[dem_rs_4000_4500 < 4000] <- NA; raster::values(dem_rs_4000_4500)[raster::values(dem_rs_4000_4500) > 0] = 1 
    dem_rs_4500_5000 <- dem_rs; dem_rs_4500_5000[dem_rs_4500_5000 > 5000] <- NA; dem_rs_4500_5000[dem_rs_4500_5000 < 4500] <- NA; raster::values(dem_rs_4500_5000)[raster::values(dem_rs_4500_5000) > 0] = 1 
    dem_rs_5000 <- dem_rs; dem_rs_5000[dem_rs_5000 < 5000] <- NA; dem_rs_5000[dem_rs_5000 >= 5000] <- 1
    
    slope_median <- c(raster::cellStats(dem_rs_500*rs, median),
                      raster::cellStats(dem_rs_500_1000*rs, median),
                      raster::cellStats(dem_rs_1000_1500*rs, median),
                      raster::cellStats(dem_rs_1500_2000*rs, median),
                      raster::cellStats(dem_rs_2000_2500*rs, median),
                      raster::cellStats(dem_rs_2500_3000*rs, median),
                      raster::cellStats(dem_rs_3000_3500*rs, median),
                      raster::cellStats(dem_rs_3500_4000*rs, median),
                      raster::cellStats(dem_rs_4000_4500*rs, median),
                      raster::cellStats(dem_rs_4500_5000*rs, median),
                      raster::cellStats(dem_rs_5000*rs, median))
    
    rs <- data.frame(slope_median = slope_median)
    rs$Product <- strsplit(names(x), "_")[[1]][2]
    rs$Variable <- strsplit(strsplit(names(x), "_")[[1]][3], ".nc")[[1]][1]
    rs$Elevation <- 1:length(slope_median)
    rs[complete.cases(rs), ]
    
  }) %>% do.call("rbind", .)


rownames(grid_df_files) <- NULL
grid_df_files$Variable <- factor(grid_df_files$Variable,
                              level = c("MTmax", "MTmin", "FD"))
grid_df_files$Product <- factor(grid_df_files$Product,
                             level = c("PISCOt.v1.2", "PISCOt.v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5.Land"),
                             labels = c("PISCOt v1.2", "PISCOt v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5-Land"))


ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20", size=.5) +
  geom_line(data = grid_df_files, 
            aes(x = Elevation, y = slope_median*10, colour = Product), 
            alpha = .75, size = 1.25) +
  scale_colour_manual(values = pallete_colors) +
  scale_x_continuous(breaks = 1:11,
                     labels = c("<500", "500-1000", "1000-1500", 
                                "1500-2000", "2000-2500", "2500-3000",
                                "3000-3500", "3500-4000", "4000-4500",
                                "4500-5000", ">5000")) + 
  ylab("Sen trend slope [°C / decade]") + xlab("") +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) + 
  theme_bw() + 
  theme(legend.background = element_blank(),
        strip.background = element_blank(),
        legend.position = c(0.8, 0.175),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, size = 7),
        plot.title=element_text(size=20))

ggsave(file.path(".", "paper", "output", "Figure_10_sen_trend_by_elevation.pdf"),
       device = "pdf",
       dpi = 300, scale = 1,
       width = 7, height = 5, units = "in")

# Figure_S04

rownames(grid_df_files_rs) <- NULL
grid_df_files_rs$Product <- factor(grid_df_files_rs$Product,
                                   level = c("PISCOt.v1.2", "PISCOt.v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5.Land"),
                                   labels = c("PISCOt v1.2", "PISCOt v1.1", "VS2018", "TerraClimate", "CHIRTS", "ERA5-Land"))


ggplot() + 
  geom_raster(data = grid_df_files_rs[grid_df_files_rs$Variable == "MTmax",], aes(x = x, y = y, fill = value*10)) +
  facet_grid(Variable~Product) + 
  geom_polygon(data = shp_sa, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.5) +
  geom_polygon(data = shp_lakes, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "black", size = 0.5) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$olsen_seq)(15) %>% rev(),
                       limits = c(-.6, .6),
                       "     MTmax (°C)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              barwidth = 0.5, barheight = 4,
                                              label.theme = element_text(size = 5),
                                              title.theme = element_text(size = 7,
                                                                         angle = 90,
                                                                         vjust = 2))) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.5, -12), xlim = c(-73.5, -68)) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.box = 'vertical',
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9),
        strip.text.y = element_blank(),
        legend.spacing.x = unit(.01, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        legend.margin=margin(t=0)) + 
  labs(x = "", y = "") -> mean_MTmax_trend

ggsave(file.path(".", "paper", "output", "Figure_S05_MTmax.pdf"),
       plot = mean_MTmax_trend,
       device = "pdf",
       dpi = 300, scale = .75,
       width = 10, height = 6, units = "in")


ggplot() + 
  geom_raster(data = grid_df_files_rs[grid_df_files_rs$Variable == "MTmin",], aes(x = x, y = y, fill = value*10)) +
  facet_grid(Variable~Product) + 
  geom_polygon(data = shp_sa, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.5) +
  geom_polygon(data = shp_lakes, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "black", size = 0.5) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$olsen_seq)(15) %>% rev(),
                       limits = c(-.6, .6),
                       "     MTmin (°C)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              barwidth = 0.5, barheight = 4,
                                              label.theme = element_text(size = 5),
                                              title.theme = element_text(size = 7,
                                                                         angle = 90,
                                                                         vjust = 2))) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.5, -12), xlim = c(-73.5, -68)) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.box = 'vertical',
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.spacing.x = unit(.01, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        legend.margin=margin(t=0)) + 
  labs(x = "", y = "") -> mean_MTmin_trend

ggsave(file.path(".", "paper", "output", "Figure_S05_MTmin.pdf"),
       plot = mean_MTmin_trend,
       device = "pdf",
       dpi = 300, scale = .75,
       width = 10, height = 6, units = "in")

grid_df_files_rs <- rbind(grid_df_files_rs, 
                       data.frame(x = c(-68.62500, -68.62500), 
                                  y = c(-13.825, -13.825),
                                  value = c(NA, NA), 
                                  Product = c("VS2018", "TerraClimate"), 
                                  Variable = c("FD", "FD")))

ggplot() + 
  geom_raster(data = grid_df_files_rs[grid_df_files_rs$Variable == "FD",], aes(x = x, y = y, fill = value*10)) +
  facet_grid(Variable~Product) + 
  geom_polygon(data = shp_sa, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black", size = 0.5) +
  geom_polygon(data = shp_lakes, # water bodies > 10 km^2
               aes(x = long, y = lat, group = group),
               fill = "skyblue", colour = "black", size = 0.5) +
  scale_fill_gradientn(colors = colorRampPalette(ochRe::ochre_palettes$olsen_seq)(15),
                       limits = c(-.8, .8),
                       "         FD (%)",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              title.position = "left",
                                              barwidth = 0.5, barheight = 4,
                                              label.theme = element_text(size = 5),
                                              title.theme = element_text(size = 7,
                                                                         angle = 90,
                                                                         vjust = 2))) +
  coord_quickmap(expand = c(0, 0), ylim = c(-18.5, -12), xlim = c(-73.5, -68)) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.box = 'vertical',
        legend.background = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null"),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.spacing.x = unit(.01, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        legend.margin=margin(t=0)) + 
  labs(x = "", y = "") -> mean_FD_trend

library(grid)

g = ggplotGrob(mean_FD_trend)
g$layout
pos <- grepl(pattern = c("panel-1-3"), g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
pos <- grepl(pattern = c("panel-1-4"), g$layout$name)
g$grobs <- g$grobs[!pos]
g$layout <- g$layout[!pos, ]
grid.newpage()


ggsave(file.path(".", "paper", "output", "Figure_S05_FD.pdf"),
       plot = grid.draw(g),
       device = "pdf",
       dpi = 300, scale = .75,
       width = 10, height = 6, units = "in")
