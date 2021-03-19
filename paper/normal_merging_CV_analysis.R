rm(list = ls())

library(spatialsample)
"%>%" = magrittr::`%>%`

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/Normals_OBS.RDS")

####### points for spcv/nospcv #######

shp_peru = file.path(".", "data", "raw", "vectorial", "Departamentos.shp") %>% 
  raster::shapefile() 

shp_sa = file.path(".", "data", "raw", "vectorial", "Sudamérica.shp") %>% 
  raster::shapefile()

set.seed(2020+1)
folds_spcv <- spatial_clustering_cv(qc_data$xyz@data, coords = c("LON", "LAT"), v = 10)
folds_nospcv <- rsample::vfold_cv(qc_data$xyz@data, v = 10)


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

c(plt1, plt2) %>%
  update(ylim = c(-18.575, 1.275), xlim = c(-81.325, -67.175), xlab = "", ylab = "") +
  latticeExtra::layer(sp.polygons(shp_peru, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE) +
  latticeExtra::layer(sp.polygons(shp_sa, fill = NA, col = "gray50"), under = TRUE, superpose = FALSE)


####### statistics for spcv/nospcv #######

tmax_obs <- reshape2::melt(qc_data$values$tmax) %>% setNames(c("date", "ID", "obs"))
tmin_obs <- reshape2::melt(qc_data$values$tmin) %>% setNames(c("date", "ID", "obs"))

# cv 
output_normals <- "./paper/others/normals"
tmax_spcv <- file.path(output_normals, sprintf("%s/tmax_spcv_%02d.RDS", "tmax",  1:12)) %>%
  lapply(function(x){
    

  })
