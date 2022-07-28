rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(lattice)

##

# font.settings <- list(fontfamily = "helvetica")

mytheme <- list(strip.background = list(col = 'white'), 
                strip.border = list(col = 'black')
                # par.xlab.text = font.settings,
                # par.ylab.text = font.settings,
                # axis.text = font.settings,
                # sub.text = font.settings,
                # add.text = font.settings
)
# PE106106 

tmax_gridded_PE106106 <- read.csv.zoo(file.path("paper", "others", "global_gridded_products", "PISCOt_v1.2", "PE106106_tmin.csv"),
                                      format = "%Y-%m-%d", skip = 0)
tmax_hmg_PE106106 <- readRDS(file.path("data", "processed", "obs", "qc_output", "QC_GF_HG_data.RDS"))$values$tmin[, "PE106106"]
tmax_PE106106 <- cbind(tmax_hmg_PE106106, as.xts(tmax_gridded_PE106106)) %>%
  setNames(c("Observed Tmin", "Grid Tmin"))

PE106106 <- xyplot(tmax_PE106106, type = "p", cex = .1,
                   par.settings = mytheme, xlab = "", ylab = "",
                   ylim = c(5, 26))

# PE113116 

tmax_gridded_PE113116 <- read.csv.zoo(file.path("paper", "others", "global_gridded_products", "PISCOt_v1.2", "PE113116_tmax.csv"),
                                      format = "%Y-%m-%d", skip = 0)
tmax_hmg_PE113116 <- readRDS(file.path("data", "processed", "obs", "qc_output", "QC_GF_HG_data.RDS"))$values$tmax[, "PE113116"]

tmax_PE113116 <- cbind(tmax_hmg_PE113116, as.xts(tmax_gridded_PE113116)) %>%
  setNames(c("Observed Tmax", "Grid Tmax"))

PE113116 <- xyplot(tmax_PE113116, type = "p", cex = .1,
                   par.settings = mytheme, xlab = "", ylab = "",
                   ylim = c(5, 25))

pdf(file = file.path(".", "paper", "output", "Figure_S04_precision_example.pdf"),
    width = 10, height = 5.75)
print(cowplot::plot_grid(PE106106, PE113116,  labels = c("(a)", "(b)")))
dev.off()

