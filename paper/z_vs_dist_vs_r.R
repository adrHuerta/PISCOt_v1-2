rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(latticeExtra)
library(lattice)
library(parallel)

##

font.settings <- list(fontfamily = "helvetica")

mytheme <- list(strip.background = list(col = 'white'), 
                strip.border = list(col = 'black'),
                par.xlab.text = font.settings,
                par.ylab.text = font.settings,
                axis.text = font.settings,
                sub.text = font.settings,
                add.text = font.settings)

# qc 01

qc01 <- file.path(".", "data", "processed", "obs", "qc_output", "RAW(QC01)_data.RDS") %>% 
  readRDS()

sapply(qc01$values, function(x) min(zoo::index(x))) %>% min() %>% as.Date()
sapply(qc01$values, function(x) max(zoo::index(x))) %>% sort(., decreasing = TRUE) %>% .[7] %>% as.Date()

############ 
tx <- parallel::mclapply(qc01$values, function(x) x[,1], mc.cores = 6) %>% do.call("cbind", .)
tx <- tx["1981/2020"]
colnames(tx) <- names(qc01$values)

tn <- parallel::mclapply(qc01$values, function(x) x[,2], mc.cores = 6) %>% do.call("cbind", .)
tn <- tn["1981/2020"]
colnames(tn) <- names(qc01$values)

# without seasonal variability?
# get_monthly_clim <- function(daily_time_serie)
# {
#   
#   median_values <- sapply(1:12, function(x) median(daily_time_serie[xts::.indexmon(daily_time_serie) %in% (x - 1)], na.rm = TRUE))
#   sd_values <- sapply(1:12, function(x) sd(daily_time_serie[xts::.indexmon(daily_time_serie) %in% (x - 1)], na.rm = TRUE))
#   median_ts <- daily_time_serie
#   sd_ts <- daily_time_serie
#   
#   for(i in 1:12){
#     median_ts[xts::.indexmon(median_ts) %in% (i - 1)] <- median_values[i]
#     sd_ts[xts::.indexmon(sd_ts) %in% (i - 1)] <- sd_values[i]
#   }
#   
#   return((daily_time_serie - median_ts)/sd_ts)
#   
# }

# tx <- parallel::mclapply(qc01$values, function(x) get_monthly_clim(x[,1]), mc.cores = 6) %>% do.call("cbind", .)
# colnames(tx) <- names(qc01$values)
# 
# tn <- parallel::mclapply(qc01$values, function(x) get_monthly_clim(x[,2]), mc.cores = 6) %>% do.call("cbind", .)
# colnames(tn) <- names(qc01$values)


# number of stations vs distance vs elevation difference
parallel::mclapply(1:length(qc01$xyz$ID),
                   function(x){
                     
                     x_can <- qc01$xyz[x, ]
                     x_nei <- qc01$xyz[-x,]
                     
                     x_res <- geosphere::distVincentyEllipsoid(x_can[, c("LON", "LAT")],
                                                               x_nei[, c("LON", "LAT")])
                     x_nei[, "DIS"]  <- x_res/1000
                     x_nei[, "ALT_diff"] <- abs(x_can[, "ALT"] - x_nei[, "ALT"])
                     
                     seq_dist <- c(5, 25, 50, 70, 100)
                     seq_elv <- c(100, 500, 1000, 5500)
                     
                     sapply(seq_dist, function(z){
                       sapply(seq_elv, function(y){
                         
                         c(dim(x_nei[x_nei$DIS < z & x_nei$ALT_diff <= y, ])[1])
                         
                       })
                     }) %>% as.data.frame.table -> response
                     
                     response$Var1 <- factor(response$Var1, levels = c("A","B","C","D"), labels = c("100", "500", "1000", ">1000"))
                     response$Var2 <- factor(response$Var2, levels = c("A","B","C","D", "E"), labels = c("5", "25", "50", "70", "100"))
                     
                     response
                     
                   }, mc.cores = 6) %>%
  do.call(rbind, .) -> response_nei

bw_theme <- trellis.par.get()
bw_theme$box.rectangle$col <- "black"
bw_theme$box.umbrella$col <- "black"
bw_theme$plot.symbol$col <- "grey80"

bwplot(Freq~Var2, group = Var1, data = response_nei,
       panel = panel.superpose,
       box.width = 1/6,
       par.settings = bw_theme,
       panel.groups = function(x, y,..., group.number)
         panel.bwplot(x + (group.number-2.5)/6, y, ...),
       auto.key = list(corner = c(0, 1),
                       title = "Elevation\ndifference (m)",
                       cex.title = 1,
                       cex = .8,
                       rectangles = TRUE,
                       points = FALSE),
       xlab = "Distance (km)",
       ylab = "Number of stations") -> p3
p3 <- update(p3, par.settings = mytheme)


# Correlation vs distance vs Elevation difference
parallel::mclapply(1:length(qc01$xyz$ID),
                   function(x){
                     
                     x_can <- qc01$xyz[x, ]
                     x_nei <- qc01$xyz[-x,]
                     
                     x_res <- geosphere::distVincentyEllipsoid(x_can[, c("LON", "LAT")],
                                                               x_nei[, c("LON", "LAT")])
                     x_nei[, "DIS"]  <- x_res/1000
                     x_nei[, "ALT_diff"] <- abs(x_can[, "ALT"] - x_nei[, "ALT"])
                     
                     seq_dist <- seq(0, 150, 15)
                     seq_dist[1] <- 1
                     seq_elv <- c(100, 500, 1000, 5500)
                     
                     sapply(seq_dist, function(z){
                       sapply(seq_elv, function(y){
                         
                         sapply(x_nei[x_nei$DIS < z & x_nei$ALT_diff <= y, ]$ID,
                                function(j) cor(tx[,x], tx[,j], use = "pairwise.complete.obs")) %>%
                           mean(na.rm = TRUE)
                       })
                     }) %>% as.data.frame.table -> response0
                     
                     sapply(seq_dist, function(z){
                       sapply(seq_elv, function(y){
                         
                         sapply(x_nei[x_nei$DIS < z & x_nei$ALT_diff <= y, ]$ID,
                                function(j) cor(tn[,x], tn[,j], use = "pairwise.complete.obs")) %>%
                           mean(na.rm = TRUE)
                       })
                     }) %>% as.data.frame.table -> response1
                     
                     sapply(seq_dist, function(z){
                       sapply(seq_elv, function(y){
                         
                         x_nei[x_nei$DIS < z & x_nei$ALT_diff <= y, ]$ID %>%
                           length()
                       })
                     }) %>% as.data.frame.table -> response2
                     
                     response <- cbind(response0, response1[, 3], response2[, 3])
                     colnames(response) <- c("Var1", "Var2", "Tmax", "Tmin", "Dist")
                     
                     response$Var1 <- factor(response$Var1, labels = c("100", "500", "1000", ">1000"))
                     response$Var2 <- factor(response$Var2, labels = seq_dist)
                     
                     response
                     
                   }, mc.cores = 4) %>%
  do.call(rbind, .) -> response_cor


xyplot(value  ~ Var2 | variable, groups = Var1, type = c("l"), lwd = 3,
       auto.key = list(lines = TRUE, points = FALSE,
                       corner = c(.975, .3),
                       title = "Elevation\ndifference (m)",
                       cex.title = 1,
                       cex = .8),
       par.settings = list(superpose.line = list(lwd = 3)),
       xlab = "Distance (km)", ylab = "Pearson Correlation", ylim = c(0.5, 0.875),
       data = aggregate(response_cor[c("Tmin", "Tmax")], 
                        by = response_cor[c("Var1", "Var2")],
                        FUN = function(x) median(x, na.rm = TRUE)) %>%
         reshape2::melt()) %>%
  update(par.settings = mytheme,
         layout = c(1, 2))-> p0


xyplot(Dist  ~ Var2, groups = Var1, type = c("l"), lwd = 3,
       auto.key = FALSE,
       ylim = c(0, 3500),
       xlab = "Distance (km)",
       ylab = "Number of pair stations",
       data = aggregate(response_cor[c("Dist")], 
                        by = response_cor[c("Var1", "Var2")],
                        FUN = function(x) sum(x, na.rm = TRUE))) %>%
  update(par.settings = mytheme)-> p1


tiff(filename = file.path(".", "paper", "output", "Fig_z_vs_dist_vs_r.tiff"),
     width = 10, height = 5.75, units = "in",
     res = 500)
print(cowplot::plot_grid(p3, p0,  labels = c("a)", "b)")))
dev.off()
