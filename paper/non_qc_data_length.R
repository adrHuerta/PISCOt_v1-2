rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(lattice)
library(latticeExtra)

font.settings <- list(fontfamily = "helvetica")

mytheme <- list(strip.background = list(col = 'gray95'), 
                strip.border = list(col = 'black'),
                par.xlab.text = font.settings,
                par.ylab.text = font.settings,
                axis.text = font.settings,
                sub.text = font.settings,
                add.text = font.settings)

##

non_qc_dir = dir("data/processed/obs/non_qc_output", full.names = TRUE) 

non_qc_data <- lapply(non_qc_dir, readRDS)
names(non_qc_data) <- sapply(non_qc_dir, 
                             function(z) strsplit(z, "/")[[1]][5] %>% strsplit(., "\\.") %>% .[[1]] %>% .[1])

qc1 <- non_qc_data$non_QC01 %>% sapply(nrow)
qc2 <- non_qc_data$non_QC02 %>% sapply(function(x) c(x[x$var == "tmax", ] %>% nrow, x[x$var == "tmin", ] %>% nrow))
qc3 <- non_qc_data$non_QC03 %>% sapply(function(x) c(x[x$var == "tmax", ] %>% nrow, x[x$var == "tmin", ] %>% nrow))
qc4 <- non_qc_data$non_QC04 %>% sapply(function(x) c(x[x$var == "tmax", ] %>% nrow, x[x$var == "tmin", ] %>% nrow))
qc5 <- non_qc_data$non_QC05 %>% sapply(function(x) c(x[x$var == "tmax", ] %>% nrow, x[x$var == "tmin", ] %>% nrow))
qc6 <- non_qc_data$non_QC06[lengths(non_qc_data$non_QC06) != 0] %>% sapply(function(x) c(x[x$var == "tmax", ] %>% nrow, x[x$var == "tmin", ] %>% nrow))

sum(qc1) + sum(qc2[1,]) + sum(qc3[1,]) + sum(qc4[1,]) + sum(qc5[1, ]) + sum(qc6[1, ])
sum(qc1) + sum(qc2[2,]) + sum(qc3[2,]) + sum(qc4[2,]) + sum(qc5[2, ]) + sum(qc6[2, ])

non_qc_data$non_QC01 %>% lapply(function(x) x$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC1_ts

non_qc_data$non_QC02 %>% lapply(function(x) x[x$var == "tmax", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC2_ts_tmax
non_qc_data$non_QC02 %>% lapply(function(x) x[x$var == "tmin", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC2_ts_tmin

non_qc_data$non_QC03 %>% lapply(function(x) x[x$var == "tmax", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC3_ts

non_qc_data$non_QC04 %>% lapply(function(x) x[x$var == "tmax", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC4_ts_tmax

non_qc_data$non_QC04 %>% lapply(function(x) x[x$var == "tmin", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC4_ts_tmin

non_qc_data$non_QC05 %>% lapply(function(x) x[x$var == "tmax", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC5_ts_tmax

non_qc_data$non_QC05 %>% lapply(function(x) x[x$var == "tmin", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC5_ts_tmin

non_qc_data$non_QC06 %>% lapply(function(x) x[x$var == "tmax", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC6_ts_tmax

non_qc_data$non_QC06 %>% lapply(function(x) x[x$var == "tmin", ]$date) %>% Reduce(c, .) %>% 
  table() %>%
  xts(., as.Date(names(.))) -> QC6_ts_tmin

to_fill <- xts::xts(,seq(as.Date("1927-03-01"), as.Date("2020-03-23"), by = "day"))

QC1_ts <- xts::merge.xts(QC1_ts, to_fill)
QC2_ts_tmax <- xts::merge.xts(QC2_ts_tmax, to_fill)
QC2_ts_tmin <- xts::merge.xts(QC2_ts_tmin, to_fill)
QC3_ts <- xts::merge.xts(QC3_ts, to_fill)
QC4_ts_tmax <- xts::merge.xts(QC4_ts_tmax, to_fill)
QC4_ts_tmin <- xts::merge.xts(QC4_ts_tmin, to_fill)
QC5_ts_tmax <- xts::merge.xts(QC5_ts_tmax, to_fill)
QC5_ts_tmin <- xts::merge.xts(QC5_ts_tmin, to_fill)
QC6_ts_tmax <- xts::merge.xts(QC6_ts_tmax, to_fill)
QC6_ts_tmin <- xts::merge.xts(QC6_ts_tmin, to_fill)

xyplot(QC1_ts, cex = .5, type = "p", col = "black", auto.key = FALSE) -> p1
xyplot(cbind(QC2_ts_tmax, QC2_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p2
xyplot(QC3_ts, cex = .5, type = "p", col = "black", auto.key = FALSE) -> p3
xyplot(cbind(QC4_ts_tmax, QC4_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p4
xyplot(cbind(QC5_ts_tmax, QC5_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p5
xyplot(cbind(QC6_ts_tmax, QC6_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p6

c(qc1=p1, qc2=p2, qc3=p3, qc4=p4, qc5=p5, qc6=p6,layout=c(2,3)) %>%
  update(xlab = "", ylab = "Number of deleted data",
         par.settings = mytheme) -> pfinal

jpeg(filename = file.path(".", "paper", "output", "Fig_non_qc_data_lenght.jpg"),
     width = 1700, height = 1000, units = "px",
     res = 200)
print(pfinal)
dev.off()