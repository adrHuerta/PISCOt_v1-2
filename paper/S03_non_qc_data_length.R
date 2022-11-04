rm(list = ls())
"%>%" = magrittr::`%>%`

library(xts)
library(lattice)
library(latticeExtra)

# font.settings <- list(fontfamily = "helvetica")

mytheme <- list(strip.background = list(col = 'white'), 
                strip.border = list(col = 'black')
                # par.xlab.text = font.settings,
                # par.ylab.text = font.settings,
                # axis.text = font.settings,
                # sub.text = font.settings,
                # add.text = font.settings
)
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

to_fill <- xts::xts(,seq(as.Date("1927-03-01"), as.Date("2020-12-31"), by = "day"))

QC1_ts <- xts::merge.xts(QC1_ts, to_fill); QC1_ts <- QC1_ts["/2020"]
QC2_ts_tmax <- xts::merge.xts(QC2_ts_tmax, to_fill); QC2_ts_tmax <- QC2_ts_tmax["/2020"] 
QC2_ts_tmin <- xts::merge.xts(QC2_ts_tmin, to_fill); QC2_ts_tmin <- QC2_ts_tmin["/2020"]
QC3_ts <- xts::merge.xts(QC3_ts, to_fill); QC3_ts <- QC3_ts["/2020"]
QC4_ts_tmax <- xts::merge.xts(QC4_ts_tmax, to_fill); QC4_ts_tmax <- QC4_ts_tmax["/2020"]
QC4_ts_tmin <- xts::merge.xts(QC4_ts_tmin, to_fill); QC4_ts_tmin <- QC4_ts_tmin["/2020"]
QC5_ts_tmax <- xts::merge.xts(QC5_ts_tmax, to_fill); QC5_ts_tmax <- QC5_ts_tmax["/2020"]
QC5_ts_tmin <- xts::merge.xts(QC5_ts_tmin, to_fill); QC5_ts_tmin <- QC5_ts_tmin["/2020"]
QC6_ts_tmax <- xts::merge.xts(QC6_ts_tmax, to_fill); QC6_ts_tmax <- QC6_ts_tmax["/2020"]
QC6_ts_tmin <- xts::merge.xts(QC6_ts_tmin, to_fill); QC6_ts_tmin <- QC6_ts_tmin["/2020"]

library(ggplot2)

QC1_ts <- fortify.zoo(QC1_ts)
QC1_ts[is.na(QC1_ts)] <- 0
ggplot(QC1_ts, aes(x=Index, y=QC1_ts), fill = "black") + 
  geom_bar(stat="identity", na.rm = TRUE) + theme_bw() + xlab("") + ylab("") +
  annotate("text", x = as.Date("1940-01-01"), y = 40, label = "i) Obvious errors") -> p1
  

QC2_ts <- cbind(QC2_ts_tmax, QC2_ts_tmin)
QC2_ts[is.na(QC2_ts)] <- 0
QC2_ts <- fortify.zoo(QC2_ts)
QC2_ts <- reshape2::melt(QC2_ts, id.vars = "Index")
ggplot(QC2_ts, aes(x=Index, y=value, fill = variable)) +
  scale_fill_manual(values = c("tomato", "royalblue")) +
  geom_bar(stat="identity") + theme_bw() + guides(fill="none") + xlab("") + ylab("") +
  annotate("text", x = as.Date("1940-01-01"), y = 30, label = "ii) Extreme values") -> p2



QC3_ts <- fortify.zoo(QC3_ts)
QC3_ts[is.na(QC3_ts)] <- 0
ggplot(QC3_ts, aes(x=Index, y=QC3_ts), fill = "black") + 
  geom_bar(stat="identity", na.rm = TRUE) + theme_bw() + xlab("") + ylab("") +
  scale_y_continuous(labels = c(0, 1), breaks = c(0, 1)) +
  annotate("text", x = as.Date("1945-01-01"), y = .9, label = "iii) Internal consistency values") -> p3


QC4_ts <- cbind(QC4_ts_tmax, QC4_ts_tmin)
QC4_ts[is.na(QC4_ts)] <- 0
QC4_ts <- fortify.zoo(QC4_ts)
QC4_ts <- reshape2::melt(QC4_ts, id.vars = "Index")
ggplot(QC4_ts, aes(x=Index, y=value, fill = variable)) + 
  scale_fill_manual(values = c("tomato", "royalblue")) +
  geom_bar(stat="identity") + theme_bw() + guides(fill="none") + xlab("") + ylab("") +
  annotate("text", x = as.Date("1942-01-01"), y = 2.75, label = "iv) Temporal coherence") -> p4


QC5_ts <- cbind(QC5_ts_tmax, QC5_ts_tmin)
QC5_ts[is.na(QC5_ts)] <- 0
QC5_ts <- fortify.zoo(QC5_ts)
QC5_ts <- reshape2::melt(QC5_ts, id.vars = "Index")
ggplot(QC5_ts, aes(x=Index, y=value, fill = variable)) + 
  scale_fill_manual(values = c("tomato", "royalblue")) +
  geom_bar(stat="identity") + theme_bw() + guides(fill="none") + xlab("") + ylab("") +
  annotate("text", x = as.Date("1941-01-01"), y = 7.75, label = "v) Spatial coherence") -> p5


QC6_ts <- cbind(QC6_ts_tmax, QC6_ts_tmin)
QC6_ts[is.na(QC6_ts)] <- 0
QC6_ts <- fortify.zoo(QC6_ts)
QC6_ts <- reshape2::melt(QC6_ts, id.vars = "Index")
ggplot(QC6_ts, aes(x=Index, y=value, fill = variable)) + 
  scale_fill_manual(values = c("tomato", "royalblue")) +
  geom_bar(stat="identity") + theme_bw() + guides(fill="none") + xlab("") + ylab("") +
  annotate("text", x = as.Date("1941-01-01"), y = 95, label = "vi) Visual Inspection") -> p6

library(cowplot)
library(grid)
library(gridExtra)

plot_grid(p1 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5), axis.text.x = element_blank(), axis.ticks.x=element_blank()),
          p2 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5), axis.text.x = element_blank(), axis.ticks.x=element_blank()), 
          p3 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5), axis.text.x = element_blank(), axis.ticks.x=element_blank()), 
          p4 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5), axis.text.x = element_blank(), axis.ticks.x=element_blank()), 
          p5 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5), axis.text.x = element_blank(), axis.ticks.x=element_blank()), 
          p6 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)),
          ncol = 1) -> plot_ps

y.grob <- textGrob("Number of deleted observations", 
                   gp=gpar(col="black", fontsize=12), rot=90)

plot_ps <- grid.arrange(arrangeGrob(plot_ps, left = y.grob))

ggsave(file.path(".", "paper", "output", "Figure_S03_non_qc_data_lenght.pdf"),
       device = "pdf",
       plot = plot_ps,
       dpi = 500, scale = 0.75,
       width = 8, height = 10, units = "in")

# xyplot(QC1_ts, cex = .5, type = "p", col = "black", auto.key = FALSE) -> p1
# xyplot(cbind(QC2_ts_tmax, QC2_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p2
# xyplot(QC3_ts, cex = .5, type = "p", col = "black", auto.key = FALSE) -> p3
# xyplot(cbind(QC4_ts_tmax, QC4_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p4
# xyplot(cbind(QC5_ts_tmax, QC5_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p5
# xyplot(cbind(QC6_ts_tmax, QC6_ts_tmin), superpose = TRUE, type = "p", cex = c(.5, .15), col = c("tomato", "royalblue"), auto.key = FALSE) -> p6
# 
# c("(i) Obvious errors" = p1, "(ii) Extreme values" = p2, 
#   "(iii) Internal consistency" = p3, "(iv) Temporal coherence"= p4, 
#   "(v) Spatial coherence" = p5, "(vi) Visual Inspection" = p6, layout = c(2,3)) %>%
#   update(xlab = "", ylab = "Number of deleted observations",
#          par.settings = mytheme) -> pfinal
# 
# pdf(file = file.path(".", "paper", "output", "Figure_S03_non_qc_data_lenght.pdf"),
#      width = 8, height = 5)
# print(pfinal)
# dev.off()
