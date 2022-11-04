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

# qc 01

qc01 <- file.path(".", "data", "processed", "obs", "qc_output", "RAW(QC01)_data.RDS") %>% 
  readRDS()

qc_data <- file.path(".", "data", "processed", "obs", "qc_output", "QC_data.RDS") %>% 
  readRDS()

#

tx_data <- parallel::mclapply(qc01$values, function(x) x[,1], mc.cores = 6) %>% do.call("cbind", .)
colnames(tx_data) <- names(qc01$values)
tx <- apply(tx_data, 1, function(x) sum(!is.na(x)))
tx <- xts(tx, time(tx_data))
tx <- tx["/2020"]

tn_data <- parallel::mclapply(qc01$values, function(x) x[,2], mc.cores = 6) %>% do.call("cbind", .)
colnames(tn_data) <- names(qc01$values)
tn <- apply(tn_data, 1, function(x) sum(!is.na(x)))
tn <- xts(tn, time(tn_data))
tn <- tn["/2020"]

#
# tx_qc <- apply(qc_data$values$tmax, 1, function(x) sum(!is.na(x)))
# tx_qc <- xts(tx_qc, time(qc_data$values$tmax))
# tx_qc <- tx_qc["/2020"]
# 
# tn_qc <- apply(qc_data$values$tmin, 1, function(x) sum(!is.na(x)))
# tn_qc <- xts(tn_qc, time(qc_data$values$tmin))
# tn_qc <- tn_qc["/2020"]
# 
# 
# xyplot(tx, par.settings = mytheme, col = "tomato", lwd = 2) + 
# xyplot(tn, par.settings = mytheme, col = "royalblue", lwd = 2) -> p1
# 
# xyplot(tx_qc, par.settings = mytheme, col = "tomato", lwd = 2) + 
#   xyplot(tn_qc, par.settings = mytheme, col = "royalblue", lwd = 2) -> p2
# 
# c("Raw" = p1, "After QC" = p2, layout = c(1, 2)) %>%
#   update(ylim = c(0, 450), ylab = "Number of observations", xlab = "") -> pfinal
# 
# pdf(file = file.path(".", "paper", "output", "Figure_S01_data_lenght.pdf"),
#     width = 6, height = 5)
# print(pfinal)
# dev.off()

library(ggplot2)

raw_ts <- cbind(tx, tn) %>% setNames(c("Tmax", "Tmin"))
raw_ts <- fortify.zoo(raw_ts)
raw_ts <- reshape2::melt(raw_ts, id.vars = "Index")
ggplot(raw_ts, aes(x=Index, y=value, colour = variable)) +
  scale_colour_manual("", values = c("tomato", "royalblue")) +
  geom_line(size = .25) + theme_bw() + xlab("") + ylab("") + theme(legend.position = c(0.15, 0.5),
                                                                   legend.justification = "left", 
                                                                   legend.margin = margin(0, 0, 0, 0),
                                                                   legend.spacing.x = unit(0, "pt"),
                                                                   legend.box.background = element_rect(colour = "gray50", size = .5),
                                                                   legend.title = element_blank()) +
  annotate("text", x = as.Date("1930-01-01"), y = 380, label = "Raw") -> p1


qc_ts <- cbind(tx_qc, tn_qc) %>% setNames(c("Tmax", "Tmin"))
qc_ts <- fortify.zoo(qc_ts) 
qc_ts <- reshape2::melt(qc_ts, id.vars = "Index")
ggplot(qc_ts, aes(x=Index, y=value, colour = variable)) +
  scale_colour_manual(values = c("tomato", "royalblue")) +
  geom_line(size = .25) + guides(colour="none") +  theme_bw() + xlab("") + ylab("") +
  annotate("text", x = as.Date("1984-01-01"), y = 345, label = "After QC") -> p2

library(cowplot)
library(grid)
library(gridExtra)

plot_grid(p1 + theme(plot.margin=grid::unit(c(2,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)),
          p2 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)),
          ncol = 1, nrow = 2) -> plot_ps

y.grob <- textGrob("Number of observations", 
                   gp=gpar(col="black", fontsize=12), rot=90)

plot_ps <- grid.arrange(arrangeGrob(plot_ps, left = y.grob))

ggsave(file.path(".", "paper", "output", "Figure_S01_data_lenght.pdf"),
       device = "pdf",
       plot = plot_ps,
       dpi = 500, scale = 0.75,
       width = 6, height = 5, units = "in")

