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
tx_qc <- apply(qc_data$values$tmax, 1, function(x) sum(!is.na(x)))
tx_qc <- xts(tx_qc, time(qc_data$values$tmax))
tx_qc <- tx_qc["/2020"]

tn_qc <- apply(qc_data$values$tmin, 1, function(x) sum(!is.na(x)))
tn_qc <- xts(tn_qc, time(qc_data$values$tmin))
tn_qc <- tn_qc["/2020"]


xyplot(tx, par.settings = mytheme, col = "tomato", lwd = 2) + 
xyplot(tn, par.settings = mytheme, col = "royalblue", lwd = 2) -> p1

xyplot(tx_qc, par.settings = mytheme, col = "tomato", lwd = 2) + 
  xyplot(tn_qc, par.settings = mytheme, col = "royalblue", lwd = 2) -> p2

c("Raw" = p1, "After QC" = p2, layout = c(1, 2)) %>%
  update(ylim = c(0, 450), ylab = "Number of data", xlab = "") -> pfinal

pdf(file = file.path(".", "paper", "output", "Figure_S01_data_lenght.pdf"),
     width = 6, height = 5)
print(pfinal)
dev.off()
