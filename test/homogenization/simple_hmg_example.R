rm(list = ls())

library(xts)
"%>%" = magrittr::`%>%`

source('./src/process/Homogenization/HG_simple_snht.R')

# random dataset with a fake a break
data = rnorm(1000)
time = 1:1000
brk = sample(1000, size=1)
data[1:brk] = data[1:brk]-2
data = xts(data, as.Date(1:length(data)))
colnames(data) <- "PRUEBA" # it should be a matrix

# time serie
lattice::xyplot(data)
# breaks
snht_hmg(ts_data = data)$breaks
# corrected time serie
lattice::xyplot(snht_hmg(ts_data = data)$hmg)
# difference
lattice::xyplot(snht_hmg(ts_data = data)$original-snht_hmg(ts_data = data)$hmg)
