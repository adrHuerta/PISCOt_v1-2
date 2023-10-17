rm(list = ls())

library(xts)
library(spatialsample)
library(ggplot2)
"%>%" = magrittr::`%>%`

# obs
qc_data <- readRDS("./data/processed/obs/qc_output/OBS.RDS")

# shps in other script

# stations for cv
stations_CV <- qc_data$xyz[qc_data$xyz@data$filter_qc70 != 0, ]

####### points for nospcv #######
set.seed(2020+1)
folds_nospcv <- rsample::vfold_cv(stations_CV@data, v = 10)

####### statistics for nospcv #######
tmax_obs <- qc_data$values$tmax[, stations_CV@data$ID]
tmin_obs <- qc_data$values$tmin[, stations_CV@data$ID]

# cv
output_anomalies <- "./paper/others/values"

tmax_nospcv <- file.path(output_anomalies, sprintf("%s/tmax_nospcv_%s.RDS", "tmax",  time(tmax_obs))) %>%
  lapply(function(x) readRDS(x)) %>% do.call("rbind", .) %>%
  xts(., time(tmax_obs))

tmin_nospcv <- file.path(output_anomalies, sprintf("%s/tmin_nospcv_%s.RDS", "tmin",  time(tmax_obs))) %>%
  lapply(function(x) readRDS(x)) %>% do.call("rbind", .) %>%
  xts(., time(tmin_obs))

# to seasonal and annual values / computing theil-sen slope
cold_months <- which(format(time(tmax_obs), "%m") %in% c("04", "05", "06", "07", "08", "09"))
warm_months <- which(format(time(tmax_obs), "%m") %in% c("10", "11", "12", "01", "02", "03"))

# obs tmax
tmax_obs_yearly <- tmax_obs %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmax_obs_cold <- tmax_obs[cold_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmax_obs_warm <- tmax_obs[warm_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmin_obs_yearly <- tmin_obs %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

# obs tmin
tmin_obs_cold <- tmin_obs[cold_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmin_obs_warm <- tmin_obs[warm_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

# model tmax
tmax_nospcv_yearly <- tmax_nospcv %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmax_nospcv_cold <- tmax_nospcv[cold_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmax_nospcv_warm <- tmax_nospcv[warm_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

# model tmin
tmin_nospcv_yearly <- tmin_nospcv %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmin_nospcv_cold <- tmin_nospcv[cold_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })

tmin_nospcv_warm <- tmin_nospcv[warm_months, ] %>%
  sapply(function(x){
    time_serie_sample = xts::apply.yearly(x, mean)
    time_serie_sample = as.numeric(time_serie_sample)
    slope = 1:length(time_serie_sample)
    RobustLinearReg::theil_sen_regression(time_serie_sample ~ slope)$coefficients[2]
  })


data_slope <- rbind(
  data.frame(slope_obs = tmax_obs_yearly, slope_model = tmax_nospcv_yearly, var = "Tmax", freq = "Annual"),
  data.frame(slope_obs = tmax_obs_cold, slope_model = tmax_nospcv_cold, var = "Tmax", freq = "Apr.-Sep."),
  data.frame(slope_obs = tmax_obs_warm, slope_model = tmax_nospcv_warm, var = "Tmax", freq = "Oct.-Mar."),
  data.frame(slope_obs = tmin_obs_yearly, slope_model = tmin_nospcv_yearly, var = "Tmin", freq = "Annual"),
  data.frame(slope_obs = tmin_obs_cold, slope_model = tmin_nospcv_cold, var = "Tmin", freq = "Apr.-Sep."),
  data.frame(slope_obs = tmin_obs_warm, slope_model = tmin_nospcv_warm, var = "Tmin", freq = "Oct.-Mar.")
)
data_slope$freq <- factor(data_slope$freq, levels = c("Annual", "Oct.-Mar.", "Apr.-Sep."))

by(data_slope, INDICES = list(data_slope$var, data_slope$freq),
   function(x){
     data.frame(var = unique(x$var), freq = unique(x$freq),
                y = 0.05, x = 0.07,
       openair::modStats(mod = "slope_model", obs = "slope_obs",
                       mydata = x, 
                       statistic = c("MB", "MGE", "IOA"))
       ) %>%
       transform(label = paste("list(italic(d[r])) ==", round(IOA, 2)))
   }
   ) %>% do.call(rbind, .) -> stat_slope_summary



ggplot2::ggplot() + 
  ggplot2::geom_point(data = data_slope,
                      ggplot2::aes(x = slope_obs, y = slope_model)) + 
  geom_smooth(data = data_slope,
              ggplot2::aes(x = slope_obs, y = slope_model), method='lm', formula= y~x, se = FALSE) +
  geom_text(data = stat_slope_summary, aes(x = x, y = y, label = label), parse = TRUE) +
  ggplot2::facet_grid(var~freq) + 
  ggplot2::geom_hline(yintercept=0, linetype="dashed") +
  ggplot2::geom_vline(xintercept=0, linetype="dashed") +
  xlab("Observed (°C/year)") + ylab("Estimated (°C/year)") + 
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.box = 'vertical',
        plot.margin=unit(c(0,0,0,0), "cm"),
        strip.background = ggplot2::element_blank(),
        legend.spacing.x = unit(.01, 'cm'),
        legend.spacing.y = unit(.05, 'cm'),
        strip.text = element_text(size = 11),
        legend.margin = ggplot2::margin(t=0))

ggsave(file.path(".", "paper", "output", "newA_Figure_trend_slope_cross_validation.pdf"),
       device = "pdf",
       dpi = 500, scale = 1,
       width = 7, height = 4, units = "in")
