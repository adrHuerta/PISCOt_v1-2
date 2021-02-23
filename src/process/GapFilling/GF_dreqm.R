# detrended non-parametric quantile mapping using robust empirical quantiles 

daily_varying_anom_qmap <- function(ts_obs,
                                    ts_model,
                                    window_c = 7)
{
  # dataset
  ts_data <- cbind(ts_obs, ts_model)
  colnames(ts_data) <- c("obs", "model")
  
  # building daily climatology, centering at 01-01
  dailyVar <- sort(unique(format(time(ts_data), format = "%m-%d")))
  tail0 <- dailyVar[(length(dailyVar) - window_c + 1):length(dailyVar)]
  tail1 <- dailyVar[1:window_c]
  dailyVar <- c(tail0, dailyVar, tail1)
  
  mapply(function(x, y){
    dailyVar[x:y]
  }, x = 1:366, y = (window_c*2+1):length(dailyVar), SIMPLIFY = FALSE) -> dailyVar
  
  # applying qmap in standardized time series for each chunk of the daily climatology
  lapply(dailyVar, function(daily_var_i){
    
    # chunk data
    ts_data_i_abs <- ts_data[format(time(ts_data), format = "%m-%d") %in% daily_var_i]
    # centroid date
    ts_data_i_abs_centroid <- ts_data_i_abs[format(time(ts_data_i_abs), format = "%m-%d") %in% daily_var_i[window_c+1]] 
    # mean value and anomaly
    ts_data_i_abs_cc <- zoo::coredata(ts_data_i_abs[complete.cases(ts_data_i_abs),])
    ts_data_i_abs_cc_mean <- colMeans(ts_data_i_abs_cc)
    ts_data_i_cc_anom <- ts_data_i_abs_cc_mean - ts_data_i_abs_cc
    
    # fitting anomaly obs/model values
    qm.fit <- qmap::fitQmapRQUANT(ts_data_i_cc_anom[, "obs"], ts_data_i_cc_anom[, "model"],
                                  qstep = 0.1, nboot = 10, wet.day = FALSE)
    # applying in anomaly values and reversing to absolute values
    model_cc <- qmap::doQmapRQUANT(ts_data_i_abs_cc_mean["obs"] - ts_data_i_abs_centroid[, "model"], qm.fit, type = "tricub")
    xts::xts(ts_data_i_abs_cc_mean["obs"] - model_cc, time(ts_data_i_abs_centroid))
    
  }) -> dailyVar_qmap
  
  round(
    do.call("rbind", dailyVar_qmap),
    2)
}