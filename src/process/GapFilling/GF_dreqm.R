# daily_varying_anom_qmap - Anomaly non-parametric quantile mapping using robust empirical quantiles
# ts_obs : time serie of observed data
# ts_model : time serie of model data
# window_c : number of days after/previous date of correction

# Note:
# The approach is similar to https://journals.ametsoc.org/view/journals/clim/28/17/jcli-d-14-00754.1.xml and
# https://journals.ametsoc.org/view/journals/eint/21/3/ei-d-16-0025.1.xml
# i) the data is divided into chunks (366), each one has a center date 
# that represent the daily climatology with +/- window_c dates, 
# so a chunk has 2*window_c + 1 values 
# ii) compute anomalies based on the mean values of OBS/MODEL data and fit the qmodel
# iii) apply the qmodel using the anomaly of MODEL in the center date (anomaly based on the mean of OBS) 
# iv) add iii) to the mean OBS value to get the absolute temperature values

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
  
  # applying qmap in standardized (anomalies) time series for each chunk of the daily climatology
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