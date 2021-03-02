# Single SNHT - An absolute homogeneity test
# ts_data: Single time serie
# alpha: The confidence level for the SNHT test (to detect break point)
# period: Window size for the test statistic in months

# Note: 
# using scaled = TRUE in snht::snht the statistic follows an approximate
# Chi^2 distribution, so in that way it is possible to obtain the critical value of the 
# SHNT test (see https://www.math.uzh.ch/li/index.php?file&key1=38056)
# Ho : there is no change point in the time series

snht_hmg <- function(ts_data,
                     alpha = 0.05,
                     period = 12*5)
{
  
  station_target <- colnames(ts_data)[1]
  base_Data <- zoo::coredata(ts_data)
  base_Data <- data.frame(time_serie = as.numeric(base_Data),
                          time = 1:length(base_Data))
  
  
  snht_result <- snht::snht(data = base_Data$time_serie,
                            period = period,
                            time = base_Data$time,
                            robust = FALSE,
                            scaled = TRUE)
  
  crit <- qchisq(1 - alpha/(length(ts_data) - period*2), df = 1)
  
  if(any(snht_result$score[!is.na(snht_result$score)] > crit)) {
    
    point_break <- snht_result$time[match(max(snht_result$score, na.rm = TRUE), snht_result$score)]
    current_time <- base_Data$time_serie[(point_break+1):length(base_Data$time)]
    previous_time <- base_Data$time_serie[1:(point_break)]
    shift_break <- mean(current_time, na.rm = TRUE) - mean(previous_time, na.rm = TRUE)
    target_hmg <- c(previous_time + shift_break, current_time)
    target_hmg <- xts::xts(target_hmg, time(ts_data))
    
  } else { 
    
    point_break <- NA
    shift_break <- NA
    target_hmg <- ts_data[, station_target]
    
  }
  
  
  # breaks
  return(
    list(original = ts_data[, station_target],
         hmg = target_hmg,
         breaks = data.frame(point_break = time(ts_data)[point_break], 
                             shift_break = -shift_break,               # represent the added/subtracted (sign) from the original data
                                                                       # (original-hmg)
                             station_target = station_target,
                             snht = "simple"))
  )
  
  
}