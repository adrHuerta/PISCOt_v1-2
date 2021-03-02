# Pairwise SNHT - A relative homogeneity test
# ts_data: Time series (matrix), first column is the target
# alpha: The confidence level for the SNHT test (to detect break point)
# period: Window size for the test statistic in months

# Note: 
# using scaled = TRUE in snht::snht the statistic follows an approximate
# Chi^2 distribution, so in that way it is possible to obtain the critical value of the 
# SHNT test (see https://www.math.uzh.ch/li/index.php?file&key1=38056)
# Ho : there is no change point in the time series

pha_hmg <- function(ts_data,
                    alpha = 0.05,
                    period = 12*5)
{
  
  station_target <- colnames(ts_data)[1]
  
  # distance matrix
  cor_dist <- round(1 - cor(ts_data), 3)
  
  # reshaping data
  base_Data <- zoo::coredata(ts_data)
  base_Data <- base_Data[nrow(base_Data):1, ] # time reversed (to make corrections to the past, previous to the breaking point)
  base_Data <- data.frame(time = 1:dim(base_Data)[1], base_Data)
  base_Data <- reshape2::melt(base_Data, id.vars = "time", variable.name = "location", value.name = "data")
  
  # pha
  pha_result <- snht::pairwiseSNHT(data = base_Data, 
                                   dist = cor_dist, 
                                   k = dim(cor_dist)[1]-1, 
                                   period = period,
                                   robust = FALSE,
                                   scaled = TRUE, 
                                   crit = qchisq(1 - alpha/(length(ts_data) - period*2), df = 1))
  
  target_hmg <- pha_result$data[pha_result$data$location == station_target,]$data
  target_hmg <- target_hmg[length(target_hmg):1] # time reversed (normal)
  target_hmg <- xts::xts(target_hmg, time(ts_data))
  
  # breaks
  breaks <- pha_result$breaks[pha_result$breaks$location == station_target, ]
  
  if(is.null(breaks)){
    
    breaks <- data.frame(point_break = NA, shift_break = NA, 
                         station_target = station_target, snht = "pairwise")
    
  } else if(nrow(breaks) == 0){
    
    breaks <- data.frame(point_break = NA, shift_break = NA, 
                         station_target = station_target, snht = "pairwise")
    
  } else {
    
    breaks <- data.frame(point_break = rev(time(ts_data))[breaks$time],      # time reversed 
                         shift_break = breaks$shift,                         # represent the added/subtracted (sign) from the original data
                                                                             # (original-hmg)
                         station_target = station_target, snht = "pairwise")
    
  }
  
  
  return(
    list(original = ts_data[, station_target],
         hmg = target_hmg,
         breaks = breaks)
  )
  
}
