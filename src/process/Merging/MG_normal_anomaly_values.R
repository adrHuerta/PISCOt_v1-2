# without seasonal variability?
get_monthly_normals <- function(daily_time_serie,
                                as_xts = FALSE)
{

  mean_values <- sapply(1:12, function(x) round(mean(daily_time_serie[xts::.indexmon(daily_time_serie) %in% (x - 1)], na.rm = TRUE), 2))
  mean_ts <- daily_time_serie
  
  for(i in 1:12){
    mean_ts[xts::.indexmon(mean_ts) %in% (i - 1)] <- mean_values[i]
  }
  
  if(isTRUE(as_xts)){
    
    mean_ts
    
  } else {
    
    mean_values
    
  }


}

get_anomaly_values <- function(daily_time_serie)
{
  
  mean_ts <- get_monthly_normals(daily_time_serie = daily_time_serie, as_xts = TRUE)
  daily_time_serie - mean_ts
  
}

get_anomaly_values2 <- function(daily_time_serie,
                                monthly_values)
{
  
  mean_ts <- daily_time_serie
  for(i in 1:12){
    mean_ts[xts::.indexmon(mean_ts) %in% (i - 1)] <- monthly_values[i]
  }
  
  round(daily_time_serie - mean_ts, 2)
  
}