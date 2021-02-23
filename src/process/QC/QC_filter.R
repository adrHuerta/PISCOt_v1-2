get_stations_from_filter <- function(data_list,
                                     n_days_by_month = 20,
                                     n_months_by_year = 12,
                                     length_of_years = 5,
                                     data_range = "1981/2019")
{
  
  tmax_list <- lapply(data_list, function(x) x[, "tmax"])
  tmax_stations <- sapply(tmax_list, function(x){
    # length of data
    do.call(cbind,
            lapply(x, function(xc){
              xts::apply.monthly(xc, function(z) ifelse(sum(!is.na(z)) >= n_days_by_month, 1, 0)) -> monthly_ts
              xts::apply.yearly(monthly_ts, sum) -> annual_ts
              annual_ts[annual_ts < n_months_by_year] <- NA
              annual_ts
            })) -> length_data
    
    length_data <- length_data[complete.cases(length_data),]
    if(nrow(length_data) != 0) length_data <- length_data[data_range]
    
    # it has at least N repeated values for each day 
    if(nrow(length_data) != 0){
      
      length_of_daily_cycle <- data.frame(time = time(na.omit(x)), month_day = format(time(na.omit(x)), "%m-%d"))
      length_of_daily_cycle <- aggregate(length_of_daily_cycle, by = list(dailyCycle = length_of_daily_cycle$month_day), length)
      pass_cycle <- any(length_of_daily_cycle[-match("02-29", length_of_daily_cycle$dailyCycle), "time"] >= 5)
      
    } else {
      pass_cycle <- NA
    }
    
    
    if((nrow(length_data) >= length_of_years) & isTRUE(pass_cycle)){
      return(x)
    } else {
      NULL
    }
  })
  tmax_stations <- names(tmax_stations[!sapply(tmax_stations, is.null)])
  
  tmin_list <- lapply(data_list, function(x) x[, "tmin"])
  tmin_stations <- sapply(tmin_list, function(x){
    
    # length of data
    
    do.call(cbind,
            lapply(x, function(xc){
              xts::apply.monthly(xc, function(z) ifelse(sum(!is.na(z)) >= n_days_by_month, 1, 0)) -> monthly_ts
              xts::apply.yearly(monthly_ts, sum) -> annual_ts
              annual_ts[annual_ts < n_months_by_year] <- NA
              annual_ts
            })) -> length_data
    
    length_data <- length_data[complete.cases(length_data),]
    if(nrow(length_data) != 0) length_data <- length_data[data_range]
    
    # it has at least N repeated values for each day 
    
    if(nrow(length_data) != 0){
      
      length_of_daily_cycle <- data.frame(time = time(na.omit(x)), month_day = format(time(na.omit(x)), "%m-%d"))
      length_of_daily_cycle <- aggregate(length_of_daily_cycle, by = list(dailyCycle = length_of_daily_cycle$month_day), length)
      pass_cycle <- any(length_of_daily_cycle[-match("02-29", length_of_daily_cycle$dailyCycle), "time"] >= 5)
      
    } else {
      pass_cycle <- NA
    }
    
    if((nrow(length_data) >= length_of_years) & isTRUE(pass_cycle)){
      return(x)
    } else {
      NULL
    }
  })
  tmin_stations <- names(tmin_stations[!sapply(tmin_stations, is.null)])
  
  stations_filter <- Reduce(intersect, list(tmax_stations, tmin_stations))
  stations_filter
  
}