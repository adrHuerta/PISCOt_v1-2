# daily_climatology_filling - Filling a time series with daily climatology values
# ts_data: A single time series

# Note:
# Only 02-28 values is used to fill two dates: itself and 02-29 

daily_climatology_filling <- function(ts_data)
{
  
  daily_clim <- ts_data[!(format(time(ts_data), "%m-%d") %in% "02-29")]
  daily_clim_values <- unique(format(time(daily_clim), "%m-%d"))
  #sapply(daily_clim_values, function(z) round(mean(ts_data[format(time(ts_data), "%m-%d") %in% z], na.rm = TRUE),1)) %>% plot()
  daily_clim <- round(apply(matrix(daily_clim, byrow = TRUE, ncol = 365), 2, mean, na.rm = TRUE), 2)
  #daily_clim %>% plot()
  
  for(i_day in c(daily_clim_values, "02-29")){
    
    ts_data[is.na(ts_data)][format(time(ts_data[is.na(ts_data)]), "%m-%d") %in% i_day] <- 
      rep(daily_clim[match(i_day, daily_clim_values)] , 
          length(ts_data[is.na(ts_data)][format(time(ts_data[is.na(ts_data)]), "%m-%d") %in% i_day]))
    
    if(i_day == "02-29"){
      
      ts_data[is.na(ts_data)][format(time(ts_data[is.na(ts_data)]), "%m-%d") %in% i_day] <- 
        rep(daily_clim[match("02-28", daily_clim_values)] , 
            length(ts_data[is.na(ts_data)][format(time(ts_data[is.na(ts_data)]), "%m-%d") %in% i_day]))
      
      
    }
  }
  
  ts_data
  
}