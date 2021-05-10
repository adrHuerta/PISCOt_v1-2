# std_dep_imputation_daily - daily imputation using standardized values (departure-based) 
# nested function - main routine
# stat_data: Time series (matrix), first column is the target

# Note:
# The approach follows the steps of https://link.springer.com/article/10.1007/s00704-017-2082-0 
# which is similar to https://journals.ametsoc.org/view/journals/apme/34/2/1520-0450-34_2_371.xml
# But, it is applied for each day (except for 02-28 where 02-29 is added), so
# it preserves the daily climatology

std_dep_imputation_daily <- function(stat_data)
{
  
  # daily climatology
  dailyVar <- sort(unique(format(time(stat_data), format = "%m-%d")))
  dailyVar <- as.list(dailyVar[dailyVar != "02-29"])
  dailyVar[[match("02-28", dailyVar)]] <- c("02-28", "02-29") # making 02-29 "part of" 02-28
 
  rest_all_orig <- list()
  rest_all_model <- list()
  rest_all_filled <- list()
  
  for(j in 1:length(dailyVar))
  {
    
    data_base_w <- stat_data[ format(time(stat_data), "%m-%d") %in% dailyVar[[j]] ]
    
    Tt <- zoo::coredata(data_base_w[, 1]) # target
    Tj <- zoo::coredata(data_base_w[, -1]) # neighbours
    Wj <- apply(Tj, 2, function(x) ((1 + 0.01) / (sd(Tt - x, na.rm = TRUE) + 0.01 ))^2 ) # adding 0.01 to avoid artifacts
    Zj <- apply(Tj, 2, function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))

    Ttj <- Tt
    for(i in 1:length(Tt)){
      
      Zj_no_na <- Zj[i,][!is.na(Zj[i,])]
      Wj_no_na <- Wj[!is.na(Zj[i,])]
      
      Zt <- sum(Zj_no_na*Wj_no_na)/sum(Wj_no_na)
      Ttj_value <- round(Zt*sd(Tt, na.rm = TRUE) + mean(Tt, na.rm = TRUE), 2)
      Ttj[i] <- ifelse(is.nan(Ttj_value), NA, Ttj_value)
      
    }
    
    lower_max_value <- round(min(Tt, na.rm = TRUE), 2)
    upper_max_value <- round(max(Tt, na.rm = TRUE), 2)
    Ttj[!is.na(Ttj)][Ttj[!is.na(Ttj)] < lower_max_value] <- lower_max_value
    Ttj[!is.na(Ttj)][Ttj[!is.na(Ttj)] > upper_max_value] <- upper_max_value
    
    data_base_filled <- data.frame(Tt = as.numeric(Tt), Ttj = as.numeric(Ttj))
    data_base_filled <- transform(data_base_filled, new = ifelse(is.na(Tt) & is.numeric(Ttj), Ttj, Tt))
    
    rest_all_orig[[j]] <- xts::xts(data_base_filled$Tt, time(data_base_w))
    rest_all_model[[j]] <- xts::xts(data_base_filled$Ttj, time(data_base_w))
    rest_all_filled[[j]] <- xts::xts(data_base_filled$new, time(data_base_w))
    
  }
  
  do.call("cbind",
          list(original = do.call("rbind", rest_all_orig),
               model = do.call("rbind", rest_all_model),
               filled = do.call("rbind", rest_all_filled))
  )
}


# -------------------------------------------------------------------------------
# std_dep_imputation - daily imputation using standardized values (departure-based) 
# stat_data: Time series (matrix), first column is the target

# Note: two conditions based of stat_data
# i) If stat_data has one single time serie or if it has full NAs (even in a matrix)
# the output is same time serie 
# ii) otherwise, std_dep_imputation_daily is applied


std_dep_imputation <- function(stat_data)
{
  
  if((length(colnames(stat_data)) < 2) | all(!is.na(stat_data[,1]))){
    
    setNames(cbind(stat_data[, 1], stat_data[, 1], stat_data[, 1]), c("original", "model", "filled"))
    
  } else {
    
    std_dep_imputation_daily(stat_data)
  }
  
}
