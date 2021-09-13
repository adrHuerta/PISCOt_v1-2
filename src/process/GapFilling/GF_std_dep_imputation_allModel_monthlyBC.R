# std_dep_imputation_daily - daily imputation using standardized values + bias-correction (departure-based) 
# nested function - main routine
# stat_data: Time series (matrix), first column is the target

# Note:
# The approach follows the steps of https://link.springer.com/article/10.1007/s00704-017-2082-0 
# which is similar to https://journals.ametsoc.org/view/journals/apme/34/2/1520-0450-34_2_371.xml
# But, it is applied for each day (except for 02-28 where 02-29 is added), 
# so it preserves the daily climatology
# and applies a bias-correction to the model data as recommended
# by https://journals.ametsoc.org/view/journals/clim/aop/JCLI-D-21-0067.1/JCLI-D-21-0067.1.xml

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
    
    rest_all_orig[[j]] <- xts::xts(Tt, time(data_base_w))
    rest_all_model[[j]] <- xts::xts(Ttj, time(data_base_w))
  }
  
  orig_ts = do.call("rbind", rest_all_orig)
  model_ts = do.call("rbind", rest_all_model)
  
  # model ts with no data from dates of obs
  obs_mod_df <- data.frame(obs = as.numeric(orig_ts),
                           mod = as.numeric(model_ts),
                           mod_cc = as.numeric(model_ts),
                           time = time(orig_ts))
  
  obs_mod_df_mod <- transform(obs_mod_df,
                              mod2cc = ifelse(is.na(obs) & !is.na(mod), 1, 0))
  
  # to numeric
  orig_ts_v <- obs_mod_df_mod[, c("obs", "time")]
  model_ts_no_with_obs_v <- obs_mod_df_mod[,c("mod", "time")]
  model_ts_no_with_obs_v <- model_ts_no_with_obs_v[complete.cases(model_ts_no_with_obs_v), ]
  model_ts_no_with_obs_v_noNA <- model_ts_no_with_obs_v[,c("mod")]
  #orig_ts_v[orig_ts_v >= quantile(orig_ts_v, 1, na.rm = TRUE)] <- NA
  #orig_ts_v[orig_ts_v <= quantile(orig_ts_v, 0, na.rm = TRUE)] <- NA
  orig_ts_v <- orig_ts_v[!is.na(orig_ts_v), ]
  
  if(length(model_ts_no_with_obs_v$mod) < 3){
    
    obs_mod_df_mod[, "mod_cc"] <- obs_mod_df_mod$mod_cc
    
  } else {
    
    # bias-correction of model ts with no data from dates of obs (model corrected)
    # qm_fit <- qmap::fitQmapRQUANT(orig_ts_v, model_ts_no_with_obs_v, qstep = 0.01, nboot = 1, wet.day = FALSE)
    # model_cc <- qmap::doQmapRQUANT(model_ts_no_with_obs_v, qm_fit, type = "tricub")
    # obs_mod_df_mod[,"mod_cc"] <- model_cc
    
    model_cc <- eqm_qmap_m(his_mod = model_ts_no_with_obs_v, his_obs = orig_ts_v)
    obs_mod_df_mod[match(model_ts_no_with_obs_v$time, obs_mod_df_mod$time),"mod_cc"] <- model_cc
    
  }
  
  # 
  obs_mod_df_mod <- transform(obs_mod_df_mod,
                              new = ifelse(is.na(obs) & is.numeric(mod_cc), mod_cc, obs),
                              new_no_cc = ifelse(is.na(obs) & is.numeric(mod), mod, obs))
  obs_mod_df_mod$new[obs_mod_df_mod$new > max(obs_mod_df_mod$obs, na.rm = TRUE)] <- max(obs_mod_df_mod$obs, na.rm = TRUE)
  obs_mod_df_mod$new[obs_mod_df_mod$new < min(obs_mod_df_mod$obs, na.rm = TRUE)] <- min(obs_mod_df_mod$obs, na.rm = TRUE)
  obs_mod_df_mod$new_no_cc[obs_mod_df_mod$new_no_cc > max(obs_mod_df_mod$obs, na.rm = TRUE)] <- max(obs_mod_df_mod$obs, na.rm = TRUE)
  obs_mod_df_mod$new_no_cc[obs_mod_df_mod$new_no_cc < min(obs_mod_df_mod$obs, na.rm = TRUE)] <- min(obs_mod_df_mod$obs, na.rm = TRUE)
  
  #
  to_output <- xts::xts(obs_mod_df_mod[, c("obs", "mod", "mod_cc", "new", "new_no_cc")], 
                        as.Date(obs_mod_df_mod[, "time"])) 
  colnames(to_output) <- c("original", "model", "model_bc", "filled", "filled_no_bc")
  
  round(to_output, 2)
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
    
    setNames(cbind(stat_data[, 1], stat_data[, 1], stat_data[, 1], stat_data[, 1], stat_data[, 1]), 
             c("original", "model", "model_bc", "filled", "filled_no_bc"))
    
  } else {
    
    std_dep_imputation_daily(stat_data)
  }
  
}

eqm_qmap_m <- function(his_mod,
                       his_obs)
{
  
  do.call(rbind,
          lapply(c("01", "02", "03",
                   "04", "05", "06",
                   "07", "08", "09",
                   "10", "11", "12"), 
                 function(x){
                   
                   his_obs_m <- his_obs[format(his_obs$time, "%m") %in% x, ]
                   his_mod_m <- his_mod[format(as.Date(his_mod$time), "%m") %in% x, ]
                   
                   eqm_model <- qmap::fitQmapQUANT(his_obs_m$obs,
                                                   his_mod_m$mod,
                                                   qstep = 0.01, 
                                                   nboot = 1, 
                                                   wet.day = FALSE)
                   
                   fut_mod_m_c <- qmap::doQmapQUANT(his_mod_m$mod, 
                                                    eqm_model, 
                                                    type = "tricub")
                   
                   response <- his_mod_m
                   response[, "mod"] = round(fut_mod_m_c, 2)
                   response
                 }
          )
  ) -> fut_mod_c
  
  fut_mod_c[order(fut_mod_c$time),]
  
}
