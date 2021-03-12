make_Normal_coVariables <- function(month_value,
                                    var,
                                    covs_list,
                                    obs)
{
  
  # covs
  covs_list$static
  covs_dynamic <- lapply(covs_list$dynamic, function(x) x[[month_value]])
  
  covs_data <- raster::brick(raster::stack(raster::stack(covs_dynamic),
                                           raster::stack(covs_list$static)))
  # covs_data <- (covs_data - raster::cellStats(covs_data, mean)) / raster::cellStats(covs_data, sd)
  
  # obs
  obs_data <- cbind(obs$xyz, obs$values[[var]][month_value, ])
  obs_data <- obs_data[, ncol(obs_data)]
  names(obs_data) <- c("OBS")
  
  # formula
  formula_lm <- as.formula(paste("OBS ~ ",  paste(names(covs_data), collapse = "+")))
  
  list(covs = covs_data, obs = obs_data, formula_lm = formula_lm)
  
}

make_Anomaly_coVariables <- function(day_date,
                                     var,
                                     covs_list,
                                     obs)
{
  
  # date to month
  month_value <- as.numeric(format(as.Date(day_date), "%m"))
  
  # covs
  covs_list$static
  covs_dynamic <- lapply(covs_list$dynamic, function(x) x[[month_value]])
  
  covs_data <- raster::brick(raster::stack(raster::stack(covs_dynamic),
                                           raster::stack(covs_list$static)))
  # covs_data <- (covs_data - raster::cellStats(covs_data, mean)) / raster::cellStats(covs_data, sd)
  
  # obs
  obs_data <- cbind(obs$xyz, as.numeric(obs$values[[var]][as.Date(day_date)]))
  obs_data <- obs_data[, ncol(obs_data)]
  names(obs_data) <- c("OBS")
  
  # formula
  formula_lm <- as.formula(paste("OBS ~ ",  paste(names(covs_data), collapse = "+")))
  
  list(covs = covs_data, obs = obs_data, formula_lm = formula_lm)
  
}
