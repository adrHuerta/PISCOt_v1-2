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
  covs_data <- (covs_data - raster::cellStats(covs_data, mean)) / raster::cellStats(covs_data, sd)
  
  # obs
  obs_data <- cbind(obs$xyz, obs$values[[var]][month_value, ])
  obs_data <- obs_data[, ncol(obs_data)]
  names(obs_data) <- c("Temp")
  
  # formula
  formula_lm <- as.formula(paste("Temp ~ ",  paste(names(covs_data), collapse = "+")))
  
  list(covs = covs_data, obs = obs_data, formula_lm = formula_lm)
  
}
