RK <- function(obs_cov_data,
               resFitting = 10)
  {
  
  # lm fitting model
  lm_coefs <- LR_model_fitting(obs_cov_data = obs_cov_data,
                               resFitting = resFitting)
  # applying coef gwr
  model_grid <- apply_coef_model(covs = obs_cov_data$covs,
                                 coef_model = lm_coefs$coefs)
  
  # residual kriging interpolation
  residual_grid <- OK_interpolation(obs_point_data = obs_cov_data$obs,
                                    model_grid_data = model_grid,
                                    resFitting = resFitting/2)
  # merging
  model_grid + residual_grid
  
  }

LR_model_fitting <- function(obs_cov_data,
                             resFitting)
{
  
  cov_data <- raster::aggregate(obs_cov_data$covs, resFitting)
  p_cov <- raster::extract(cov_data, obs_cov_data$obs, cellnumber = FALSE, sp = TRUE)
  newpts <- rasterToPoints(cov_data[[1]]); newpts <- newpts[, -3]
  
  lr_model <- lm(obs_cov_data$formula_lm,
                 data = p_cov)
  
  list(coefs = lr_model$coefficients)
  
}

apply_coef_model <- function(covs,
                             coef_model)
{
  
  MODEL_grid <- coef_model[names(coef_model)[1]] +
    sum(covs * coef_model[names(covs)])
  MODEL_grid <- round(MODEL_grid, 2)
  
  names(MODEL_grid) <- "MODEL"
  MODEL_grid
  
}

OK_interpolation <- function(obs_point_data,
                             model_grid_data,
                             resFitting = 10)
{
  
  # getting residuals
  temp_model_ag <- raster::aggregate(model_grid_data, resFitting)
  residual_sp <- extract(temp_model_ag,
                         obs_point_data, cellnumber = FALSE, sp = TRUE)
  residual_sp$residual <- residual_sp$OBS - residual_sp$MODEL
  
  # variogram autofit
  variogram_fit <- automap::autofitVariogram(residual ~ 1, input_data = residual_sp)
  
  # ok
  gridded_location <- as(temp_model_ag, 'SpatialGrid')
  gs <- gstat::gstat(formula = residual ~ 1, 
                     locations = residual_sp, 
                     model = variogram_fit$var_model)
  kp <- raster::predict(gs, gridded_location)
  response <- raster::disaggregate(raster::brick(kp)[["var1.pred"]], 
                                   resFitting, 
                                   method = "bilinear")
  response <- raster::crop(response, model_grid_data)
  
  round(response, 2)
}