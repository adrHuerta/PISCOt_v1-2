GWRK <- function(obs_cov_data,
                 resFitting = 10,
                 extentArea = NULL)
{
  
  # gwr fitting model
  gwr_bw_coefs <- GWR_model_fitting(obs_cov_data = obs_cov_data,
                                    resFitting = resFitting)
  # applying coef gwr
  model_grid <- apply_coef_model(covs = obs_cov_data$covs,
                                 coef_model = gwr_bw_coefs$coefs)
  
  # residual kriging interpolation
  if(isTRUE(is.null(extentArea))){
    
    
    residual_grid <- OK_interpolation(obs_point_data = obs_cov_data$obs,
                                      model_grid_data = model_grid,
                                      resFitting = resFitting/2)
    
    model_grid + residual_grid
    
    
  } else {
    
    
    residual_grid <- OK_interpolation2(obs_point_data = obs_cov_data$obs,
                                       model_grid_data = model_grid,
                                       resFitting = resFitting/2,
                                       extentArea = extentArea)
    
  }
  
  # merging
  suppressWarnings(model_grid + residual_grid)
  
}


GWR_model_fitting <- function(obs_cov_data,
                              resFitting,
                              getCoefs = TRUE)
{
  
  cov_data <- raster::aggregate(obs_cov_data$covs, resFitting)
  p_cov <- raster::extract(cov_data, obs_cov_data$obs, cellnumber = FALSE, sp = TRUE)
  newpts <- rasterToPoints(cov_data[[1]]); newpts <- newpts[, -3]
  
  
  bw <- GWmodel::bw.gwr(obs_cov_data$formula_lm,
                        data = p_cov,
                        approach = "AICc",
                        kernel = "bisquare",
                        adaptive = TRUE,
                        longlat = TRUE)
  
  if(isTRUE(getCoefs)){
    
    gwr_model <- GWmodel::gwr.basic(obs_cov_data$formula_lm, 
                                    data = p_cov, 
                                    bw = bw,  
                                    kernel = "bisquare", 
                                    adaptive = TRUE,
                                    longlat = TRUE,
                                    regression.points = newpts, 
                                    parallel.method = FALSE)
    
    gwr_coefs <- brick(
      lapply(c("Intercept", names(cov_data)),
             function(x){raster::rasterFromXYZ(gwr_model$SDF[, x])})
      )
    names(gwr_coefs) <- c("Intercept", names(cov_data))
    gwr_coefs <- crop(raster::disaggregate(gwr_coefs, 10, method = "bilinear"), obs_cov_data$covs)
    raster::projection(gwr_coefs) <- raster::projection(obs_cov_data$covs)
    
    list(coefs = gwr_coefs,
         bw = bw)
    
  } else {
    
    list(coefs = NULL,
         bw = bw)
    
  }
  
  
}


apply_coef_model <- function(covs,
                             coef_model)
{
  
  MODEL_grid <- coef_model[["Intercept"]] +
    sum(covs * coef_model[[names(covs)]])
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

OK_interpolation2 <- function(obs_point_data,
                              model_grid_data,
                              resFitting = 10,
                              extentArea)
{
  
  # getting residuals
  temp_model_ag <- raster::aggregate(model_grid_data, resFitting)
  residual_sp <- extract(temp_model_ag,
                         obs_point_data, cellnumber = FALSE, sp = TRUE)
  residual_sp$residual <- residual_sp$OBS - residual_sp$MODEL
  
  # variogram autofit
  variogram_fit <- automap::autofitVariogram(residual ~ 1, input_data = residual_sp)
  
  # ok
  gridded_location <- as(raster::crop(temp_model_ag, extentArea), 'SpatialGrid')
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