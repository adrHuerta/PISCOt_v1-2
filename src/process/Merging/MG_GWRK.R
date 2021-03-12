GWR_model_fitting <- function(obs_cov_data,
                              resFitting = 10,
                              getCoefs = FALSE)
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
                                    parallel.method = "cluster")
    
    Intercept <- raster::rasterFromXYZ(gwr_model$SDF[, "Intercept"])
    LST_coef <- raster::rasterFromXYZ(gwr_model$SDF[, names(cov_data)[1]])
    CC_coef <- raster::rasterFromXYZ(gwr_model$SDF[, names(cov_data)[2]])
    DEM_coef <- raster::rasterFromXYZ(gwr_model$SDF[, names(cov_data)[3]])
    X_coef <- raster::rasterFromXYZ(gwr_model$SDF[, names(cov_data)[4]])
    Y_coef <- raster::rasterFromXYZ(gwr_model$SDF[, names(cov_data)[5]])
    
    gwr_coefs <- raster::brick(Intercept, LST_coef, CC_coef, DEM_coef, X_coef, Y_coef)
    names(gwr_coefs) <- c("Intercept", names(cov_data))
    
    list(coefs = raster::disaggregate(gwr_coefs, 10),
         bw = bw)
    
  } else {
    
    list(coefs = NULL,
         bw = bw)
    
  }
  
  
}