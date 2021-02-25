# extVal_check - Extreme value check (Only performed to temperature data)
# xts_obs : a xts matrix (tmax, tmin)
# tmax_lim: maximum and minimum physical limits for tmax
# tmin_lim: maximum and minimum physical limits for tmin
# ext_lim_factor: weight value that is used to seek for outliers : 
# Q25 - ext_lim_factor*IRQ < Temp | Q75 + IRQ*ext_lim_factor > Temp

# Note:
#

extVal_check <- function(xts_obj,
                         tmax_lim = c(60, -10),
                         tmin_lim = c(40, -30),
                         ext_lim_factor = 3.5)
{
  
  tmax_filter_l = sapply(1:12, function(z){
    
    xts_obj_sample <- xts_obj$tmax[as.numeric(format(time(xts_obj), "%m")) == z]
    quantile(xts_obj_sample, .25, na.rm = TRUE) - ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
    
  })
  tmax_filter_h = sapply(1:12, function(z){
    
    xts_obj_sample <- xts_obj$tmax[as.numeric(format(time(xts_obj), "%m")) == z]
    quantile(xts_obj_sample, .75, na.rm = TRUE) + ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
    
  })
  
  tmin_filter_l = sapply(1:12, function(z){
    
    xts_obj_sample <- xts_obj$tmin[as.numeric(format(time(xts_obj), "%m")) == z]
    quantile(xts_obj_sample, .25, na.rm = TRUE) - ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
    
  })
  tmin_filter_h = sapply(1:12, function(z){
    
    xts_obj_sample <- xts_obj$tmin[as.numeric(format(time(xts_obj), "%m")) == z]
    quantile(xts_obj_sample, .75, na.rm = TRUE) + ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
    
  })
  
  do.call(c,
          sapply(1:12,
                 function(z){
                   xts_obj_sample <- xts_obj$tmax[as.numeric(format(time(xts_obj$tmax), "%m")) == z]
                   zoo::index(
                     xts_obj_sample[xts_obj_sample >= tmax_lim[1] |
                                      xts_obj_sample <= tmax_lim[2] |
                                      xts_obj_sample <= tmax_filter_l[z] |
                                      xts_obj_sample >= tmax_filter_h[z] ]
                   )
                 })
  ) -> non_QC_tmax
  
  do.call(c,
          sapply(1:12,
                 function(z){
                   xts_obj_sample <- xts_obj$tmin[as.numeric(format(time(xts_obj$tmin), "%m")) == z]
                   zoo::index(
                     xts_obj_sample[xts_obj_sample >= tmin_lim[1] |
                                      xts_obj_sample <= tmin_lim[2] |
                                      xts_obj_sample <= tmin_filter_l[z] |
                                      xts_obj_sample >= tmin_filter_h[z] ]
                   )
                 })
  ) -> non_QC_tmin
  
  
  non_qc <- rbind(data.frame(date = zoo::index(xts_obj$tmax[non_QC_tmax]), 
                             var = rep("tmax", length(xts_obj$tmax[non_QC_tmax]))),
                  data.frame(date = zoo::index(xts_obj$tmin[non_QC_tmin]), 
                             var = rep("tmin", length(xts_obj$tmin[non_QC_tmin])))
  )
  
  xts_obj$tmax[non_QC_tmax] <- NA
  xts_obj$tmin[non_QC_tmin] <- NA
  
  return(list(qc = xts_obj,
              non_qc = non_qc))
  
}