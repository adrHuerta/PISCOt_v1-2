## 2. Extreme value check
# Only performed to temperature data
# xts_obj: a xts dataframe
# tmax_lim: maximum and minimum physical limits for tmax
# tmin_lim: maximum and minimum physical limits for tmin
# ext_lim_factor: weight value that is used to seek for outliers : Q25 - 4*IRQ < Temp | Q75 + IRQ*4 > Temp

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

# extVal_check <- function(xts_obj,
#                          tmax_lim = c(60, -10),
#                          tmin_lim = c(40, -30),
#                          ext_lim_factor = 3, #
#                          Nino_years = c("1982","1983","1997","1998"),
#                          ext_lim_factor_Nino = 3.5)
# {
#   
#   xts_obj_tmax_normal <- xts_obj$tmax
#   xts_obj_tmax_normal <- xts_obj_tmax_normal[!(format(time(xts_obj_tmax_normal), "%Y") %in% Nino_years)]
#   xts_obj_tmin_normal <- xts_obj$tmin
#   xts_obj_tmin_normal <- xts_obj_tmin_normal[!(format(time(xts_obj_tmin_normal), "%Y") %in% Nino_years)]
#   
#   xts_obj_tmax_No_normal <- xts_obj$tmax
#   xts_obj_tmax_No_normal <- xts_obj_tmax_No_normal[(format(time(xts_obj_tmax_No_normal), "%Y") %in% Nino_years)]
#   xts_obj_tmin_No_normal <- xts_obj$tmin
#   xts_obj_tmin_No_normal <- xts_obj_tmin_No_normal[(format(time(xts_obj_tmin_No_normal), "%Y") %in% Nino_years)]
#   
#   tmax_filter_l = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmax[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .25, na.rm = TRUE) - ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
#     
#     })
#   tmax_filter_h = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmax[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .75, na.rm = TRUE) + ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
#     
#   })
#   
#   tmin_filter_l = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmin[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .25, na.rm = TRUE) - ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
#     
#   })
#   tmin_filter_h = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmin[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .75, na.rm = TRUE) + ext_lim_factor*IQR(xts_obj_sample, na.rm = TRUE)
#     
#   })
# 
#   do.call(c,
#           sapply(1:12,
#                  function(z){
#            xts_obj_sample <- xts_obj_tmax_normal[as.numeric(format(time(xts_obj_tmax_normal), "%m")) == z]
#            zoo::index(
#              xts_obj_sample[xts_obj_sample >= tmax_lim[1] |
#                             xts_obj_sample <= tmax_lim[2] |
#                             xts_obj_sample <= tmax_filter_l[z] |
#                             xts_obj_sample >= tmax_filter_h[z] ]
#            )
#          })
#          ) -> non_QC_tmax_Normal
#   
#   do.call(c,
#           sapply(1:12,
#                  function(z){
#            xts_obj_sample <- xts_obj_tmin_normal[as.numeric(format(time(xts_obj_tmin_normal), "%m")) == z]
#            zoo::index(
#              xts_obj_sample[xts_obj_sample >= tmin_lim[1] |
#                               xts_obj_sample <= tmin_lim[2] |
#                               xts_obj_sample <= tmin_filter_l[z] |
#                               xts_obj_sample >= tmin_filter_h[z] ]
#            )
#          })
#          ) -> non_QC_tmin_Normal
#   
#   
#   tmax_filter_l_nino = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmax[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .25, na.rm = TRUE) - ext_lim_factor_Nino*IQR(xts_obj_sample, na.rm = TRUE)
#     
#   })
#   tmax_filter_h_nino = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmax[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .75, na.rm = TRUE) + ext_lim_factor_Nino*IQR(xts_obj_sample, na.rm = TRUE)
#     
#   })
#   
#   tmin_filter_l_nino = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmin[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .25, na.rm = TRUE) - ext_lim_factor_Nino*IQR(xts_obj_sample, na.rm = TRUE)
#     
#   })
#   tmin_filter_h_nino = sapply(1:12, function(z){
#     
#     xts_obj_sample <- xts_obj$tmin[as.numeric(format(time(xts_obj), "%m")) == z]
#     quantile(xts_obj_sample, .75, na.rm = TRUE) + ext_lim_factor_Nino*IQR(xts_obj_sample, na.rm = TRUE)
#     
#   })
#   
#   do.call(c,
#           sapply(1:12,
#                  function(z){
#                    xts_obj_sample <- xts_obj_tmax_No_normal[as.numeric(format(time(xts_obj_tmax_No_normal), "%m")) == z]
#                    zoo::index(
#                      xts_obj_sample[xts_obj_sample >= tmax_lim[1] |
#                                       xts_obj_sample <= tmax_lim[2] |
#                                       xts_obj_sample <= tmax_filter_l_nino[z] |
#                                       xts_obj_sample >= tmax_filter_h_nino[z] ]
#                    )
#                  })
#   ) -> non_QC_tmax_Nino
#   
#   do.call(c,
#           sapply(1:12,
#                  function(z){
#                    xts_obj_sample <- xts_obj_tmin_No_normal[as.numeric(format(time(xts_obj_tmin_No_normal), "%m")) == z]
#                    zoo::index(
#                      xts_obj_sample[xts_obj_sample >= tmin_lim[1] |
#                                       xts_obj_sample <= tmin_lim[2] |
#                                       xts_obj_sample <= tmin_filter_l_nino[z] |
#                                       xts_obj_sample >= tmin_filter_h_nino[z] ]
#                    )
#                  })
#   ) -> non_QC_tmin_Nino
#   
#   
#   non_QC_tmax <- c(non_QC_tmax_Normal, non_QC_tmax_Nino)
#   non_QC_tmin <- c(non_QC_tmin_Normal, non_QC_tmin_Nino)
#   
#   non_qc <- rbind(data.frame(date = zoo::index(xts_obj$tmax[non_QC_tmax]), 
#                              var = rep("tmax", length(xts_obj$tmax[non_QC_tmax]))),
#                   data.frame(date = zoo::index(xts_obj$tmin[non_QC_tmin]), 
#                              var = rep("tmin", length(xts_obj$tmin[non_QC_tmin])))
#                   )
#  
#   xts_obj$tmax[non_QC_tmax] <- NA
#   xts_obj$tmin[non_QC_tmin] <- NA
#   
#   return(list(qc = xts_obj,
#               non_qc = non_qc))
#   
# }