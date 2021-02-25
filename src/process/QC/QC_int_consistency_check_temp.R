# inCons_check - Internal consistency check (Only performed to temperature data)
# xts_obs : a xts matrix (tmax, tmin)

# Note: 
# Consistency among variables:
# i) Tmax <= Tmin
# ii) Tmax = Tmin = 0
 
inCons_check <- function(xts_obj)
{
  
  tmax = xts_obj[, "tmax"]
  tmin = xts_obj[, "tmin"]
  
  zoo::index(
    na.omit(ifelse(tmin >= tmax, 1, NA)
            )
    ) -> non_QC_dates
  
  non_qc <- rbind(data.frame(date = zoo::index(xts_obj[, "tmax"][non_QC_dates]),
                             var = rep("tmax", length(xts_obj[, "tmax"][non_QC_dates]))
                             ),
                  data.frame(date = zoo::index(xts_obj[, "tmin"][non_QC_dates]), 
                             var = rep("tmin", length(xts_obj[, "tmin"][non_QC_dates]))
                             )
                  )
  
  xts_obj$tmax[non_QC_dates] <- NA
  xts_obj$tmin[non_QC_dates] <- NA
  
  return(list(qc = xts_obj,
              non_qc = non_qc))
  
}