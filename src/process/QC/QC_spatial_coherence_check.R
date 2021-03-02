# sptCohrc_check - Spatial coherence check
# nghrs_stations : IDs of stations (first is the target, the rest are neighbouring)
# xts_list_database : list() of xts objects (tmax, tmin)
# umb_outlier : threshold to identify outliers

# Note:
# Based on http://onlinelibrary.wiley.com/doi/10.1002/joc.1850/
# Only done where area highly amount of stations (jungle may is not posible),
# If nghrs_stations has a size of one, nothing is done

sptCohrc_check <- function(nghrs_stations,
                           xts_list_database,
                           umb_outlier = 85)
{
  
  xts_objs <- xts_list_database[nghrs_stations]
  
  ptmax <- do.call(cbind,
                   lapply(xts_objs, function(x){
                     
                     res_xts = zoo::coredata(x[, "tmax"]) %>% as.vector()
                     res_xts = ecdf(res_xts)(res_xts)
                     xts::xts(res_xts*100, zoo::index(x))
                     
                   }))
  
  ptmin <- do.call(cbind,
                   lapply(xts_objs, function(x){
                    
                    res_xts = zoo::coredata(x[, "tmin"]) %>% as.vector()
                    res_xts = ecdf(res_xts)(res_xts)
                    xts::xts(res_xts*100, zoo::index(x))
                    
                    }))
  
  # ptmax <- setNames(ptmax, nghrs_stations)
  # ptmin <- setNames(ptmin, nghrs_stations)
  
  non_qc_tmax <- abs(ptmax[, nghrs_stations[1]] - apply(ptmax[, nghrs_stations[-1]], 
                                                        1, mean, na.rm = TRUE)
                     )
  non_qc_tmax <- zoo::index(non_qc_tmax[non_qc_tmax > umb_outlier]
                            )
  
  
  non_qc_tmin <- abs(ptmin[, nghrs_stations[1]] - apply(ptmin[, nghrs_stations[-1]],
                                                    1, mean, na.rm = TRUE)
                     )
  non_qc_tmin <- zoo::index(non_qc_tmin[non_qc_tmin > umb_outlier]
                            )
  
  xts_obj = xts_objs[[ nghrs_stations[1] ]]
  xts_obj$tmax[non_qc_tmax] <- NA
  xts_obj$tmin[non_qc_tmin] <- NA
  
  non_qc <- rbind(data.frame(date = non_qc_tmin, 
                             var = rep("tmin", length( non_qc_tmin ))),
                  data.frame(date = non_qc_tmax, 
                             var = rep("tmax", length( non_qc_tmax  ))))
  
  return(list(qc = xts_obj,
              non_qc = non_qc))
}
