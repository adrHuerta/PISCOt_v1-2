# Only performed to temperature data
# data_list: a list where each element is a xts-data.frame (tmax and tmin)
# list_of_qc_expr: a list of expressions that should be applied to data_list (visual qc)
# output: a list with the qc (applied visual qc) and non_qc (where has been applied the data) data

apply_visual_qc <- function(data_list,
                            list_of_qc_expr)
{

  env <- new.env()
  env$response <- data_list
  name_of_list <- deparse(substitute(response))

  for(i in names(list_of_qc_expr)){
    name_of_station <- i
    tmax <- paste(c(name_of_list, name_of_station, "tmax"), collapse = "$")
    tmin <- paste(c(name_of_list, name_of_station, "tmin"), collapse = "$")
    
    to_parse <- gsub("tmax", tmax, gsub("tmin", tmin, list_of_qc_expr[[i]]))
    eval(parse(text = to_parse), envir = env)
  }
  
  mapply(function(x, y){
    if(identical(x, y) == TRUE){
      NULL
    } else {
      
      mapply(function(xx, yy){
        identical(xx, yy)
      }, x = x[,"tmax"], y = y[,"tmax"]) -> to_tmax
      
      mapply(function(xx, yy){
        identical(xx, yy)
      }, x = x[,"tmin"], y = y[,"tmin"]) -> to_tmin

      
      del_tmax <- x[,"tmax"][!to_tmax]
      del_tmin <- x[,"tmin"][!to_tmin]
      
      rbind(data.frame(date = zoo::index(del_tmax),
                       var = rep("tmax", length(del_tmax))),
            data.frame(date = zoo::index(del_tmin),
                       var = rep("tmin", length(del_tmin))))
      
      
      
    }
  }, x = data_list, y = env$response,
  SIMPLIFY = FALSE) -> non_qc
  
  list(qc = env$response,
       non_qc = non_qc)
  
}

# QC_list <- list(A123 = data.frame(tmax = c(0:10),
#                                   tmin = c(-10:0)), 
#                 B123 = 1:length(LETTERS), 
#                 D545 = data.frame(tmax = c(0:10),
#                                   tmin = c(-10:0)))
# 
# visual_QC <- source('~/Documents/Repos/PISCOt/example.R')$value
# 
# name_of_list <- deparse(substitute(QC_list))
# 
# for(i in names(visual_QC)){
#   name_of_station <- i
#   tmax <- paste(c(name_of_list, name_of_station, "tmax"), collapse = "$")
#   tmin <- paste(c(name_of_list, name_of_station, "tmin"), collapse = "$")
#   
#   to_parse <- gsub("tmax", tmax, gsub("tmin", tmin, visual_QC[[i]]))
#   eval(parse(text = to_parse))
# }
