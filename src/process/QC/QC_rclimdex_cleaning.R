## Changing NaN to NA
# Note: 
# https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame/18142212
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))


#---------------------------------------------------
# rclimdex_2_xts - rclimdex format file to xts object
# data_frame: a data.frame object from rclimdex format file
# vars: variables to be passed 
# non_values: non numeric values to be converted as NA

# Note: 
# convert a data.frame to xts class by performing a very basic QC:
# i) deleting missing values numbers
# ii) deleting bad/duplicated dates
# "good" and "bad" data is returned as a list()


rclimdex_2_xts <- function(data_frame,
                           vars = c("tmax", "tmin"),
                           non_values = c(-999, -888, -99.9, -88.8))
  {
  
  # converting missing value format to NA 
  for(non_value in non_values) data_frame[data_frame == non_value] = NA 
  
  # getting dates
  data_frame_dates =  as.Date(paste(data_frame[, 1],
                                    data_frame[, 2],
                                    data_frame[, 3], sep = "-"))
  
  data_frame = data.frame(dates = data_frame_dates,
                          data_frame[, -c(1, 2, 3)])
  
  # checking NA empty dates
  if( any(is.na(data_frame$dates)) ){
    
    non_QC1 <- data_frame[is.na(data_frame$dates),]
    data_frame <- data_frame[!is.na(data_frame$dates),]
    
  } else {
    
    non_QC1 <- data.frame(dates = rep(NA, 0),
                          V4 = rep(NA, 0),
                          V5 = rep(NA, 0),
                          V6 = rep(NA, 0))
    
  }
  
  # checking duplicated dates
  if( length(data_frame$dates) > length(unique(data_frame$dates)) ) {
    
    ids = which(data_frame$dates %in% data_frame$dates[duplicated(data_frame$dates)])
    non_QC2 = data_frame[ids, ]
    
    # aggregating duplicated values as mean
    data_frame_duplicated_average = aggregate(non_QC2[, -1],
                                              list(dates = non_QC2[,1]),
                                              FUN = mean, na.rm = T)
    data_frame_duplicated_average[is.nan(data_frame_duplicated_average)] <- NA
    
    # rbind unique and mean duplicated values
    data_frame = rbind(data_frame[-ids, ],
                       data_frame_duplicated_average)
    
  } else {
    
    non_QC2 <- data.frame(dates = rep(NA, 0),
                          V4 = rep(NA, 0),
                          V5 = rep(NA, 0),
                          V6 = rep(NA, 0))
    
  }
  
  # to xts object
  qc <- xts::xts(data_frame[, -1], data_frame[, 1]) # automatic character/logical to numeric
  qc <- round(qc, 1)
  non_qc <- rbind(non_QC1, non_QC2)
  
  colnames(qc) = c("pp", "tmax", "tmin")
  colnames(non_qc)[2:4] = c("pp", "tmax", "tmin")
  
  
  return(list(qc = qc[, vars], 
              non_qc = non_qc))
  
}