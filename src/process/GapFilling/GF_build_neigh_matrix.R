build_neigh_matrix <- function(id_stations,
                               time_series_database)
{
  
  matrix_data <- time_series_database[, id_stations[1]]
  
  for(n_station in id_stations[2:length(id_stations)])
  {
    
    matrix_data <- cbind(matrix_data,
                         time_series_database[, n_station])
    
    # the time series (target and neigh) at least match in 5 times each day except 02-29?
    at_leat_any_data <- apply(matrix_data[, c(id_stations[1], n_station)], 1, function(x) ifelse(any(is.na(x)), 0, 1))
    at_leat_any_data <- data.frame(count = at_leat_any_data, date = format(time(matrix_data),"%m-%d"))
    at_leat_any_data <- at_leat_any_data[at_leat_any_data$count != 0,]
    at_leat_any_data <- table(at_leat_any_data)
    at_leat_any_data <- at_leat_any_data[, -match("02-29", colnames(at_leat_any_data))]
   
    
    if(all(at_leat_any_data < 5)){
      
      matrix_data <- matrix_data[, -match(n_station, colnames(matrix_data))]
      
    } else {
      
      rcor_neigh = round(cor(matrix_data[, c(id_stations[1], n_station)], use = "pairwise.complete.obs")[2], 1)
    
      if(rcor_neigh < .6){
        
        matrix_data <- matrix_data[, -match(n_station, colnames(matrix_data))]
        
      }
    }
    
    
  }

  matrix_data
  
}

build_matrix <- function(id_stations,
                         time_series_database)
{
  
  if(length(id_stations) < 2){
    
    time_series_database[, id_stations]
    
  } else {
    
    build_neigh_matrix(id_stations,
                       time_series_database)
    
  }
  
  
}