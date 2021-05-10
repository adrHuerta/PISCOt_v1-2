# build_neigh_matrix - It builds time series based on neighboring stations (matrix) 
# nested function - main routine
# id_stations : IDs of stations (first one is the target)
# time_series_database : Time series database

# Note:
# An potential neighbour station is used if achieve two conditions:
# i) There is a shared period with five years of data (365*5) in both time series
# ii) Correlation is at least >= 0.6 (target vs neighbour)
# iii) otherwise, is not added

build_neigh_matrix <- function(id_stations,
                               time_series_database,
                               param_neigh = list(r_cor = NA, 
                                                  n_daily_cycle = NA))
{
  
  matrix_data <- time_series_database[, id_stations[1]]
  
  for(n_station in id_stations[2:length(id_stations)])
  {
    
    matrix_data <- cbind(matrix_data,
                         time_series_database[, n_station])
    
    # the time series (target and neighbor) at least match in 5 times each day except 02-29?
    at_leat_any_data <- apply(matrix_data[, c(id_stations[1], n_station)], 1, function(x) ifelse(any(is.na(x)), 0, 1))
    at_leat_any_data <- data.frame(count = at_leat_any_data, date = format(time(matrix_data),"%m-%d"))
    at_leat_any_data <- at_leat_any_data[at_leat_any_data$count != 0,]
    at_leat_any_data <- table(at_leat_any_data)
    at_leat_any_data <- at_leat_any_data[, -match("02-29", colnames(at_leat_any_data))]
   
    
    if(all(at_leat_any_data < param_neigh$n_daily_cycle)){
      
      matrix_data <- matrix_data[, -match(n_station, colnames(matrix_data))]
      
    } else {
      
      # how is the correlation?
      rcor_neigh = round(cor(matrix_data[, c(id_stations[1], n_station)], use = "pairwise.complete.obs")[2], 1)
    
      if(rcor_neigh < param_neigh$r_cor){
        
        matrix_data <- matrix_data[, -match(n_station, colnames(matrix_data))]
        
      }
    }
    
    
  }

  matrix_data
  
}




# -------------------------------------------------------------------------------
# build_matrix - It builds time series based on neighboring stations (matrix) 
# id_stations : IDs of stations (first one is the target)
# time_series_database : Time series database

# Note: two conditions based of id_stations
# i) If id_stations has one single time serie (one value)
# the output is same time serie 
# ii) otherwise, build_neigh_matrix is applied

build_matrix <- function(id_stations,
                         time_series_database,
                         param_neigh = list(r_cor = 0.6, n_daily_cycle = 5))
{
  
  if(length(id_stations) < 2){
    
    time_series_database[, id_stations]
    
  } else {
    
    build_neigh_matrix(id_stations,
                       time_series_database,
                       param_neigh = list(r_cor = param_neigh$r_cor, 
                                          n_daily_cycle = param_neigh$n_daily_cycle))
    
  }
  
  
}