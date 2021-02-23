# 5. Spatial coherence check
# id_station : ID of station data
# stations_database: a dataframe with LON and LAT information (it also needs to have a ID columns)
# limits_km : define the distance (in km) in which a station is declared as a neighboring station (searh in three levels)
# limints_n : at least how many stations is need to have a station to be used for spatial analysis, if is less that the designed value, neighboring station are provided 

# - Only done where area highly amount of stations (jungle may is not posible)
# - Estacion candidata vs estaciones vecinas (al menos 4 estaciones)

spt_neighrs <- function(id_station,
                        stations_database,
                        lmt_dist = 70, 
                        lmt_elv = 500,
                        lmt_n = 4)
{
  # target station
  pos_id = match(id_station, stations_database[,1])
  id_alt_lon_lat <- stations_database[pos_id, c("ID", "LON", "LAT", "ALT")]
  neigh_id_alt_lon_lat <- stations_database[-pos_id, ]
  
  # target station vs all stations
  dist_df <- data.frame(ID = neigh_id_alt_lon_lat[, c("ID")],
                        ELV = round(abs(id_alt_lon_lat[, c("ALT")] -  neigh_id_alt_lon_lat[, c("ALT")]), 3),
                        DIST = round(geosphere::distVincentyEllipsoid(id_alt_lon_lat[, c("LON", "LAT")],
                                                                      neigh_id_alt_lon_lat[, c("LON", "LAT")]) / 1000, 3),
                        neigh_id_alt_lon_lat[, c("LON", "LAT", "ALT")])
  
  # applying filter
  dist_df <- dist_df[(dist_df["ELV"] <= lmt_elv) & (dist_df["DIST"] <= lmt_dist), -c(2, 3)]
  dist_df <- rbind(id_alt_lon_lat, dist_df) # adding target station as first
  
  # creating 3D matrix distance
  matrix_df <- as.matrix(dist(dist_df[, c("LON", "LAT", "ALT")]))
  rownames(matrix_df) <- as.character(dist_df$ID)
  colnames(matrix_df) <- as.character(dist_df$ID)
  
  # getting neighbours based on target stations
  id_alt_lon_lat <- names(matrix_df[,1])[1]
  neigh_ids <- names(sort(matrix_df[,1][-1]))
  
  # if there is not any neighbours stations just get the target, otherwise the target + neighbours
  response <- c(id_alt_lon_lat, neigh_ids)
  if(is.null(response)) response <- as.character(dist_df$ID)
  
  # filter based on n neighbours
  if(length(response) <= (lmt_n+1)){
      
      response <- response
      
      } else {
        
        response <- response[1:(lmt_n+1)]
        
      }
  
  return(response)
  
}
