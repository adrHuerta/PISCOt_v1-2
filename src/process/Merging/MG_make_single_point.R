make_single_point <- function(pts, rgrid, nameID = "ID"){
  
  counts <- raster::cellFromXY(rgrid, pts)
  pts2 <- pts
  pts2@data[, "pixel_value"] <- counts
  
  list_of_points <- by(pts2@data, pts2@data[, "pixel_value"], function(x) x[, nameID])
  list_of_points <- list_of_points[lapply(list_of_points, length) != 1]
  
  if(length(list_of_points) == 0) stop("all point-stations are already in a single pixel")
  
  do.call("rbind", 
          lapply(list_of_points, function(x){
            
            to_save <- pts@data[1, ]
            to_merge <- pts@data[match(x, pts@data[, nameID]),]
            
            for(i in 1:ncol(to_merge)){
              
              res <- unlist(to_merge[, i])
              res <- ifelse(is.numeric(res), median(res), paste(res, collapse = '_'))
              to_save[i] <- res
              
            }
            
            return(to_save)
            
          })) -> single_points
  
  clean_points <- pts[-match(unlist(list_of_points), pts@data[, nameID]),]@data
  clean_points <- rbind(clean_points, single_points)
  rownames(clean_points) <- NULL
  
  clean_points <- sp::SpatialPointsDataFrame(coords = clean_points[, c("LON", "LAT")],
                                             data = clean_points,
                                             proj4string = sp::CRS(sp::proj4string(pts)))
  
  return(list(xyz = clean_points, no_single_points = list_of_points))
  
}