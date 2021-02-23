## Topografic disection index
# based on Holden et al., 2011

# raster_grid: a raster object (from raster::raster())
# moving_window: spatial windows (matriz) of N km.

TDI_fun <- function(raster_grid,
                    moving_window)
  {
  
  # needed to build a matriz of size N x N km
  moving_window = moving_window*2 + 1
  
  raster::focal(raster_grid,
                # same weight for each pixel inside the spatial window
                w = matrix(1, nrow = moving_window, ncol = moving_window), 
                fun = function(pixel)
                  {
                  # based on Holden et al., 2011
                  (pixel[moving_window*(moving_window/2 + 0.5) - (moving_window/2 - 0.5)] - min(pixel)) / 
                    (max(pixel) - min(pixel))
                
                  })
  
  }
