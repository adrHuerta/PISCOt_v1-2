Zobs_2_Zdem <- function(rxyz,
                        z_dem,
                        Diff = 200)
{
  Zdem <- raster::extract(z_dem, rxyz[, c("LON", "LAT")])
  Zobs <- rxyz[, c("ALT")]
  Zcorrected <- ifelse( abs(Zdem - Zobs) > Diff, Zdem, Zobs)
  
  rxyz_corrected <- rxyz
  rxyz_corrected[, c("ALT")] <- Zcorrected
  
  rxyz_errors <- rxyz[which((Zobs-Zcorrected) != 0), ]
  rxyz_errors[, c("ALT_dem")]<- Zdem[which((Zobs-Zcorrected) != 0)]
  
  return(list(qc = rxyz_corrected,
              non_qc = rxyz_errors))
}