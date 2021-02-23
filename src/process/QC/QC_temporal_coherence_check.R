# 4. Temporal coherence check
# 
# xts_obs : a xts dataframe
# csc_umb : max number of repetead values, after that number, all values are deleted
# jump_umb: jump steps (day to day) of this value are deleted 
# https://www.senamhi.gob.pe/load/file/01402SENA-5.pdf
# https://www.senamhi.gob.pe/pdf/estudios/PublicacionesDMA/2012/Espinoza_etal_clidy_2013_AuthorCopy.pdf

temCoh_check <- function(xts_obj,
                         csc_umb = 8,
                         jump_umb = 20)
{
  
  tmax = xts_obj[, "tmax"]
  csc_seq_tmax = sequence(rle(as.vector(tmax))$lengths) 
  
  non_qc_tx_1 <- zoo::index( tmax[which(csc_seq_tmax > csc_umb)] )
  non_qc_tx_2 <- diff(tmax)
  non_qc_tx_2 <- zoo::index( non_qc_tx_2[(non_qc_tx_2 > jump_umb) | (non_qc_tx_2 < -jump_umb)] )

  tmin = xts_obj[, "tmin"]
  csc_seq_tmin = sequence(rle(as.vector(tmin))$lengths) 
  
  non_qc_tn_1 <- zoo::index( tmin[which(csc_seq_tmin > csc_umb)] )
  non_qc_tn_2 <- diff(tmin)
  non_qc_tn_2 <- zoo::index( non_qc_tn_2[(non_qc_tn_2 > jump_umb) | (non_qc_tn_2 < -jump_umb)] )
  
  
  non_qc <- rbind(data.frame(date = c(non_qc_tx_1, non_qc_tx_2), 
                             var = rep("tmax", length( c(non_qc_tx_1, non_qc_tx_2)  ))
                             ),
                  data.frame(date = c(non_qc_tn_1, non_qc_tn_2), 
                             var = rep("tmin", length( c(non_qc_tn_1, non_qc_tn_2) ))
                             )
                  )
  
  
  xts_obj[, "tmax"][c(non_qc_tx_1, non_qc_tx_2)] <- NA
  xts_obj[, "tmin"][c(non_qc_tn_1, non_qc_tn_2)] <- NA

  return(list(qc = xts_obj,
              non_qc = non_qc))
  
}