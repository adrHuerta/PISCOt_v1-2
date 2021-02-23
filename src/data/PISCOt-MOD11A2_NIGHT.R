temperature_palette <- c(
  '040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
  '0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
  '3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
  'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
  'ff0000', 'de0101', 'c21301', 'a71001', '911003'
)

count_pixels <- function(img) {
  img$select("LST_Night_1km")$multiply(0)$add(1)$unmask(0)
}

mod11A2_clean <- function(img) {
  # from kelvin to celsius LST Day
  lst_value_celsius <- img$
    select("LST_Night_1km")$
    multiply(0.02)$
    subtract(273.15)
  # quality band
  lst_qa <- img$select("QC_Night")
  # qa mask
  #quality_flag <- getQABits(lst_qa, 2, 3, "quality")$lte(0)
  emissivityerror_mask <- getQABits(lst_qa, 4, 5, "emissivity_error")$lte(1)
  LSTerror_mask <- getQABits(lst_qa, 6, 7, "lst_error")$lte(1)
  
  # Both flags should be set to zero, indicating clear conditions.
  mask <- emissivityerror_mask$Or(LSTerror_mask)#$Or(quality_flag)
  lst_value_celsius$updateMask(mask)
}

getQABits = function(image, start, end, newName) {
  # Compute the bits we need to extract.
  pattern = 0
  for (index in start:end) {
    pattern = pattern + 2**index
  }
  # Return a single band image of the extracted QA bits, giving the band
  # a new name.
  image$
    bitwiseAnd(pattern)$
    rightShift(start)
}