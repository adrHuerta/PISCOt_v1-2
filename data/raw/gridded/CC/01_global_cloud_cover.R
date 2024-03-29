#' Download Global 1-km Cloud Cover
#' https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1002415
#' https://www.earthenv.org//cloud
#' @noRd

library(raster)
"%>%" = magrittr::`%>%`

PISCO_BBOX <- c(-81.4 - 0.03, 
                -67.05 + 0.03, 
                -18.75 - 0.03, 
                1.13 + 0.03)

CLOUD_FOLDER <- "./data/raw/gridded/CC/CC_monthly/"

# Link of the files
files_to_download <- sprintf("https://data.earthenv.org/cloud/MODCF_monthlymean_%02d.tif", 1:12)

# Folder to save data
file_save <- sprintf("%s/cloud_coverage_%02d.tif",CLOUD_FOLDER, 1:12)
lapply(1:12, function(x) download.file(files_to_download[x], file_save[x]))

# list cloud coverage
cloudcoverage_files <- list.files(CLOUD_FOLDER, full.names = TRUE) %>% stack()
# pisco_region <- ee$Geometry$Rectangle(coords = CLOUD_FOLDER) %>%
#  ee_as_sf() %>%
#  as("Spatial")
pisco_region <- extent(PISCO_BBOX)

# We divide by 100 for obtain real values.
cloudcoverage_files_peru <- crop(cloudcoverage_files, pisco_region)/100


file_save <- sprintf(
  "%s/cloud_coverage_%02d.tif", paste0(dirname(CLOUD_FOLDER), "/CC_monthly/Peru"), 
  1:12
)
lapply(1:12, function(x) writeRaster(cloudcoverage_files_peru[[x]], file_save[x]))
