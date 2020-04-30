

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, magrittr)
rm(list = ls())

# Function ----------------------------------------------------------------
extract_mask <- function(fle){
  rst <- raster(fle)
  rst <- crop(rst, shp_lim)
  rst <- mask(rst, shp_lim)
  writeRaster(rst, paste0('../rst/climate/chelsa/crn/', basename(fle)), overwrite = T)
  print('Done')
}

# Load data ---------------------------------------------------------------
pth_chl <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/CHELSA/tif/crn'
fls_chl <- list.files(pth_chl, '.tif$', full.names = T)
shp_lim <- shapefile('../shp/bse/zone_geo.shp')

# Apply the function ------------------------------------------------------
map(.x = fls_chl, .f = extract_mask)

