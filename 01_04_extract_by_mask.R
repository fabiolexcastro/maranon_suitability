
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, dismo, rgeos, gtools, stringr, tidyverse, magrittr, RSAGA, tmap)
rm(list = ls())

# Functions to use --------------------------------------------------------
read <- function(x){
  x <- grep(x, fls, value = T) %>% 
    mixedsort() %>% 
    stack()
}

# Load data ---------------------------------------------------------------
msk <- shapefile('../shp/bse/zone_geo_fnl_2.shp')
fls <- list.files('../rst/climate/chelsa/crn/1ha', full.names = T, pattern = '.sdat$')
stk <- stack(fls)
cut <- raster::crop(stk, msk)
cut <- raster::mask(cut, msk)
Map('writeRaster', x = unstack(cut), filename = paste0('../rst/climate/chelsa/crn/1ha/zne/', names(cut), '.tif'))

# Make the bioclim variables ----------------------------------------------
fls <- list.files('../rst/climate/chelsa/crn/1ha/zne', full.names = T, pattern = '.tif$')
ppt <- read('prec')
tmx <- read('tmax')
tav <- read('temp')
tmn <- read('tmin')

bcl <- biovars(prec = ppt, tmin = tmn, tmax = tmx)
Map('writeRaster', x = unstack(bcl), filename = paste0('../rst/climate/chelsa/crn/1ha/bios/bio_', 1:19, '.tif'))


