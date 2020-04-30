
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, magrittr, RSAGA, tmap)
rm(list = ls())

# Functions to use --------------------------------------------------------
extract_zones <- function(var = 'prec'){
  fle <- grep(var, fls, value = T)
  plg <- lapply(1:length(fle), function(i){ 
    print(i)
    rst <- raster(fle[i]) 
    rst <- rst * 0
    pol <- rasterToPolygons(rst)
    shapefile(pol, paste0('../workspace/prec_hold/shp/prec_hold_', i, '.shp'))
    print(paste0('Done ', i))
  })
}

# Load data ---------------------------------------------------------------
fls <- list.files('../rst/climate/chelsa/crn/1ha', full.names = T, pattern = '.sdat$')
vrs <- c('prec', 'tmax', 'temp', 'tmin')

# Extracting the zones
extract_zones(var = vrs[1])


extract_mask <- function(var){
  # var <- vrs[1]
  
  fle <- grep(var, fls, value = T)
  stk <- stack(fle)
  vls <- rasterToPoints(stk) %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', 'value'))
  
  
}

