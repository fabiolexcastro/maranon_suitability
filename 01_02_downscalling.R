
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, magrittr, RSAGA, tmap)
rm(list = ls())

# Load data ---------------------------------------------------------------
pth <- '../rst/climate/chelsa/crn'
fls <- list.files(pth, full.names = T, pattern = '.tif') 
zne <- shapefile('../shp/bse/zone_geo.shp')
zne@data$gid <- 1
zne <- aggregate(zne, 'gid')
dem <- raster('../rst/dem/zne/srtm90m.tif')
env <- rsaga.env(path = '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_maranon/saga/saga-7.0.0_x64/saga-7.0.0_x64')



downscaling <- function(fle){
  # fle <- fls[1]
  
  rsl <- rsaga.geoprocessor(lib = 'statistics_regression',
                            module = 'GWR for Grid Downscaling',
                            param = list(PREDICTORS = '../rst/dem/zne/srtm90m.tif',
                                         REGRESSION = paste0('../rst/climate/chelsa/crn/1ha/', basename(fle)),
                                         DEPENDENT  = fle),
                            env = env)

}

map(.x = fls, .f = downscaling)


