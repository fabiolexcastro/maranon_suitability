
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, dismo, rgeos, usdm, gtools, stringr, tidyverse, magrittr)
rm(list = ls())

# Load data ---------------------------------------------------------------

# Zone
zne <- shapefile('../shp/bse/zone_geo_fnl_3.shp')
zne@data$gid <- 1
zne <- aggregate(zne, 'gid')

# Points
pnt <- shapefile('../shp/pnt/points.shp')

# Finca Erika
pnt_erk <- c((72 + (52 / 60) + (32 / 3600) ) * -1, (4 + 49/60 + 46 /36000))
pnt <- rbind(coordinates(pnt), pnt_erk) %>% 
  as.data.frame %>% 
  as_tibble %>% 
  setNames(c('lon', 'lat')) %>% 
  mutate(id = 1:nrow(.)) %>%  
  dplyr::select(id, lon, lat)
rm(pnt_erk)

# Climate
bio <- list.files('../rst/climate/chelsa/crn/1ha/bios', full.names = T, pattern = '.tif$') %>% 
  mixedsort %>% 
  stack()
msk <- raster::getData('worldclim', var = 'prec', res = 0.5, lon = as.numeric(pnt[1,2]), lat = as.numeric(pnt[1,3]) )
zne <- spTransform(zne, CRSobj = crs(msk))
msk <- msk[[1]] %>% raster::crop(., zne) %>% raster::mask(., zne)
msk <- msk * 0 + 1

# Remove duplcicate by cell
num <- raster::extract(msk, pnt[,2:3], cellnumbers = T) 
cls <- xyFromCell(msk, num[,'cells'])
dup <- duplicated(cls[,c('x', 'y')])
pnt <- coordinates(pnt)[!dup,]
pnt <- pnt %>% 
  as_tibble %>% 
  mutate(id = 1:nrow(.)) %>% 
  setNames(c('id', 'x', 'y'))
swd <- raster::extract(bio, pnt[,2:3])
swd <- cbind(pnt, swd)
swd <- drop_na(swd)

# VIF Analysis ------------------------------------------------------------
vif.res <- vif(x = swd[,4:ncol(swd)])
vif.step <- vifstep(x = swd[,4:ncol(swd)], th = 10)
vrs <- vif.step@results$Variables %>% as.character()

swd <- dplyr::select(swd, x, y, id, vrs)
dir.create('../tbl/mdl/run5', recursive = T)
write.csv(swd, '../tbl/mdl/run5/swd_vrs.csv', row.names = F)

dir.create('../rds/run5', recursive = T)
saveRDS(vrs, '../rds/run5/vars.rds')

# Generate background -----------------------------------------------------
back_raster <- msk
speciescell <- raster::extract(msk, swd[,2:3], cellnumber = TRUE)
back_raster[speciescell[,1]]  <- NA #remove the cell with presences
back <- randomPoints(back_raster, 5*nrow(swd)) %>% as_tibble()
coordinates(back) <- ~ x + y
back_swd  <- raster::extract(bio, back) %>% cbind(coordinates(back), .) %>% as.data.frame %>% dplyr::select(x, y, vrs)

dir.create('../tbl/mdl/run5')
write.csv(back_swd, '../tbl/mdl/run5/back_swd.csv', row.names = FALSE)
write.csv(swd, '../tbl/mdl/run5/occ_swd.csv', row.names = FALSE)



