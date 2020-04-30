
# Load libraries
library(raster)
library(rgdal)
library(dplyr)
library(dismo)
library(readr)
library(rgeos)
library(gtools)
library(rJava)
library(ecospat)

# Initial setup
options(java.parameters = '-Xmx4g')

# Load data
occ <- read_csv('../tbl/mdl/run5/occ_swd.csv')
bck <- read_csv('../tbl/mdl/run5/back_swd.csv')
vrs <- readRDS('../rds/run5/vars.rds')

fls <- list.files('../rst/climate/chelsa/crn/1ha/bios', full.names = T, pattern = '.tif$') %>% 
  grep(paste0(vrs, '.tif$', collapse = '|'), ., value = T) %>% 
  mixedsort()
lyr <- stack(fls)

# Run Maxent - Way 1 (Normal)
me <- maxent(lyr, as.data.frame(occ[,1:2]), remove.Duplicates = TRUE, path = '../mxn/run1')
rs <- predict(me, lyr, progress = 'text')

# Run Maxent - Way 2 (SWD)
pres.covs <- raster::extract(lyr, occ[,1:2], cellnumbers = T) %>%
  na.omit() %>%
  unique() %>%
  .[,-1] %>%
  cbind(occ[,1:2], .)

bck.covs <- raster::extract(lyr, bck[,1:2]) %>%
  cbind(bck[,c(1,2)], .)

env.values <- data.frame(rbind(pres.covs, bck.covs))
y   <- c(rep(1, nrow(pres.covs)), rep(0, nrow(bck.covs)))
me  <- maxent(env.values[,3:6], y, args = c('addsamplestobackground=true'), path = './mxn/run4')
map <- predict(me, lyr, progress = 'text')

writeRaster(map, './mxn/run2/map.asc')
save(me, file = './mxn/run2/mx_obj.RData')

# Evaluations using K-fold partioning
fold <- kfold(pres.covs, k = 7)
occtest <- pres.covs[fold == 1,]
occtrain <- pres.covs[fold != 1,]
y <- c(rep(1, nrow(occtrain)), rep(0, nrow(bck.covs)))
env.values <- data.frame(rbind(occtrain, bck.covs))
dir.create('./mxn/run5')

me <- maxent(env.values[,3:ncol(env.values)], y, args = c('addsamplestobackground=true'), path = './_maxent/_robusta/_models/_run4')
e  <- evaluate(me, p = data.frame(occtest[,3:ncol(occtest)]), a = data.frame(bck.covs[,3:ncol(bck.covs)]))

# Threshold value that maximizes Kappa
plot(e@t, e@kappa, type = 'l')
e@t[which.max(e@kappa)]

# Computing True Skill Statistic = TPR(Sensitivity)+TNR(Specificity)-1
tss <- e@TPR + e@TNR - 1
plot(e@t, tss, type = 'l')
e@t[which.max(tss)]

#AUC Plot: X=1-Specificity, Y=Sensitivity
plot((1 - e@TNR), e@TPR, type = 'l', xlab = 'Fractional Predicted Area (1 - Specificity)', ylab = 'Sensitiviy')
e@auc

# Now, for all folds
auc <- rep(NA, 7)
max.tss <- rep(NA,7)
maps <- list()
e <- list()
me <- list()
dir_out <- paste0('../mxn/run4/mod_', 1:7)

dir.create(dir_out)

for (i in 1:7){
  
  occtest <- pres.covs[fold == i, ]
  occtrain <- pres.covs[fold != i, ]
  env.values <- data.frame(rbind(occtrain, bck.covs))
  y  <- c(rep(1, nrow(occtrain)), rep(0, nrow(bck.covs)))
  me[[i]] <- maxent(env.values[,3:ncol(env.values)], y, args = c('addsamplestobackground=true'), path = dir_out[[i]])
  maps[[i]] <- predict(me[[i]], lyr)
  e[[i]] <- evaluate(me[[i]], p = data.frame(occtest[,3:ncol(occtest)]), a = data.frame(bck.covs[,3:ncol(bck.covs)]))
  auc[i] <- e[[i]]@auc
  lines((1 - e[[i]]@TNR), e[[i]]@TPR)
  tss <- e[[i]]@TPR + e[[i]]@TNR-1
  max.tss[i] <- e[[i]]@t[which.max(tss)]
  
}

map_avg <- mean(stack(maps))

Map('writeRaster', x = maps, filename = paste0('../mxn/run4/map_run', 1:7, '.tif'))
writeRaster(map_avg, '../mxn/run4/map_avg.tif')

th_tss <- mean(max.tss)
dir.create('../rst/model/run4', recursive = TRUE)
saveRDS(th_tss, '../rst/model/run4/th_tss.rds')


