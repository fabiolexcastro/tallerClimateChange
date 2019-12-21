
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, tidyverse, parallel, foreach, doSNOW, rgdal, cclust, outliers, dismo, gtools, multcomp, sp, rgeos, outliers, FactoMineR, pROC, randomForest, stringr)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 9999)
run <- 'run_1'
myproj <- CRS('+proj=longlat +datum=WGS84')

# Load data ---------------------------------------------------------------
load('../rData/run_1/clusterdata.rData')
load('../rData/run_1/clustereddata.rData')
load('../rData/run_1/rflist_3.rdata')

NumberOfClusters <- 3
ar5biofolder <- '../data/raster/IPCC/data/col/RCP85'
yearlist <- '2040_2069'
gcmlist <- list.files(paste0(ar5biofolder, '/', yearlist, '/_grid'))
resultsfolder <- paste0('../rf/output/run1/results/raw/') 
modelfolder <- '../rf/output/run1/models/'
shp <- shapefile('../data/shp/arroz_zone.shp')

rff <- do.call(randomForest::combine, rflist)

vrs <- paste0('bio_', 1:19, '$')
y <- 1
nCores <- detectCores()
cl <- makeCluster(2)
registerDoSNOW(cl)

foreach(i = 15:length(gcmlist), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach', 'randomForest', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  
  print(gcmlist[i]) 
  
  gcmfiles <- paste(ar5biofolder, yearlist[y], '_grid', gcmlist[i], sep = '/') %>%
    list.files(., full.names = T) %>% 
    grep(paste0(vrs, collapse = '|'), ., value = T) %>%  
    mixedsort()
  
  climatelayers <- raster::stack(gcmfiles)
  climatelayers <- raster::crop(climatelayers, shp) %>% 
    raster::mask(., shp)
  climatevalues <- data.frame(getValues(climatelayers))
  
  print('Climate values')
  
  rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
  rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
  uncertainty <- apply(rasterProbs, 1, max)  
  
  rasterRFprob <- climatelayers[[1]] * 0 + 1
  values(rasterRFprob) <- rasterRF 
  
  rasterRFuncertainty <- climatelayers[[1]] * 0 + 1
  values(rasterRFuncertainty) <- uncertainty 
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- climatelayers[[1]] * 0 + 1
  values(rasterRFclass) <- rasterRF
  
  print("Write Raster...")
  dir.create(paste0('../rf/output/run1/results/raw/', yearlist[y]))
  writeRaster(rasterRFclass, paste0('../rf/output/run1/results/raw/',  yearlist[y], '/RF_', NumberOfClusters, 'Clust_', gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  writeRaster(rasterRFprob, paste0('../rf/output/run1/results/raw/', yearlist[y], '/RF_', NumberOfClusters, 'Prob_',  gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  writeRaster(rasterRFuncertainty, paste('../rf/output/run1/results/raw/', yearlist[y], '/RF_', NumberOfClusters, 'Unc_', gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = T)
  print('Done!')
  print(gcmlist[i])
}
stopCluster(cl)

mdl <- list.files('../rf/output/run1/results/raw/2040_2069', full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  grep('Clust', ., value = T) %>% 
  stack()
mdl <- raster::modal(mdl)  
writeRaster(mdl, '../_rf/_output/_run1/_results/_raw/RF_5Clust_future.asc')


prb <- list.files('../_rf/_output/_run1/_results/_raw/_2050/', full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort() %>% 
  grep('Prob_', ., value = T) %>% 
  stack()
prb <- mean(prb)
writeRaster(mdl, '../_rf/_output/_run1/_results/_raw/RF_5Prob_future.asc', overwrite = TRUE)


