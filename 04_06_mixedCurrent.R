

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use
mixedCategory <- function(pth.cls, pth.unc, pth.prb, threshold, no.absenceclasses){
  lyrClust <- raster(pth.cls)
  lyrUnc <- raster(pth.unc)
  lyrPrb <- raster(pth.prb)
  
  thrUnc <- raster::extract(lyrUnc, occ[,1:2])
  thrUnc <- thrUnc[!is.na(thrUnc)]
  thrUnc1  <- quantile(thrUnc, 0.1) %>% as.numeric()
  quantile(thrUnc, seq(0, 1, 0.01))
  min(thrUnc)
  
  save(thrUnc1, file = '../rData/run_1/threshold_unc.rData')
  
  rslt <- lyrClust
  rslt[which(lyrUnc[] < thrUnc1 & lyrPrb[] > threshold)] <- max(unique(lyrClust[]), na.rm = T) + 1 
  print('To write the raster')
  writeRaster(rslt, paste0('../rf/output/run1/results/process/RF_5Classes_unc_', gcm, '.asc'), overwrite = TRUE)
  print('¡Done!')
}

# Load data
run <- 'run_1'
load('../rData/run_1/threshold_prob.rData')
load('../rData/run_1/clustereddata.rData')
gcm <- 'current'

pth.cls <- '../rf/output/run1/results/process/RF_3_Clust_lim_current.asc'
pth.unc <- '../rf/output/run1/results/raw/RF_5Unc_current.asc'
pth.prb <- '../rf/output/run1/results/raw/RF_5Prob_current.asc'
pth.out <- '../rf/output/run1/results/process/'

no.absenceclasses <- 2
no.clusters <- 3

occ <- occ[,1:2]

mixedCategory(pth.cls, pth.unc, pth.prb, threshold, no.absenceclasses)
