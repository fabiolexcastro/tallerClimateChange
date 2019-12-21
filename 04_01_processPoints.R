
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, randomForest, tidyverse, rgeos, gtools, stringr, outliers, Hmisc, cclust, sf, randomForest, multcomp, dismo, magrittr, ggpubr, corrplot)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)
source('FunctionsRFclustering.R')

# Functions to use --------------------------------------------------------
dup_cell <- function(mask, df){
  cellNum <- raster::extract(mask, df[,c('Long', 'Lat')], cellnumbers = T) 
  cells <- xyFromCell(mask, cellNum[,'cells'])
  dupvec <- duplicated(cells[,c('x', 'y')])
  occ_rmDupCell <- tbl_df(df[!dupvec,])
  occ_DupCell <- tbl_df(df[dupvec,])
  return(list(occ_rmDupCell, occ_DupCell))
}
rmvOutliers <- function(pnts){
  # pnts <- pnt2
  norm <- scores(pnts[,3:ncol(pnts)], 'z')
  norm_na <- norm
  norm_na[abs(norm_na) > 3.5] <- NA
  normpoints <- cbind(pnts[,c('x', 'y')], norm_na) %>% 
    na.omit() %>% 
    as_data_frame()
  print('Done...!')
  normpoints <- normpoints[,c('x', 'y')]
  return(normpoints)
}
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  datRF_presences <- occ[,3:ncol(occ)]
  print(nrow(datRF))
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  return(list(labelRF, clusterdata))
}

# Load data ---------------------------------------------------------------
pnt <- read_csv('../data/tbl/points/arroz.csv')
fls <- list.files('../data/raster/WORLDCLIM_COLOMBIA/COLOMBIA', full.names = TRUE, pattern = '.asc$') %>% mixedsort()
stk <- stack(fls)
shp <- shapefile('../data/shp/arroz_zone.shp')

# Extract by mask
stk <- raster::crop(stk, shp) %>% raster::mask(., shp)
msk <- stk[[1]] * 0 + 1
Map('writeRaster', x = unstack(stk), filename = paste0('../data/raster/WORLDCLIM_COLOMBIA/STUDY_ZONE/', names(stk), '.asc'), overwrite = FALSE)

# Removing presences duplicated by cell -----------------------------------
pnt <- dup_cell(mask = msk, df = pnt)[[1]]
swd <- raster::extract(stk, pnt[,c('Long', 'Lat')]) %>%
  as.tibble() %>%
  mutate(x = pnt %>% pull(Long),
         y = pnt %>% pull(Lat)) %>%
  dplyr::select(x, y, bio_1:bio_19)

# Remove outliers ---------------------------------------------------------
swd <- rmvOutliers(pnts = swd)
swd <- raster::extract(stk, swd) %>% 
  as_data_frame() %>%
  mutate(x = swd %>% pull(1),
         y = swd %>% pull(2)) %>%
  dplyr::select(x, y, bio_1:bio_19)

# Clustering random forest ------------------------------------------------
occ <- swd
env_values <- as.matrix(occ[,3:ncol(occ)]); nrow(env_values)
datRF <- as.data.frame(occ[,3:ncol(occ)]); nrow(datRF)
d <- dist(datRF, method = "euclidean")  
rfClust <- rf.clust(occ = occ, nforest = 25, ntrees = 100, nVars = 8, nclasses = 3)
labelRF <- rfClust[[1]]
clusterdata <- rfClust[[2]]
classdata <- cbind(pb = as.factor(labelRF), occ[,3:ncol(occ)])
clusteredpresdata <- cbind(occ, cluster = labelRF) %>% na.omit() %>% tbl_df()
no.clusters <- 3

run <- 'run_1'
save(datRF, file = paste0('../rData/', run, '/datRF.rData'))
save(clusterdata, file = paste0('../rData/', run, '/clusterdata.rData'))
save(occ, clusteredpresdata, no.clusters, labelRF, file = paste0('../rData/', run, '/clustereddata.rData'))

