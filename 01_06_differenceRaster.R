
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue, gtools, foreach, parallel, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
myMean <- function(rc){
  # rc <- rcp[1]
  yrs <- list.files(rc)
  gcm1 <- glue('{rc}/{yrs[1]}') %>% list.files(., full.names = TRUE)
  gcm2 <- glue('{rc}/{yrs[2]}') %>% list.files(., full.names = TRUE)
  vrs <- paste0('bio_', 1:19, '.asc')
  
  for(i in 1:length(vrs)){
    rs <- glue('{gcm1}/{vrs[i]}') %>% 
      stack() %>% 
      mean()
    dir <- glue('../data/raster/DIFFERENCE_MEAN/{basename(rc)}/2020_2049')
    dir.create(dir, recursive = TRUE)
    writeRaster(rs, filename = glue('{dir}/mean_{vrs[i]}'), overwrite = TRUE)
    print('Done!')
  }
  for(i in 1:length(vrs)){
    rs <- glue('{gcm2}/{vrs[i]}') %>% 
      stack() %>% 
      mean()
    dir <- glue('../data/raster/DIFFERENCE_MEAN/{basename(rc)}/2040_2069')
    dir.create(dir, recursive = TRUE)
    writeRaster(rs, filename = glue('{dir}/mean_{vrs[i]}'), overwrite = TRUE)
    print('Done!')
  }
}

# Load data ---------------------------------------------------------------
rcp <- list.files('../data/raster/DIFFERENCE/', full.names = TRUE)

# Apply the function ------------------------------------------------------
for(i in 1:length(rcp)){
  myMean(rc = rcp[i])
}

