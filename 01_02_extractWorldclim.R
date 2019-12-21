
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
wcl <- 'E:/data/WORLDCLIM/Global_30s/'
wcl <- paste0(wcl, c('bio_'), 1:19)
wcl <- stack(wcl)
vll <- shapefile('../data/shp/valle_limite.shp')

# Extract by mask ---------------------------------------------------------
wcl <- raster::crop(wcl, vll) %>% 
  raster::mask(., vll)
dir.create('../data/raster/WORLDCLIM')
Map('writeRaster', x = unstack(wcl), filename = glue('../data/raster/WORLDCLIM/{names(wcl)}.asc'), overwrite = FALSE)
