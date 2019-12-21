
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue, gtools, foreach, parallel, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
myExtract <- function(yr){
  print(yr)
  fl <- grep(yr, fls, value = TRUE)
  st <- stack(fl) %>% 
    raster::crop(., vll) %>% 
    raster::mask(., vll)
  Map('writeRaster', x = unstack(st), filename = glue('../data/raster/TERRACLIMATE/vll_lim/bios/{names(st)}.asc'), overwrite = FALSE)
  print('Done!')
}

# Load data ---------------------------------------------------------------
fls <- list.files('../data/raster/TERRACLIMATE/col_lim/bios', full.names = TRUE, pattern = '.asc$') %>% 
  mixedsort(.) %>% 
  grep(paste0(paste0('bio_', 1:19, '.asc'), collapse = '|'), ., value = TRUE)
vll <- shapefile('../data/shp/valle_limite.shp')
yrs <- str_sub(fls, start = 43, end = 46) %>% 
  unique()

# Process data to extract by mask -----------------------------------------
map(.x = yrs, .f = myExtract)




