
# Load data ---------------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(lisst = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
myCrop <- function(yr){
  # yr <- yrs[1]
  st <- grep(yr, fls, value = TRUE) %>% 
    stack()
  ct <- raster::crop(st, vll) %>% 
    raster::mask(., vll)
  Map('writeRaster', x = unstack(ct), filename = paste0('../data/raster/CHIRPS/vll/', names(ct), '.tif'), overwrite = TRUE)
}


# Load data ---------------------------------------------------------------
fls <- list.files('E:/data/CHIRPS', full.names = TRUE, pattern = '.tif$')
yrs <- str_sub(fls, 28, 31) %>% unique() %>% as.numeric()
vll <- shapefile('../data/shp/valle_limite.shp')

# Extract by mask ---------------------------------------------------------
cl <- makeCluster(6)
registerDoSNOW(cl)
foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  myCrop(yr = yrs[i])
}
stopCluster(cl)
