
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
calcDifference <- function(rc, yr){
  # rc <- rcp[1]; yr <- yrs[1]
  print(glue('{rc} {yr}'))
  gcm <- glue('../data/raster/IPCC/data/vll/{rc}/{yr}') %>% 
    list.files(., full.names = TRUE)
  for(i in 1:length(gcm)){
    gc <- gcm[i]
    st <- list.files(gc, full.names = TRUE) %>% 
      stack()
    df <- st - wcl
    dir <- glue('../data/raster/DIFFERENCE/{rc}/{yr}/{basename(gc)}')
    dir.create(dir, recursive = TRUE)
    Map('writeRaster', x = unstack(df), filename = glue('{dir}/{names(df)}.asc'), overwrite = TRUE)
  }
  print('Done!')
}

# Load data ---------------------------------------------------------------
wcl <- list.files('../data/raster/WORLDCLIM', full.names = TRUE, pattern = '.asc$') %>% 
  stack()
rcp <- list.files('../data/raster/IPCC/data/vll')
yrs <- list.files('../data/raster/IPCC/data/vll/RCP25')

# Apply the function ------------------------------------------------------
lapply(1:length(rcp), function(r){
  lapply(1:length(yrs), function(y){
    calcDifference(rc = rcp[r], yr = yrs[y])
  })
})

# End of the code ---------------------------------------------------------





