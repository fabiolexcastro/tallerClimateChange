
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
myExtract <- function(rc, yr){
  # rc <- rcp[1]; yr <- yrs[1]
  pt <- glue('{pth}/col/{rc}/{yr}/_grid') 
  gcm <- list.files(pt, full.names = TRUE)
  rst <- list()
  for(i in 1:length(gcm)){
    gc <- basename(gcm[i])
    print(gc)
    fls <- glue('{pt}/{basename(gcm[i])}') 
    fls <- glue('{fls}/{paste0("bio_", 1:19)}')  
    rst[[i]] <- stack(fls)
    rst[[i]] <- raster::crop(rst[[i]], vll) %>% 
      raster::mask(., vll)
    dir <- glue('{pth}/vll/{rc}/{yr}/{gc}')
    dir.create(dir)
    nms <- paste0(names(rst[[i]]), '.asc')
    Map('writeRaster', x = unstack(rst[[i]]), filename = glue('{dir}/{nms}'), overwrite = FALSE)
  }
}

# Load data ---------------------------------------------------------------
pth <- '../data/raster/IPCC/data'
rcp <- glue('{pth}/col') %>% 
  list.files()
yrs <- glue('{pth}/col/{rcp[1]}') %>% 
  list.files()
vll <- shapefile('../data/shp/mpios_geo_ok.shp')

# Selecting and aggregating data in Valle del Cauca
vll <- vll[vll@data$NOMBRE_DPT %in% c('VALLE DEL CAUCA'),]
lim <- aggregate(vll, 'COD_DEPTO')
shapefile(lim, '../data/shp/valle_limite.shp')

# Applying the function for all the folders -------------------------------
lapply(1:length(rcp), function(r){
  lapply(1:length(yrs), function(y){
    myExtract(rc = rcp[r], yr = yrs[y])
  })
})

# End of the code ---------------------------------------------------------











