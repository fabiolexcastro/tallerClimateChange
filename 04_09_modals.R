
# Load libraries
library(pacman)
pacman::p_load(raster, rgdal, tidyverse)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
yrs <- paste0('../rf/output/run1/results/process/mixed/_2050') %>%
  list.files(full.names = TRUE, pattern = '.asc$') %>% 
  stack()

# To make the modal
lyr.mdl <- raster::modal(yrs)
writeRaster(lyr.mdl, paste0('../rf/output/run1/results/process/mixed/RF_3classes_unc_future.asc'), overwrite = TRUE)





