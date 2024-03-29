
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, tidyverse, magrittr)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
run <- 'run1'

# Load data ---------------------------------------------------------------
crn <- raster(paste0('../rf/output/', run, '/results/process/RF_5Classes_unc_current.asc'))
f50 <- raster(paste0('../rf/output/', run, '/results/process/mixed/RF_3classes_unc_future.asc'))

all_options <- read_csv('../data/tbl/impactGradient/impactGradient.csv')
unique(all_options$category) 
labelss <- data.frame(value = c(0, 1, 2, 3, 4, 5), category = c('Unsuit', 'cope', 'adjust', 'transform', 'opportunity', 'resilience'))

# Function to use
impGra <- function(crn, ftr){
  
  crn <- crn
  ftr <- f50
  
  msk <- crn * 0
  crd_df <- coordinates(crn)
  
  x <- raster::extract(crn, crd_df, cellnumbers = TRUE) %>% as_data_frame()
  ncell <- dplyr::select(x, cells)
  x <- select_(x, names(crn))
  colnames(x) <- 'current'
  
  y <- raster::extract(ftr, crd_df[,c('x', 'y')], cellnumbers = TRUE) %>% as_data_frame()
  y <- select_(y, names(ftr))
  colnames(y) <- 'future'
  
  z <- data.frame(x, y, ncell) %>% as_tibble()
  
  print('To Results')
  rslts <- left_join(z, all_options, by = c('current', 'future'))
  labls <- as_tibble(labelss) %>% mutate(category = as.character(category))
  
  final <- full_join(rslts, labls, by = 'category') %>%
    dplyr::select(value) %>%
    pull(1)
  
  final <- left_join(rslts, labls, by = 'category') %>%
    dplyr::select(value) %>%
    pull(1)
  
  length(final)
  length(msk)
  hist(final)
  
  rst <- raster::setValues(msk, final)
  return(rst)
  
}

hist(rst[])
writeRaster(rst, '../rf/output/run1/results/process/change_2050.tif', overwrite = TRUE) 

