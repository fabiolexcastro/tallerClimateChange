
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse)

rm(list = ls())

# Functions to use --------------------------------------------------------
download_chirps <- function(year){
  print(year)
  dates <- seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by="days")
  dates <- gsub('-', '.', as.character(dates))
  paths <- paste0(pth, year, '/chirps-v2.0.', dates, '.tif.gz')
  dir.create(paste0('../data/raster/CHIRPS/world/', year))
  lapply(1:length(paths), function(k){
    download.file(url = paths[k],
                  destfile = paste0('../data/raster/CHIRPS/world/', year, '/', basename(paths[k])),
                  mode = 'wb')
  })
}

# Load data ---------------------------------------------------------------
pth <- 'ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/'
yrs <- 1981:2018

dir.create('../data/raster/CHIRPS/world', recursive = TRUE)

# Apply the function ------------------------------------------------------
download_chirps(year = 1982)
yrs <- 1983:1995
map(.x = yrs, .f = download_chirps)
