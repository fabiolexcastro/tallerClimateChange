
# Load data ---------------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(lisst = ls())
options(scipen = 999)