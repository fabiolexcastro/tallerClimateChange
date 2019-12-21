
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue, gtools, foreach, parallel, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
fls <- list.files('../data/raster/TERRACLIMATE/vll_lim/bios', full.names = TRUE, pattern = '.asc$')
stk <- stack(fls)

# Raster to table
tbl <- rasterToPoints(stk)
tb2 <- as.data.frame(tbl) %>% 
  as_tibble(.) %>% 
  mutate(id = 1:nrow(.)) %>% 
  gather(var, value, -id, -x, -y) %>% 
  mutate(year = str_sub(var, 2, 5),
         variable = str_sub(var, 7, nchar(var))) %>% 
  dplyr::select(id, x, y, year, variable, value)
 
dir.create('../data/tbl/terraclimate')
write.csv(tb2, '../data/tbl/terraclimate/climate_bios_valle.csv', row.names = FALSE)
