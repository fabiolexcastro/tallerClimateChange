
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue, gtools, foreach, parallel, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
myJoin <- function(rc, yr){
  print(glue('{rc} {yr}'))
  df <- tbl %>% 
    filter(rcp == rc &
             year == yr) %>% 
    group_by(zone, NOM_MUNICI, year, rcp) %>% 
    dplyr::summarise_all(.funs = mean) %>% 
    ungroup() %>% 
    dplyr::select(-gc)
  rs <- inner_join(shp, df, by = 'NOM_MUNICI')
  st_write(obj = rs, dsn = '../data/shp/difference', layer = glue('shp_{rc}_{yr}'), driver = 'ESRI Shapefile', update = TRUE, delete_layer = TRUE)
  print('Done!')  
}

# Load data ---------------------------------------------------------------
tbl <- read_csv('../data/tbl/difference/tbl_difference_all.csv') %>% 
  mutate(NOM_MUNICI = iconv(NOM_MUNICI, to = 'latin1'))
shp <- st_read('../data/shp/mpios_geo_ok.shp') %>% 
  filter(NOMBRE_DPT == 'VALLE DEL CAUCA')
rcp <- unique(tbl$rcp)
yrs <- unique(tbl$year)

# Apply the function -----------------------------------------------------
for(i in 1:length(rcp)){
  for(j in 1:length(yrs)){
    myJoin(rc = rcp[i], yr = yrs[j]) 
  }
}

# Fin de el script --------------------------------------------------------