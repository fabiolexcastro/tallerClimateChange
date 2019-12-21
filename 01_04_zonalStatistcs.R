
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue, gtools, foreach, parallel, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
zonal <- function(rc, yr){
  # rc <- rcp[1]; yr <- yrs[1]
  gcm <- glue('../data/raster/DIFFERENCE/{rc}/{yr}') %>% 
    list.files(., full.names = TRUE)
  zn <- list()
  for(i in 1:length(gcm)){
    st <- list.files(gcm[i], full.names = TRUE, pattern = '.asc$') %>% 
      mixedsort() %>% 
      stack()
    ly <- rasterize(vll, st, field = 'gid')
    zn[[i]] <- raster::zonal(x = st, z = ly, fun = 'mean') %>% 
      as.data.frame(zn) %>% 
      inner_join(., vll@data, by = c('zone' = 'gid')) %>% 
      mutate(NOM_MUNICI = iconv(NOM_MUNICI, from = 'UTF-8', to = 'latin1')) %>% 
      as_tibble() %>% 
      dplyr::select(zone, NOM_MUNICI, bio_1:bio_19) %>% 
      gather(variable, value, -zone, -NOM_MUNICI) %>%  
      mutate(value = case_when(
        variable %in% c('bio_1', 'bio_5', 'bio_6', 'bio_7', 'bio_8', 'bio_9', 'bio_10', 'bio_11') ~ value / 10,
        TRUE ~ value)) %>% 
      spread(variable, value) %>% 
      mutate(rcp = rc,
             gc = basename(gcm[i]),
             year = yr) %>% 
      dplyr::select(zone, NOM_MUNICI, year, rcp, gc, glue('{paste0("bio_", 1:19)}'))
  }
  zn <- bind_rows(zn)
  write.csv(zn, glue('../data/tbl/difference/tbl_{rc}_{yr}.csv'),  row.names = FALSE)
}

# Load data ---------------------------------------------------------------
rcp <- list.files('../data/raster/DIFFERENCE')
yrs <- list.files('../data/raster/IPCC/data/vll/RCP25')
shp <- shapefile('../data/shp/mpios_geo_ok.shp')

# Filtering for only Valle del Cauca --------------------------------------
vll <- shp[shp@data$NOMBRE_DPT %in% 'VALLE DEL CAUCA',]
vll@data$gid <- 1:nrow(vll@data)

# Processing data ---------------------------------------------------------

# Paralleling process
nCores <- detectCores(all.tests = FALSE, logical = TRUE)
cl <- makeCluster(4) #Número de nucleos a utilizar
registerDoSNOW(cl)
foreach(i = 1:length(rcp), .packages = c('raster', 'rgeos', 'gtools', 'rgdal', 'dplyr', 'glue', 'tidyverse', 'foreach', 'parallel'),  .verbose = TRUE) %dopar% {
  foreach(j = 1:length(yrs)) %do% {
    zonal(rc = rcp[i], yr = yrs[j])
  }
} 
stopCluster(cl)
rm(nCores, cl)

# Reading the results table -----------------------------------------------
fls <- list.files('../data/tbl/difference', full.names = TRUE, pattern = '.csv$')
tbl <- map(.x = fls, .f = read_csv)
tbl <- bind_rows(tbl) %>% 
  mutate(NOM_MUNICI = iconv(NOM_MUNICI, to = 'latin1'))
write.csv(tbl, '../data/tbl/difference/tbl_difference_all.csv', row.names = FALSE)





