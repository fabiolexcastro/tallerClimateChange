
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue, broom, gtools, foreach, parallel, tidyr, doSNOW, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
calcSlope <- function(x){
  sl <- sens.slope(x)
  return(sl$estimates)
}
calcPvalue <- function(x){
  pv <- sens.slope(x)
  return(pv$p.value)
}
makeGraph <- function(vr, i){
  df <- rsl_sub %>% 
    filter(variable == vr &
             id == i)
  vl <- tbl %>% 
    filter(variable == vr &
             id == i) 
  nm <- lbl %>% 
    filter(Abreviatura == vr) %>% 
    pull(2)
  gg <- ggplot(data = vl, aes(x = year, y = value)) +
    stat_smooth(se = TRUE, method = 'gam', size = 1.2) +
    geom_line(size = 1.2) +
    labs(x = '',
         y = nm) +
    theme_bw() +
    theme() 
  ggsave(plot = gg, filename = glue('../png/trends/terraclimate/gg_{vr}_{i}.png'), units = 'in', width = 12, height = 9, dpi = 300)
}

# Load data ---------------------------------------------------------------
tbl <- read_csv('../data/tbl/terraclimate/climate_bios_valle.csv')
lbl <- read_csv('../data/tbl/names/label_bioclimaticas.csv') %>% mutate(Nombre = iconv(Nombre, to = 'latin1'))
vrs <- unique(tbl$variable)

# Calc Slope --------------------------------------------------------------
slp <- tbl %>% 
  group_by(id, variable) %>% 
  dplyr::summarise(slope = calcSlope(value)) %>% 
  ungroup()
pvl <- tbl %>% 
  group_by(id, variable) %>% 
  dplyr::summarise(p_value = calcPvalue(value)) %>% 
  ungroup()
rsl <- inner_join(slp, pvl, by = c('id', 'variable')) %>% 
  mutate(significative = case_when(p_value < 0.05 ~ TRUE,
                                   TRUE ~ FALSE))
crd <- tbl %>% 
  filter(year == '1980' &
           variable == 'bio_1') %>% 
  dplyr::select(id, x, y)

# Union con las coordenadas
rsl <- inner_join(rsl, crd, by = 'id') %>% 
  dplyr::select(id, x, y, variable, slope, p_value, significative)

rsl_sub <- rsl %>% 
  filter(significative == TRUE)

# Making the graphs -------------------------------------------------------
vrs <- unique(rsl_sub$variable) %>% mixedsort()

# Variable Bio 6
ids_6 <- rsl_sub %>% 
  filter(variable == 'bio_6') %>% 
  pull(id) %>% 
  mixedsort()
ids_8 <- rsl_sub %>% 
  filter(variable == 'bio_8') %>% 
  pull(id) %>% 
  mixedsort()

for(i in 1:length(ids_6)){
  makeGraph(vr = 'bio_6', i = ids_6[i])
}

for(i in 1:length(ids_8)){
  makeGraph(vr = 'bio_8', i = ids_8[i])
}










