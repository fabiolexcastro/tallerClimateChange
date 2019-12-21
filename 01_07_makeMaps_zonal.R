
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, glue, gtools, foreach, parallel, doSNOW, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Functions to use --------------------------------------------------------
makeMap <- function(fle, var){
  # fle <- fls[1]
  # var <- 'bio_12.asc'
  
  rc <- str_sub(basename(fle), start = 5, end = 9)
  yr <- str_sub(basename(fle), start = 11, end = 19)
  print(glue('{rc} {yr}'))
  
  rs <- glue('../data/raster/DIFFERENCE_MEAN/{rc}/{yr}') %>% 
    list.files(., full.names = TRUE, pattern = '.asc$') %>% 
    mixedsort() %>% 
    grep(var, ., value = TRUE) %>% 
    raster()
  
  if(var == 'bio_1.asc'){
    rs <- rs / 10
  } else {
    rs <- rs
  }
  
  sf <- st_read(fle) %>% 
    dplyr::select(NOM_MUNICI, str_replace(string = var, '.asc', '')) %>% 
    setNames(c('NOM_MUNICI', 'VALUE', 'geometry')) %>% 
    as(., 'Spatial')
  vl <- rasterToPoints(rs) %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', 'value'))
  
  # Extract the values from the polygons
  lb <- coordinates(sf) %>% 
    as.data.frame() %>% 
    mutate(value = round(sf@data$VALUE, 1)) %>% 
    as_tibble() 
  
  if(var == 'bio_1.asc'){
    plt <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")
  } else {
    plt <- RColorBrewer::brewer.pal(n = 10, name = "BrBG")
  }
  
  gg <- ggplot(vl)  +
    geom_tile(aes(x = x, y =  y, fill = value)) +
    scale_fill_gradientn(colours = plt, 
                         na.value = 'white') +
    geom_polygon(data = sf, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
    theme_bw() +
    coord_equal() +
    labs(x = 'Longitud', y = 'Latitud', fill = '') +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5),
          # panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.width = unit(5, 'line'),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    guides(shape = guide_legend(override.aes = list(size = 10))) +
    annotate('text', x = lb$V1, y = lb$V2, label = lb$value, size = 3.5) +
    ggtitle(label = glue('{gsub(".asc","",var)} {rc}, {yr}'))
  ggsave(plot = gg, filename = glue('../png/diff_zonal/{gsub(".asc", "", var)}_{rc}_{yr}.png'), units = 'in', width = 10, height = 9, dpi = 300)
}

# Load data ---------------------------------------------------------------
fls <- list.files('../data/shp/difference', full.names = TRUE, pattern = '.shp$')
nms <- basename(fls)

# Making the maps ---------------------------------------------------------
for(i in 1:length(fls)){
  makeMap(fle = fls[i], var = 'bio_1.asc')
}

for(i in 1:length(fls)){
  makeMap(fle = fls[i], var = 'bio_12.asc')
}



