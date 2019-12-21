
# Load libraries and initial setup ----------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, readxl, tidyverse, glue, broom, gtools, foreach, parallel, tidyr, doSNOW, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, 
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
fls <- list.files('E:/data/IDEAM/Update_2019_10_07', full.names = TRUE, pattern = '.data$')
stt <- read_excel('E:/data/IDEAM/Update_2019_10_07/CNE_OE.xls') 
stt <- stt %>% 
  filter(DEPARTAMENTO == 'Valle del Cauca')
sts <- unique(stt$CODIGO)
fls <- grep(paste0(sts, collapse = '|'), fls, value = TRUE)
%>% 
tbl <- read. %>% table(fls[2], sep = '|') %>% 
  as_tibble()
View(tbl)

summary(tbl$V2)


