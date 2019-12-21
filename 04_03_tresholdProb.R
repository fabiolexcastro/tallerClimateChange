
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, rgdal, data.table, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
load('../rData/run_1/clustereddata.rData')
lyr <- raster('../rf/output/run1/results/raw/RF_5Prob_current.asc')
occ <- tbl_df(occ)

# Extracting the values
vls <- raster::extract(lyr, occ[,1:2])
vls <- vls[!is.na(vls)]
length(vls)
qnt_05 <- quantile(vls, seq(0, 1, 0.01))

vls_df <- as.data.frame(qnt_05)
threshold <- as.numeric(subset(vls_df, rownames(vls_df) == '5%'))
save(threshold, file = '../rData/run_1/threshold_prob.rData')

