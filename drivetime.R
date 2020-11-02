library(tidyr)
library(dplyr)
library(ecat)
library(sf)
library(tigris)
library(tmap)
tmap_mode("plot")

#ecat bounding box
ecat_bb <-
  st_bbox(ecat:::elevation.raster) %>%
  st_as_sfc(ecat_bb) %>%
  st_transform(5072)

drive_all<-read.Rds("pepr_isochrones_no_overlap_5072.rds")