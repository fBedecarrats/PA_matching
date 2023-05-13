library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)
library(tmap)

if (!stringr::str_ends(getwd(), "replication_report")) {
  setwd("replication_report")
}

pas <- rast("data_processed/rasters/PAs_old.tif")

worldmap <- ne_download(scale = 110,
                       type = "countries",
                       category = "cultural",
                       destdir = tempdir(),
                       load = TRUE,
                       returnclass = "sf")

mada <- worldmap %>%
  filter(SOV_A3 == "MDG") %>%
  st_transform(crs = crs(pas))

st_crs(mada)

tm_shape(mada) +
  tm_polygons()

mada_pas <- pas %>%
  crop(mada)

tm_shape(mada) +
  tm_polygons() + 
tm_shape(mada_pas) +
  tm_raster()

pas2 <- rast("data_processed/rasters/PAs.tif")
mada_pas2 <- pas2 %>%
  crop(mada)


tm_shape(mada) +
  tm_polygons() + 
  tm_shape(mada_pas2) +
  tm_raster()
