library(tidyverse)
library(sf)
library(tmap)
library(geodata)
library(rnaturalearth)
library(units)
library(terra)
library(stars)
library(arrow)
library(tidyterra)
library(MatchIt)
library(tictoc)


# Get files prepared using the scripts of the initial study
## List files
all_tifs <- aws.s3::get_bucket_df(
  bucket = "fbedecarrats",
  prefix = "Replication_wolf/data_processed/rasters",
  region = "")
my_tifs <- all_tifs$Key
my_tifs_dest <- str_replace(my_tifs, 
                            "Replication_wolf/data_processed/rasters",
                            "PA_matching_asis/processed_rasters")

dir.create("PA_matching_asis/processed_rasters")
## A function to iterate/vectorize copy
save_from_s3 <- function(x, y) {
  aws.s3::save_object(
    object = x,
    bucket = "fbedecarrats",
    file = y,
    overwrite = TRUE,
    region = "")
}
# copy from S3 to local
map2(my_tifs, my_tifs_dest, save_from_s3)

# Load files into memory
files <- list.files("PA_matching_asis/processed_rasters", pattern = "\\.tif$", 
                    full.names = TRUE) # List available files
file_names <- list.files("PA_matching_asis/processed_rasters", pattern = "\\.tif$", 
                    full.names = FALSE) %>%
  str_remove("\\.tif") # Prepare the names (no path or extension)
data <- map(files, rast) # Load in a list
names(data) <- file_names # name the elements
list2env(data, envir = .GlobalEnv) # split the elements
data <- c(carbon, countries, cover, cover_loss, drivers, ecoregions,
          elev, forest_biome, gain, loss, lossyear, non_threatened, PAs,
          PAs_buffer, pop_dens, slope, threatened, travel_time) 

data %>%
  as.data.frame() %>%
  write_parquet("PA_matching_asis/processed_rasters/consolidated.parquet")


mada_rast <- rast("revamp/gee_mada1/MDG_2023_04_17_11_00_41.tif")
mada_df <- as.data.frame(my_rast) %>%
  mutate(is_pa_buffer = is_pa_buffer - is_pa,
         status_year = ifelse(status_year == 0, NA, status_year),
         wdpa_id = ifelse(wdpa_id == 0, NA, wdpa_id)) %>%
  filter(country_num > 0 & biome_num > 0 & is_pa_buffer != 1)

mada2 <- mada_df %>%
  filter(is_pa == 1) %>%
  filter(loss > 0) %>%
  head(10000) 

calc_defor_annual <- function(x, min_year = 2001, max_year = 2018) {
  x <- x %>%
    mutate(row_id = row_number())
  defor <- x %>%
    select(row_id, status_year, starts_with("lossyear_")) %>%
    pivot_longer(-c(row_id, status_year)) %>%
    mutate(loss_year = as.integer(gsub("lossyear_", "", name))) %>%
    filter(loss_year >= min_year & loss_year <= max_year) %>%
    mutate(loss_before = ifelse(loss_year < status_year, value, 0),
           loss_after = ifelse(loss_year > status_year, value, 0)) %>%
    group_by(row_id) %>%
    summarise(loss_before_annual = sum(loss_before, na.rm = TRUE),
              loss_after_annual = sum(loss_after, na.rm = TRUE))
  x <- left_join(x, defor, by = join_by(row_id))
  return(x)
}

mada3 <- calc_defor_annual(mada2) %>%
  mutate(ratio = loss_after_annual/loss_2021)

73.54545 + 12.15385 + 518.4286 + 23.625 + 65.85714

# From Wolf source
aois_mada <- aois %>%
  filter(GID_0 == "MDG" & biome_num == 1)

library(tidyterra)
lossyear <- rast("data_input/GEE/GEE_rasts/lossyear.tif") %>%
  crop(aois_mada)
loss <- rast("data_input/GEE/GEE_rasts/loss-0000000000-0000023296.tif") %>%
  crop(aois_mada)
both <- c(lossyear, loss)

test <- as.data.frame(both)

no_prob <- test %>%
  filter(loss > 0 & lossyear != 0)
problem <- test %>%
  filter(loss > 0 & lossyear == 0)

tm_shape(loss) +
  tm_raster()
  
