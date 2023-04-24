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
setwd("replication_report")

if (!dir.exists("PA_matching_asis/processed_rasters")) {
  dir.create("PA_matching_asis/processed_rasters")
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
}

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

all_df <- read_parquet("PA_matching_asis/processed_rasters/consolidated.parquet")
test <- all_df %>%
  filter(cover > 0) %>%
  summarise(n = n(),
            loss_no_lossyear = sum(loss > 0 & lossyear == 0, na.rm = TRUE),
            lossyear_no_loss = sum(lossyear > 0 & loss == 0, na.rm = TRUE)) 


all_df <- all_df %>%
  rename(lossyear = lossyear_as_wolf) %>%
  mutate(loss = loss / 10000,
         across(starts_with("lossyear_"), ~ .x / 10000),
         lossyear = lossyear + 2000)

all_PAs <- all_df %>%
  filter(is_pa == 1)

first_year <- 2001
last_year <- 2018

all_PAs2 <- all_PAs %>%
  mutate(before = lossyear >= first_year & lossyear < status_year,
         before = as.integer(before), 
         after = lossyear > status_year & lossyear <= last_year,
         after = as.integer(after),
         C = cover,
         CL = cover * loss,
         CL_b = CL * before,
         CL_a = CL * after,
         Y_b = status_year - first_year,
         Y_a = last_year - status_year,
         annual_loss = - (((C - CL)/C)^(1/18) - 1),
         loss_before = - (((C - CL_b)/C)^(1/Y_b) - 1),
         loss_after = - (((C - CL_a)/C)^(1/Y_a) - 1)) %>%
  select(-before, -after, -C, -CL, -CL_b, -CL_a, -Y_a, -Y_b) %>%
  rename(trt_annual_loss = annual_loss, 
         trt_annual_loss_before = loss_before, 
         trt_annual_loss_after = loss_after)


test <- all_PAs2 %>%
  filter(loss > 0) %>%
  head(10000) %>%
  collect()


table(test$loss)



