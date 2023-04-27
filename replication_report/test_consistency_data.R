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
  dir.create("PA_matching_asis")
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

if (!file.exists("PA_matching_asis/aois.gpkg")) {
  aws.s3::save_object(
    object = "Replication_wolf/aois.gpkg",
    bucket = "fbedecarrats",
    file = "PA_matching_asis/aois.gpkg",
    overwrite = TRUE,
    region = "")
}



# Load files into memory
files <- list.files("PA_matching_asis/processed_rasters", pattern = "\\.tif$", 
                    full.names = TRUE) # List available files
file_names <- list.files("PA_matching_asis/processed_rasters", pattern = "\\.tif$", 
                         full.names = FALSE) %>%
  str_remove("\\.tif") # Prepare the names (no path or extension)
data <- map2(files, file_names, ~ rast(.x, names = )) # Load in a list
names(data) <- file_names # name the elements
list2env(data, envir = .GlobalEnv) # split the elements
rm(data)
data <- c(carbon, countries, cover, cover_loss, drivers, ecoregions,
          elev, forest_biome, gain, loss, lossyear, non_threatened, PAs,
          PAs_buffer, pop_dens, slope, threatened, travel_time) 
names(data) <- file_names

# Prepare AOIs to iterate upon
aois <- read_sf("PA_matching_asis/aois.gpkg") %>%
  st_transform(crs = crs(cover))

all_data <- data.frame()
n_aois <- nrow(aois)
for (i in 1:nrow(aois)) {
  aoi <- aois[i, ]
  print(paste(aoi$GID_0, i, "/", n_aois))
  aoi_data <- data %>%
    crop(aoi) %>%
    mask(aoi) %>%
    as.data.frame() %>%
    filter(ecoregions >= 0 & countries >= 0 & drivers >= 0 & forest_biome == 1 &
             slope >= 0 & !is.na(cover_loss) & !is.na(pop_dens))
  all_data <- bind_rows(all_data, aoi_data)
  rm(aoi_data)
}
write_parquet(all_data, "PA_matching_asis/processed_rasters/all_data.parquet")
aws.s3::put_object(file = "PA_matching_asis/processed_rasters/all_data.parquet",
                   object = "PA_matching_asis/all_data.parquet",
                   bucket = "fbedecarrats",
                   region = "",
                   overwrite = TRUE,
                   multipart = TRUE)

data <- read_parquet("PA_matching_asis/processed_rasters/all_data.parquet",
                     as_data_frame = FALSE)
# Stats for incomplete data 
stats <- data %>%
  mutate(with_forest_cover = cover > 0,
         with_loss = loss > 0,
         with_lossyear = lossyear > 0) %>%
  group_by(with_forest_cover, with_loss, with_lossyear) %>%
  summarise(n = n()) %>%
  collect()
stats <- stats %>%
  mutate(percent = round(n / sum(stats$n) * 100, 2)) 
# By treatmnent/control
stats2 <- data %>%
  mutate(with_forest_cover = cover > 0,
         with_loss = loss > 0,
         with_lossyear = lossyear > 0,
         is_treatment = PAs == 1) %>%
  group_by(with_forest_cover, with_loss, with_lossyear, is_treatment) %>%
  summarise(n = n()) %>%
  collect() %>%
  filter(with_forest_cover & with_loss) 

no_lossyear_control = (stats2 %>% 
                         filter(with_forest_cover, with_loss, !is_treatment) %>%
                         filter(!with_lossyear) %>%
                         pluck("n")) /
  (ungroup(stats2) %>% 
     filter(with_forest_cover, with_loss, !is_treatment) %>%
     summarise(n = sum(n)) %>% pluck("n")) 


no_lossyear_treatment = (stats2 %>% 
                         filter(with_forest_cover, with_loss, is_treatment) %>%
                         filter(!with_lossyear) %>%
                         pluck("n")) /
  (ungroup(stats2) %>% 
     filter(with_forest_cover, with_loss, is_treatment) %>%
     summarise(n = sum(n)) %>% pluck("n")) 



stats2_bal <- stats2 %>%
  filter(with_forest_cover & with_loss) %>%
  mutate(tot)
  

data_mdg %>%
  summarise(n = n(),
            with_cover = sum(cover > 0),
            loss_noyear = sum((loss > 0 & lossyear == 0)),
            loss_noyear_share = loss_noyear / n * 100,
            loss_noyear_share_cover = loss_noyear / with_cover * 100)

countries %>%
  crop(aoi_mada) %>%
  plot()

hist(test$lossyear)

cover2 <- rast("PA_matching_asis/processed_rasters/cover.tif", lyrs = c("cover"))

carbon2 <- rast("PA_matching_asis/processed_rasters/carbon.tif", lyrs = 1, names = c("carbon"))
identical(cover, cover2)





aoi_mada2 <- st_transform(aoi_mada, crs = crs(cover2))

cover3 <- cover2 %>%
  crop(aoi_mada2)

plot(cover3)

tm_shape(cover2) +
  tm_raster()

# aoi <- aois %>%
#   mutate(rownum = row_number(), .before = everything()) %>%
#   filter(GID_0 == "MDG")
aois_mada <- aois %>%
  filter(GID_0 == "MDG" & biome_num == 1)

lossyear <- rast("PA_matching_asis/processed_rasters/lossyear.tif") %>%
  crop(aois_mada)
loss <- rast("PA_matching_asis/processed_rasters/loss.tif") %>%
  crop(aois_mada)
both <- c(lossyear, loss)


tm_shape(loss) +
  tm_raster() + 
  tm_shape(aois_mada) + 
  tm_polygons()

cover_mdg <- cover %>%
  crop(aoi) %>%
  as.data.frame()

cover_mdg2 <- cover_mdg %>%
  filter(!is.na(cover))

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



