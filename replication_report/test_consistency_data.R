
# Load libraries ----------------------------------------------------------

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

# Teste format de la variable PAs
pas <- rast("PA_matching_asis/processed_rasters/PAs.tif")
pas_df <- pas %>%
  as.data.frame()
test <- pas_df %>%
  group_by(layer) %>%
  summarise(n = n())
###############

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
data <- map(files, rast) # Load in a list
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

for (i in 1:n_aois) {
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

write_parquet(all_data, "PA_matching_asis/processed_rasters/all_data_wolf.parquet")
aws.s3::put_object(file = "PA_matching_asis/processed_rasters/all_data_wolf.parquet",
                   object = "PA_matching_asis/all_data_wolf.parquet",
                   bucket = "fbedecarrats",
                   region = "",
                   overwrite = TRUE,
                   multipart = TRUE)
rm(list = ls())


# Load tabularized wolf raster data and PAs' csv -------------------------------
data <- read_parquet("PA_matching_asis/processed_rasters/all_data_wolf.parquet")

wdpa <- read_csv("PA_matching_asis/data_processed/PAs_tab.csv") %>%
  select(ID, WDPAID, IUCN_CAT, MARINE, STATUS, GIS_AREA,
         status_year = STATUS_YR)

# Join tabularized wolf raster data and PAs ------------------------------------
data <- data %>%
  left_join(wdpa, by = join_by(PAs == ID)) %>%
  mutate(treatment = case_when(
    MARINE == 0 & STATUS != "Proposed" & GIS_AREA >= 1 ~ 1,
    .default = 0)) %>% # PAs have a value 1 at PA_buffer
  filter(!(PAs_buffer - sum(PAs > 0, na.rm = TRUE) > 0)) # On enlève les buffers


data %>%
  group_by(treatment) %>%
  summarise(n = n())

# We check that we have no pixel with a raster value for PA and no match in
# the WDPA dataset
testthat::expect_equal(
data %>%
  mutate(is_PA = !is.na(PAs),
         has_wdpaid = !is.na(WDPAID)) %>%
  group_by(is_PA, has_wdpaid) %>%
  summarise(n = n()) %>%
  filter((is_PA != has_wdpaid)) %>%
  nrow(), 0
)



# Stats for incomplete data by treatmnent/control
test_lossyear_presence <- data %>%
  mutate(with_forest_cover = cover > 30,
         with_loss = loss > 0,
         with_lossyear = lossyear > 0) %>%
  group_by(with_forest_cover, with_loss, with_lossyear, treatment) %>%
  summarise(n = n()) %>%
  collect() %>%
  filter(with_forest_cover & with_loss) 


no_lossyear_control = (test_lossyear_presence %>% 
                         filter(with_forest_cover, with_loss, treatment == 0) %>%
                         filter(!with_lossyear) %>%
                         pluck("n")) /
  (ungroup(test_lossyear_presence) %>% 
     filter(with_forest_cover, with_loss, treatment == 0) %>%
     summarise(n = sum(n)) %>% pluck("n")) 

no_lossyear_treatment = (test_lossyear_presence %>% 
                         filter(with_forest_cover, with_loss, treatment == 1) %>%
                         filter(!with_lossyear) %>%
                         pluck("n")) /
  (ungroup(test_lossyear_presence) %>% 
     filter(with_forest_cover, with_loss, treatment == 1) %>%
     summarise(n = sum(n)) %>% pluck("n")) 

# Matching
tic()
matched_all <- matchit(# the first line of covar is coarsened
  formula = treatment ~ elev + slope + cover + travel_time + pop_dens +
    countries + ecoregions + drivers, # these on 2nd line remain exact
  method = "cem",
  # for cutpoints 0 means no coarsening, and q5 means quintiles
  cutpoints = list(elev = "q5", slope = "q5", cover = "q5",
                   travel_time = "q5", pop_dens = "q5",
                   countries = 0, ecoregions = 0, drivers = 0),
  # k2k = TRUE, # To get a 1-to-1 matching 
  data = data)
toc() # 222.13 sec elapsed

tic()
matched_data <- match.data(matched_all)
toc() # 15.54 sec elapsed

matched_data <-  matched_data %>%
  mutate(subclass = as.integer(subclass))

dir.create("PA_matching_asis/consolidated")
write_parquet(matched_data, "PA_matching_asis/consolidated/matched_data.parquet",
                )
# matched_data <- read_parquet("revamp/consolidated/matched_data.parquet")
# tic()
matched_data %>%
  group_by(treatment) %>%
  write_dataset("PA_matching_asis/consolidated/matched_data")
rm(list = c("matched_data", "matched_all", "data", "wdpa"))


# Compute deforestation ---------------------------------------------------

# A function that computes the deforestation before and after status year for
# protected area pixels, and before and after matched treatment status year for 
# control pixels. Uses arrow for fast and larger-than-memory calculations
calc_defor_mode <- function(first_year, last_year, matched_treatment,
                            matched_control) {
  subset_trt <- matched_treatment %>%
    filter(status_year %in% first_year:last_year)
  
  subset_ctrl <- matched_control %>%
    select(-status_year)
  
  subclasses_trt <- subset_trt %>%
    select(subclass, status_year) %>%
    unique()
  
  defor_to_compute <- subclasses_trt %>%
    inner_join(subset_ctrl, by = "subclass")
  
  defor_computed <- defor_to_compute %>%
    mutate(before = lossyear >= first_year & lossyear < status_year,
           before = as.integer(before), 
           after = lossyear > status_year & lossyear <= last_year,
           after = as.integer(after),
           CL_b = cover * loss * before,
           CL_a = cover * loss * after) %>%
    group_by(subclass, status_year) %>%
    summarise(C = sum(cover, na.rm = TRUE),
              CL = sum(cover * loss, na.rm = TRUE),
              CL_b = sum(CL_b, na.rm = TRUE),
              CL_a = sum(CL_a, na.rm = TRUE)) %>%
    mutate(Y_b = status_year - first_year,
           Y_a = last_year - status_year,
           loss = - (((C - CL)/C)^(1/18) - 1),
           loss_before = - (((C - CL_b)/C)^(1/Y_b) - 1),
           loss_after = - (((C - CL_a)/C)^(1/Y_a) - 1)) %>%
    select(subclass, status_year, ctrl_loss = loss, 
           ctrl_loss_before = loss_before, ctrl_loss_after = loss_after)
  
  expanded <- subset_trt %>%
    left_join(defor_computed, by = c("subclass", "status_year"))
  
  complete <- expanded %>%
    ungroup() %>%
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
           loss = - (((C - CL)/C)^(1/18) - 1),
           loss_before = - (((C - CL_b)/C)^(1/Y_b) - 1),
           loss_after = - (((C - CL_a)/C)^(1/Y_a) - 1)) %>%
    select(-before, -after, -C, -CL, -CL_b, -CL_a, -Y_a, -Y_b) %>%
    rename(trt_loss = loss, 
           trt_loss_before = loss_before, 
           trt_loss_after = loss_after)
  return(collect(complete))
}

# Set parameters as in the original study
tic()
calc_defor_mode(
  first_year = 2001,
  last_year = 2018,
  matched_treatment = open_dataset(
    "PA_matching_asis/consolidated/matched_data/treatment=1"),
  matched_control  = open_dataset(
    "PA_matching_asis/consolidated/matched_data/treatment=0")) %>%
  write_parquet("PA_matching_asis/consolidated/matched_df.parquet")
toc() # 280.07 sec elapsed

# Write output
tic()
write_parquet(df, "PA_matching_asis/consolidated/matched_df.parquet")
toc()


















no_lossyear_treatment = (stats2 %>% 
                         filter(with_forest_cover, with_loss, is_treatment) %>%
                         filter(!with_lossyear) %>%
                         pluck("n")) /
  (ungroup(stats2) %>% 
     filter(with_forest_cover, with_loss, is_treatment) %>%
     summarise(n = sum(n)) %>% pluck("n")) 


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



