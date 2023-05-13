library(tidyverse)
library(aws.s3)

if (!stringr::str_ends(getwd(), "replication_report")) {
  setwd("replication_report")
}


# A function to put data from local machine to S3
put_to_s3 <- function(from, to) {
  aws.s3::put_object(
    file = from,
    object = to,
    bucket = "fbedecarrats",
    region = "",
    multipart = TRUE)
}

# A function to iterate/vectorize copy
save_from_s3 <- function(from, to) {
  aws.s3::save_object(
    object = from,
    bucket = "fbedecarrats",
    file = to,
    overwrite = FALSE,
    region = "")
}

# Listing files in bucket
my_files <- get_bucket_df(bucket = "fbedecarrats",
                          prefix = "data_processed",
                          region = "") %>%
  pluck("Key")

my_files_dest <- str_replace(my_files, "data_processed", "data_processed_new")
dir.create("data_processed_new")
dir.create("data_processed_new/rasters")
map2(my_files, my_files_dest, save_from_s3)
