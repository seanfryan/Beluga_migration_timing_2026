# =====================================================
# 00_process_ice_data.R
# Process raw ice cover shapefiles into a single RDS dataset
# NOTE: Raw shapefiles sourced from:
#   https://drive.google.com/drive/u/0/folders/1RkHCc6HgLxsI8dwROna6w4rfMKGMDvBE
# If raw shapefiles are unavailable, use pre-computed data/working/ice_data.rds
# =====================================================

rm(list = ls())

library(sf)
library(tidyverse)
library(data.table)

## Set path to ice data (shape files)
directory_path <- file.path("data", "raw", "ice_cover", "New_Shapefiles")

## Determine files available
file_paths <- list.files(path = directory_path, pattern = "\\.shp$", full.names = TRUE)
print(as_tibble(file_paths), n = Inf)

## Load all ice data files
sf_objects <- list()

for (file_path in file_paths) {
  date <- gsub(".*_([0-9]{8}).*", "\\1", file_path)
  sf_objects[[file_path]] <- st_read(file_path, quiet = TRUE) %>%
    mutate(date = as.Date(date, format = "%Y%m%d"))
}

## Combine all sf objects into one
combined_sf <- sf::st_as_sf(data.table::rbindlist(sf_objects, fill = TRUE))

## Add date columns that match whale location data
ice_data <-
  combined_sf %>%
  mutate(
    date = as.Date(date, format = "%Y%m%d"),
    year = year(date),
    month = month(date),
    day = day(date),
    yday = yday(date)
  ) %>%
  mutate(
    sampling_year = case_when(yday < 155 ~ year - 1,
                              yday >= 155 ~ year),
    sampling_day = case_when(yday < 155 ~ yday + 365,
                             yday >= 155 ~ yday)
  )

## Subset to just columns of interest
ice_data <- 
  ice_data %>%
  dplyr::select(AREA, PERIMETER, POLY_TYPE, N_CT, date, geometry, year, month, day, yday, sampling_year, sampling_day) %>%
  filter(!is.na(N_CT))

## Save ice data
saveRDS(ice_data, "data/working/ice_data.rds")
message("Saved: data/working/ice_data.rds")
