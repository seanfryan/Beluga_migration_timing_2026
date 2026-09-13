# =====================================================
# 01_data_processing.R
# Load raw whale data, filter, clean, produce:
#   - Figure 1: Study area bathymetry map
#   - Table 1: Tag deployment summary (metadata)
#   - Intermediate RDS files for downstream analysis
# =====================================================

rm(list = ls())

library(tidyverse)
library(readxl)
library(dplyr)
library(sf)
library(rnaturalearth)
library(geosphere)
library(purrr)
library(furrr)
library(argosfilter)
library(marmap)
library(data.table)
library(ggplot2)
library(ggspatial)
library(cowplot)

source("./R/utils.R")

## Set speed filter cut-off (m/s)
speed_cutoff_mps <- 7.5

################################################################################
##### Load whale data

## Location data; format time
whale_loc <- readr::read_csv("data/raw/allwhales.csv", show_col_types = FALSE) %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  mutate(date_time_rounded = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") %>%
    round_date(., unit = "hour"))

## Create sampling year with all days < 155 set to previous year
whale_loc <-
  whale_loc %>%
  mutate(
    sampling_year = case_when(day < 155 ~ year - 1,
                              day >= 155 ~ year),
    sampling_day = case_when(day < 155 ~ day + 365,
                             day >= 155 ~ day)
  )

## Need to give unique ids to whales in different years
whale_loc <-
  whale_loc %>%
  mutate(id = paste0(tag, "_", sampling_year), .before = tag)


##### Determine what samples are on land vs water

## Get the world's land boundaries
world <- readRDS("data/working/world.rds")
world_df <- world %>% st_transform(crs = st_crs(4326))

## Convert whale data to a simple feature object
whale_loc <- whale_loc %>% st_as_sf(coords = c("lon1", "lat1"),
                                     remove = FALSE,
                                     crs = 4326)

## Check if points fall on land
whale_loc$on_land <- st_intersects(whale_loc, world_df, sparse = FALSE) %>% apply(1, any)

## Remove extreme spatial outliers (locations far outside Cumberland Sound region)
whale_loc <-
  whale_loc %>%
  filter(lat1 >= 63 & lat1 <= 70 & lon1 >= -72 & lon1 <= -60)

## Summarize number of measurements by year on land/water and tag
waterland_by_year <- 
  whale_loc %>%
  group_by(sampling_year, tag) %>%
  summarise(
    total = n(),
    water_count = sum(!on_land, na.rm = TRUE),
    land_count = sum(on_land, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percent_water = (water_count / total) * 100,
         percent_land = (land_count / total) * 100)

## Filter to just points on water
whale_df_water <-
  whale_loc %>%
  filter(on_land == FALSE)

## Exclude short-duration deployments (<50 observations per tag-year)
## Paper reports 4 tags excluded (<14 days transmission); this threshold
## catches those deployments (e.g., 7927=9obs, 7928=9obs, 17000=15obs)
min_obs_threshold <- 50

tag_year_obs <- whale_df_water %>%
  st_drop_geometry() %>%
  count(tag, sampling_year, name = "n_obs")

short_deployments <- tag_year_obs %>% filter(n_obs < min_obs_threshold)
if (nrow(short_deployments) > 0) {
  message("Excluding short deployments (<", min_obs_threshold, " obs):")
  print(short_deployments)
}

whale_df_water <-
  whale_df_water %>%
  anti_join(short_deployments, by = c("tag", "sampling_year"))

## Also exclude deployments with poor spatial distribution (insufficient
## spatial coverage for migration analysis despite adequate observation count)
whale_df_water <-
  whale_df_water %>%
  filter(!(sampling_year == 1998 & tag == 20683)) %>%
  filter(!(sampling_year == 2008 & tag == 40623))


##### Handle duplicate date_times

## Create quantitative location quality
whale_df_water <-
  whale_df_water %>%
  mutate(loc_qual_num = case_when(
    loc_qual == 3 ~ 3,
    loc_qual == 2 ~ 2,
    loc_qual == 1 ~ 1,
    loc_qual == 0 ~ 0,
    loc_qual == "A" ~ -1,
    loc_qual == "B" ~ -2,
    loc_qual == "Z" ~ -3,
    TRUE ~ NA
  ))

## When two data points have the same tag and date_time, keep best quality
whale_df_water <- whale_df_water %>%
  group_by(tag, date_time) %>%
  slice_max(order_by = loc_qual_num, n = 1, with_ties = FALSE) %>%
  ungroup()


##### Filter by speed (argosfilter)
tbl_locs <- whale_df_water %>%
  dplyr::arrange(id, date_time) %>%
  dplyr::group_by(tag) %>%
  tidyr::nest()

tbl_locs <- tbl_locs %>%
  dplyr::mutate(filtered = furrr::future_map(data, ~ argosfilter::sdafilter(
    lat = .x$lat1,
    lon = .x$lon1,
    dtime = .x$date_time,
    lc = .x$loc_qual,
    vmax = speed_cutoff_mps
  ))) %>%
  tidyr::unnest(cols = c(data, filtered)) %>%
  dplyr::filter(filtered %in% c("not", "end_location")) %>%
  dplyr::select(-filtered) %>%
  dplyr::arrange(tag, date_time)

## Apply argos filter
whale_df_water <-
  whale_df_water %>%
  filter(unique_ID %in% tbl_locs$unique_ID)

## Secondary speed filter: pairwise Haversine distance/time check
## NOTE: This is redundant with argosfilter but included to reproduce the
## exact published dataset. Removing it yields ~1900 additional valid points
## that argosfilter accepted. Future work may consider using argosfilter alone.
tag_years <- distinct(whale_df_water, id, sampling_year)

dist_all <- tibble()
for (i in 1:nrow(tag_years)) {
  tag_i <- tag_years %>% slice(i) %>% pull(id)
  sampling_year_i <- tag_years %>% slice(i) %>% pull(sampling_year)
  
  dist_i <-
    whale_df_water %>%
    filter(id == tag_i,
           sampling_year == sampling_year_i) %>%
    arrange(date_time) %>%
    mutate(
      prev_lon = lag(lon1),
      prev_lat = lag(lat1),
      prev_date_time = lag(date_time),
      distance_m = geosphere::distHaversine(cbind(lon1, lat1),
                                            cbind(prev_lon, prev_lat)),
      duration_s = time_length(date_time - prev_date_time, "second"),
      speed = distance_m / duration_s,
    ) %>%
    dplyr::select(-c(prev_lon, prev_lat, prev_date_time))
  
  dist_all <- rbind(dist_all, dist_i)
}

## Apply speed filter
whale_df_water <-
  dist_all %>%
  filter(speed <= speed_cutoff_mps)

## Save whale_df_water
saveRDS(whale_df_water, "data/working/whale_df_water.rds")
message("Saved: data/working/whale_df_water.rds")


################################################################################
##### FIGURE 1: Study Area Bathymetry Map

## Get bathymetry data (use cache if available to avoid NOAA download)
if (file.exists("data/working/bathymetry_f.rds")) {
  message("Loading cached bathymetry from data/working/bathymetry_f.rds")
  bathymetry_f <- readRDS("data/working/bathymetry_f.rds")
} else {
  bathymetry <- getNOAA.bathy(
    lon1 = min(whale_df_water$lon1) - 0.5,
    lon2 = max(whale_df_water$lon1) + 0.5,
    lat1 = min(whale_df_water$lat1) - 0.5,
    lat2 = max(whale_df_water$lat1) + 0.5,
    resolution = 1
  )

  bathymetry_f <- fortify.bathy(bathymetry)
  bathymetry_f <- st_as_sf(bathymetry_f, coords = c("x", "y"), remove = FALSE, crs = 4326)
  saveRDS(bathymetry_f, "data/working/bathymetry_f.rds")
}

## Set map limits
lons <- c(min(whale_df_water$lon1) - 0.5, max(whale_df_water$lon1) + 0.7)
lats <- c(min(whale_df_water$lat1) - 0.5, max(whale_df_water$lat1) + 0.2)

## Define breaks and colors
contour_breaks <- c(10000, 0, -20, -100, -200, -300, -400, -2000)
break_colors <- c("#bd9c68", "#d8e6ff", "#87b3ff", "#4789fe", "#005dff", "#0141b2", "#02276a")
custom_labels <- c(">0", "0 to -20", "-20 to -100", "-100 to -200", "-200 to -300", "-300 to -400", "<-400")

cities <- tibble(city = "Pangnirtung", lon = -65.71101, lat = 66.14701) 
cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326)

## Main bathymetry map (Figure 1 main panel)
world_base <-
  ggplot() +
  geom_path(color = 'grey', alpha = 0.5) +
  geom_contour_filled(
    data = bathymetry_f,
    aes(x = x, y = y, z = z),
    breaks = contour_breaks
  ) +
  scale_fill_manual(values = break_colors, labels = custom_labels) +
  geom_sf(data = cities_sf, color = "black", size = 3) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city), 
            color = "black", size = 6, nudge_x = 0.02, nudge_y = 0.1) +
  coord_sf(xlim = lons, ylim = lats, expand = FALSE, crs = 4326) +
  ylab("Latitude (degrees)") +
  xlab("Longitude (degrees)") +
  gg_theme +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) 

ggsave("outputs/bathymetry_map.png",
       width = 12, height = 10, bg = "white")
message("Saved: outputs/bathymetry_map.png (Figure 1 main panel)")

## Inset context map (Figure 1 inset)
larger_bbox <- c(xmin = -100, xmax = -30, ymin = 50, ymax = 80)
smaller_bbox <- c(xmin = -68.313, xmax = -62.622, ymin = 64.121, ymax = 66.819)

world_map_plot <- 
  ggplot(data = world) +
  geom_sf(color = "#544123", fill = "#bd9c68") +
  coord_sf(xlim = c(larger_bbox["xmin"], larger_bbox["xmax"]),
           ylim = c(larger_bbox["ymin"], larger_bbox["ymax"]),
           expand = FALSE) +
  geom_rect(aes(xmin = smaller_bbox["xmin"], xmax = smaller_bbox["xmax"],
                ymin = smaller_bbox["ymin"], ymax = smaller_bbox["ymax"]),
            fill = NA, color = "white", linetype = "solid", linewidth = 1.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#87b3ff"))

ggsave("outputs/bathymetry_map_outer.png",
       width = 12, height = 10, bg = "white")

## Composite Figure 1: overlay inset on main map
figure1 <- cowplot::ggdraw() +
  cowplot::draw_plot(world_base) +
  cowplot::draw_plot(world_map_plot, x = 0.62, y = 0.65, width = 0.35, height = 0.33)

ggsave("outputs/figure1_study_area.png", plot = figure1,
       width = 12, height = 10, dpi = 300, bg = "white")
message("Saved: outputs/figure1_study_area.png (Figure 1 composite)")


################################################################################
##### TABLE 1: Tag Deployment Metadata + Combined Dataset

## Load metadata
whale_meta <-
  read_excel("data/raw/CS tag dates and whale info.xlsx",
             sheet = "Verified") %>%
  mutate(tag_tranmission_start = lubridate::ymd(tag_tranmission_start)) %>%
  mutate(tag_tranmission_end = lubridate::ymd(tag_tranmission_end))

## Combine location and meta data
whale_df <- 
  full_join(whale_df_water, whale_meta,
            by = c("tag", "sampling_year" = "year"))

##### DIVE DATA

## Load dive depth data (6-hour intervals; adjust timestamp by -3 hours)
whale_dive_depth <-
  read_excel("data/raw/divedepths_all.xlsx",
             sheet = "divedepths") %>%
  mutate(date_time_dive_orig = ymd_hms(date_time)) %>%
  mutate(date_time = ymd_hms(date_time) - hours(3)) %>%
  mutate(dive_id = seq(1:nrow(.)))

## Create sampling year
whale_dive_depth <-
  whale_dive_depth %>%
  mutate(yday = yday(date_time),
         year = year(date_time)) %>%
  mutate(
    sampling_year = case_when(yday < 155 ~ year - 1,
                              yday >= 155 ~ year),
    sampling_day = case_when(yday < 155 ~ yday + 365,
                             yday >= 155 ~ yday)
  )

## Create unique ID
whale_dive_depth <-
  whale_dive_depth %>%
  mutate(id = paste0(tag, "_", sampling_year), .before = tag)

## Join location and dive data by nearest date (using data.table rolling join)
whale_dive_depth <- as.data.table(whale_dive_depth)
whale_df <- as.data.table(whale_df)

whale_df_dives <- whale_df[whale_dive_depth, on = .(id, date_time), roll = "nearest"]

## Get location data that did not match dive data and combine
whale_df_locs <-
  whale_df %>% 
  filter(!unique_ID %in% whale_df_dives$unique_ID)

whale_df_all <-
  bind_rows(whale_df_locs, whale_df_dives) %>%
  as_tibble() %>%
  filter(!is.na(sampling_year))

saveRDS(whale_df_all, "data/working/whale_df_all.rds")
message("Saved: data/working/whale_df_all.rds")

## Convert to long format for dive analysis
whale_df_all_long <-
  whale_df_all %>%
  pivot_longer(
    cols = starts_with("divedepth_"),
    names_to = "dive_category",
    values_to = "dives"
  )

saveRDS(whale_df_all_long, "data/working/whale_df_all_long.rds")
message("Saved: data/working/whale_df_all_long.rds")

message("\n=== Table 1 data available in whale_meta ===")
message("Columns: tag, sex_genetic, body_length, tag_tranmission_start, tag_tranmission_end")
print(whale_meta %>% select(any_of(c("tag", "year", "sex_genetic", "body_length", 
                                      "tag_tranmission_start", "tag_tranmission_end"))))
