# =====================================================
# 02_ice_cover_timeseries.R
# Calculate percent ice cover over time within Cumberland Sound bounding box
# Produces: data/working/ice_sum_results.rds
# Produces: outputs/change_ice_cover_over_time_*.png
# =====================================================

rm(list = ls())

library(tidyverse)

source("./R/utils.R")

## If ice_sum_results.rds already exists, skip computation and go to plotting
if (file.exists("data/working/ice_sum_results.rds")) {
  message("Loading pre-computed ice_sum_results.rds (skipping spatial computation)")
  sum_results <- readRDS("data/working/ice_sum_results.rds")
} else {

library(sf)
library(rnaturalearth)

## Bounding box for Cumberland Sound
lons <- c(-68, -64)
lats <- c(64, 67)

## Create bounding box
bbox <- st_bbox(c(xmin = lons[1], xmax = lons[2], ymin = lats[1], ymax = lats[2]), crs = st_crs(4326))
bbox_sfc <- st_as_sfc(bbox)

## Load ice data
ice_data <- readRDS("data/working/ice_data.rds") %>%
  mutate(N_CT = as.numeric(N_CT))

## Load or create ocean data
if (file.exists("data/working/ocean.rds")) {
  ocean <- readRDS("data/working/ocean.rds")
} else {
  message("Downloading ocean data from Natural Earth...")
  ocean <- ne_download(scale = "large", type = "ocean", category = "physical",
                       returnclass = "sf")
  saveRDS(ocean, "data/working/ocean.rds")
  message("Created and saved: data/working/ocean.rds")
}

## Fix invalid geometries
if (any(!st_is_valid(ocean))) {
  ocean <- st_make_valid(ocean)
}

## Transform bbox to ice_data CRS
bbox_sf <- st_sfc(bbox_sfc, crs = 4326)
bbox_transformed <- st_transform(bbox_sf, st_crs(ice_data))

## Filter ice to bounded box
ice_bound <- st_intersection(ice_data, bbox_transformed)

## Calculate ocean area within bounding box
bbox_sf_ocean <- st_transform(bbox_sf, st_crs(ocean))
ocean_sf <- st_difference(bbox_sf_ocean, ocean)
area_ocean <- st_area(ocean_sf)

## Load pre-computed percent cover results (computation loop is intensive)
## The commented-out loop below shows the calculation logic
if (file.exists("data/working/ice_data_prnct_cover.rds")) {
  results <- readRDS("data/working/ice_data_prnct_cover.rds")
} else {
  ## Create list of dates to process
  dates <- unique(data.table::data.table(ice_bound)[, .(sampling_day, sampling_year)]) %>% as.data.frame()
  
  results <- tibble()
  
  for (i in 1:nrow(dates)) {
    ice_bound_i <-
      ice_bound %>%
      filter(sampling_year == as.numeric(dates[i, "sampling_year"]),
             sampling_day == as.numeric(dates[i, "sampling_day"]))
    
    area_ice_cat10 <- sum(st_area(ice_bound_i %>% filter(as.numeric(N_CT) == 10)))
    area_ice_cat5above <- sum(st_area(ice_bound_i %>% 
                                    filter(as.numeric(N_CT) >= 5)))
    
    results_i <- tibble(sampling_year = as.numeric(dates[i, "sampling_year"]),
                        sampling_day = as.numeric(dates[i, "sampling_day"]),
                        area_ice_cat10 = as.numeric(area_ice_cat10),
                        area_ice_cat5above = as.numeric(area_ice_cat5above))
    
    results <- bind_rows(results, results_i)
  }
  
  saveRDS(results, "data/working/ice_data_prnct_cover.rds")
}

## Calculate percent coverage
sum_results <-
  results %>%
  mutate(prnct_cover_10 = (area_ice_cat10 / as.numeric(area_ocean)) * 100) %>%
  mutate(prnct_cover_5above = (area_ice_cat5above / as.numeric(area_ocean)) * 100)

saveRDS(sum_results, "data/working/ice_sum_results.rds")
message("Saved: data/working/ice_sum_results.rds")

} # end else (computation block)


################################################################################
##### Plots: Percent ice cover over time

## Define year colors for whale-sampling years
year_colors <- c(
  "1998" = "#006bbb",
  "1999" = "#003e6d",
  "2006" = "#e80000",
  "2007" = "#a10000",
  "2008" = "#750000"
)

## Create color gradient for all other years
blue_to_red <- colorRampPalette(c("blue", "red"))(31)
other_years <- setdiff(unique(sum_results$sampling_year), names(year_colors))
other_year_colors <- setNames(blue_to_red[1:length(other_years)], other_years)
all_year_colors <- c(other_year_colors, year_colors)

## Order factor levels chronologically
ordered_years <- sort(c(as.character(other_years), names(year_colors)))
sum_results <- sum_results %>%
  mutate(sampling_year = factor(sampling_year, levels = ordered_years))

## Cat 10 - all years
prcnt_cov_cat10 <- sum_results %>%
  ggplot(aes(x = sampling_day, y = prnct_cover_10, color = sampling_year)) +
  scale_color_manual(values = all_year_colors, name = "Year") +
  xlab("Day of year") +
  ylab("Percent ice cover") +
  geom_line(aes(group = sampling_year, 
                alpha = ifelse(sampling_year %in% names(year_colors), 1, 0.2),
                linewidth = ifelse(sampling_year %in% names(year_colors), 1, 0.5))) +
  ylim(0, 100) +
  scale_alpha_identity() +
  scale_linewidth_identity() +
  gg_theme +
  ggtitle("Percent ice coverage (cat 10)")
ggsave("outputs/change_ice_cover_over_time_cat10.png",
       width = 8, height = 6, bg = "white")

## Cat 10 - sampling years only
prcnt_cov_cat10_sampling <- sum_results %>%
  filter(sampling_year %in% names(year_colors)) %>%
  ggplot(aes(x = sampling_day, y = prnct_cover_10, color = sampling_year)) +
  scale_color_manual(values = all_year_colors, name = "Year") +
  xlab("Day of year") +
  ylab("Percent ice cover") +
  geom_line(aes(group = sampling_year), linewidth = 1.5, alpha = 0.7) +
  ylim(0, 100) +
  gg_theme +
  ggtitle("Percent ice coverage (cat 10)")
ggsave("outputs/change_ice_cover_over_time_cat10_samplingyears.png",
       width = 8, height = 6, bg = "white")

## Cat >=5 - all years
prcnt_cov_cat5above <- sum_results %>%
  ggplot(aes(x = sampling_day, y = prnct_cover_5above, color = sampling_year)) +
  scale_color_manual(values = all_year_colors, name = "Year") +
  xlab("Day of year") +
  ylab("Percent ice cover") +
  geom_line(aes(group = sampling_year,
                alpha = ifelse(sampling_year %in% names(year_colors), 1, 0.2),
                linewidth = ifelse(sampling_year %in% names(year_colors), 1, 0.5))) +
  scale_alpha_identity() +
  scale_linewidth_identity() +
  ylim(0, 100) +
  gg_theme +
  ggtitle("Percent ice coverage (cat >=5)")
ggsave("outputs/change_ice_cover_over_time_cat5above.png",
       width = 8, height = 6, bg = "white")

## Cat >=5 - sampling years only
prcnt_cov_cat5above_sampling <- sum_results %>%
  filter(sampling_year %in% names(year_colors)) %>%
  ggplot(aes(x = sampling_day, y = prnct_cover_5above, color = sampling_year)) +
  scale_color_manual(values = all_year_colors, name = "Year") +
  xlab("Day of year") +
  ylab("Percent ice cover") +
  geom_line(aes(group = sampling_year), linewidth = 1.5, alpha = 0.7) +
  ylim(0, 100) +
  gg_theme +
  ggtitle("Percent ice coverage (cat >=5)")
ggsave("outputs/change_ice_cover_over_time_cat5above_samplingyears.png",
       width = 8, height = 6, bg = "white")

message("Saved ice cover plots to outputs/")
