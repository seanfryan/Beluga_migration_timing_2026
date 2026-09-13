# =====================================================
# 04_transect_projection.R
# Project whale positions onto a diagonal transect through Cumberland Sound
# Produces: Figure 3 — Position along transect vs DOY with ice overlay
# =====================================================

rm(list = ls())

library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(sf)
library(maptiles)
library(tidyterra)

source("./R/utils.R")

## Load whale data
whale_df_all <- readRDS("data/working/whale_df_all.rds")

## Drop geometry column if present (we only need lat1/lon1 columns)
if (inherits(whale_df_all, "sf")) {
  whale_df_all <- sf::st_drop_geometry(whale_df_all)
}
## Also remove any leftover sfc columns
geom_cols <- names(whale_df_all)[vapply(whale_df_all, inherits, logical(1), "sfc")]
if (length(geom_cols) > 0) {
  whale_df_all <- whale_df_all[, !names(whale_df_all) %in% geom_cols, drop = FALSE]
}

## Define the top and bottom points of the transect axis
## NOTE: Projection uses Euclidean lat/lon geometry (treats 1 deg lat = 1 deg lon).
## At 65 deg N, 1 deg lon ~ 47 km vs 1 deg lat ~ 111 km. This is consistent with
## the published figure but compresses the longitude dimension.
top_point <- c(lat = 66.6, lon = -67.75)
bottom_point <- c(lat = 64.25, lon = -64.25)

## Create the axis vector
axis_vector <- c(top_point["lat"] - bottom_point["lat"], top_point["lon"] - bottom_point["lon"])
axis_length <- sqrt(sum(axis_vector^2))

## Normalize the axis vector
axis_vector_normalized <- axis_vector / axis_length

## Translate points to the bottom point as origin
whale_df_all$lat_trans <- whale_df_all$lat1 - bottom_point["lat"]
whale_df_all$lon_trans <- whale_df_all$lon1 - bottom_point["lon"]

## Project the points onto the axis vector
whale_df_all$projection <- whale_df_all$lat_trans * axis_vector_normalized[1] + whale_df_all$lon_trans * axis_vector_normalized[2]

## Scale the projections to 0-100
min_proj <- 0
max_proj <- axis_length

whale_df_all$position <- 100 * (whale_df_all$projection - min_proj) / (max_proj - min_proj)

## Sort by sampling day
whale_df_all <- whale_df_all %>% arrange(sampling_day)

## Create datasets by year
datasets <- list(
  whale_df_all %>% filter(year == 1998),
  whale_df_all %>% filter(year == 1999),
  whale_df_all %>% filter(year == 2006),
  whale_df_all %>% filter(year == 2007),
  whale_df_all %>% filter(year == 2008)
)

## Define axis parameters
y_breaks <- seq(190, 510, by = 40)


################################################################################
## Load percent ice cover summary stats

sum_results <- readRDS("data/working/ice_sum_results.rds")
ice_data_plotting <- sum_results
ice_data_plotting <- ice_data_plotting %>% arrange(sampling_day)
ice_data_plotting$prnct_cover_10 <- as.numeric(as.character(ice_data_plotting$prnct_cover_10))

## Create ice datasets by year
ice_datasets <- list(
  ice_data_plotting %>% filter(sampling_year == 1998),
  ice_data_plotting %>% filter(sampling_year == 1999),
  ice_data_plotting %>% filter(sampling_year == 2006),
  ice_data_plotting %>% filter(sampling_year == 2007),
  ice_data_plotting %>% filter(sampling_year == 2008)
)


################################################################################
## Figure 3: Transect position vs DOY with ice overlay

## Find closest position for the boundary lines
unique_mappings <- imap_dfr(datasets, ~ .x %>%
                              select(lat1, position) %>%
                              mutate(dataset_id = .y)
) %>%
  select(-dataset_id) %>%
  distinct()

get_closest_pos <- function(target_lat, mapping_df) {
  mapping_df %>%
    mutate(diff = abs(lat1 - target_lat)) %>%
    filter(diff == min(diff)) %>%
    slice(1) %>% 
    pull(position)
}

b1_pos <- get_closest_pos(66.5, unique_mappings)
b2_pos <- get_closest_pos(65.75, unique_mappings)

## Define colors
custom_colors <- c("F" = "#CC79A7", "M" = "#009E73")

## Generate plots
plots <- lapply(1:5, function(i) {
  
  ggplot() +
    coord_flip() +
    ylab("Day of year") + 
    
    # Whale Data Points (Uses COLOR)
    geom_point(data = datasets[[i]], 
               aes(x = position, y = sampling_day, color = sex_genetic), 
               alpha = 0.4) +
    
    # Ice Data Points (Uses FILL)
    geom_point(data = ice_datasets[[i]], 
               aes(x = prnct_cover_10, y = sampling_day, fill = prnct_cover_10), 
               shape = 21, color = "transparent", size = 3) +
    
    # Boundary Lines & Labels
    geom_vline(xintercept = b1_pos, color = "black", linewidth = 0.8, linetype = "dashed") +
    annotate("text", x = b1_pos, y = 510, label = "Boundary 1 (66.5)", 
             vjust = -0.5, hjust = 1, size = 3.5, color = "black") +
    
    geom_vline(xintercept = b2_pos, color = "black", linewidth = 0.8, linetype = "dashed") +
    annotate("text", x = b2_pos, y = 510, label = "Boundary 2 (65.75)", 
             vjust = 1.5, hjust = 1, size = 3.5, color = "black") +
    
    # Separate Scales
    scale_color_manual(values = custom_colors, name = "Sex") +
    scale_fill_gradient(name = "Ice coverage", low = "#154365", high = "#b7e0ff", 
                        limits = c(0, 100), na.value = NA) +
    
    # Axes & Theme
    scale_y_continuous(breaks = y_breaks, limits = c(190, 510)) +
    scale_x_continuous(
      name = "Position along transect (0-100)",
      limits = c(0, 100),
      sec.axis = sec_axis(~ ., name = "Percent ice coverage (cat 10)")
    ) +
    
    guides(alpha = "none") + 
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "white", color = "black")
    ) +
    ggtitle(paste0(unique(ice_datasets[[i]]$sampling_year)))
})

## Individual subplot adjustments
plots[[1]] <- plots[[1]] + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y.right = element_blank())
plots[[2]] <- plots[[2]] + theme(axis.title.y.left = element_blank(), axis.title.x = element_blank(), legend.position = "none")
plots[[3]] <- plots[[3]] + theme(legend.position = "none", axis.title.y.right = element_blank())
plots[[4]] <- plots[[4]] + theme(axis.title.y.left = element_blank(), legend.position = "none")
plots[[5]] <- plots[[5]] + theme(axis.title.y.left = element_blank())


################################################################################
## Map panel showing transect line through Cumberland Sound (terrain basemap)

bbox_sf <- st_as_sfc(st_bbox(c(xmin = -69.5, ymin = 64.0, xmax = -63.0, ymax = 67.5), crs = 4326))
terrain_tiles <- get_tiles(bbox_sf, provider = "OpenTopoMap", zoom = 7, crop = TRUE)

map_panel <-
  ggplot() +
  geom_spatraster_rgb(data = terrain_tiles) +
  geom_point(
    data = bind_rows(datasets),
    aes(x = lon1, y = lat1),
    color = "blue", size = 0.3, alpha = 0.15
  ) +
  geom_segment(
    aes(x = bottom_point["lon"], y = bottom_point["lat"],
        xend = top_point["lon"], yend = top_point["lat"]),
    color = "red", linewidth = 1.5, alpha = 0.8
  ) +
  geom_hline(yintercept = 66.5, color = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = -63.5, y = 66.5, label = "Boundary 1 (66.5)",
           vjust = -0.5, hjust = 1, size = 3.5, color = "black") +
  geom_hline(yintercept = 65.75, color = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = -63.5, y = 65.75, label = "Boundary 2 (65.75)",
           vjust = 1.5, hjust = 1, size = 3.5, color = "black") +
  coord_sf(crs = 4326, xlim = c(-69.5, -63.0), ylim = c(64.0, 67.5), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10)
  ) +
  ggtitle("Transect")


## Compose & Save (6 panels: 5 years + map)
top_row <- plots[[1]] + plots[[2]] + map_panel
bottom_row <- plots[[3]] + plots[[4]] + plots[[5]]

custom_layout <- top_row / bottom_row + plot_layout(widths = c(1, 1, 1), heights = c(1, 1))

ggsave("outputs/scatter_whale_location_over_time_transect_with_ice.png", 
       plot = custom_layout, width = 12, height = 8, bg = "white")
message("Saved: outputs/scatter_whale_location_over_time_transect_with_ice.png (Figure 3)")
