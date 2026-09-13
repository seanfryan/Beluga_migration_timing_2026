# =====================================================
# 01_cache_daily_area_by_band.R
# Precompute daily ice area by (band, year, yday, N_CT) and cache to disk
# =====================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(lubridate)
  library(readr)
  library(tibble)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

# ---- Config ----
ice_data <- readRDS("data/working/ice_data.rds") %>%
  mutate(N_CT = as.numeric(N_CT)) %>%
  st_transform(4326)

lons <- c(-68, -64)

lat_top <- 67
lat_bot <- 64
band_step <- 0.5
include_full_band <- TRUE

lat_seq <- seq(lat_top, lat_bot, by = -band_step)
if (tail(lat_seq, 1) != lat_bot) lat_seq <- c(lat_seq, lat_bot)

bands_df <- tibble(
  lat_max = lat_seq[-length(lat_seq)],
  lat_min = lat_seq[-1]
) %>%
  mutate(band_id = paste0("band_", lat_max, "_", lat_min))

if (isTRUE(include_full_band)) {
  bands_df <- bind_rows(
    bands_df,
    tibble(
      lat_max = lat_top,
      lat_min = lat_bot,
      band_id = paste0("band_", lat_top, "_", lat_bot)
    )
  )
}

parent_root <- ensure_dir(file.path("outputs", "freeze_0p5"))
master_dir  <- ensure_dir(file.path(parent_root, "master"))
cache_file  <- file.path(master_dir, "cache_daily_area_bands.rds")

# ---- Build cache ----
all_daily <- list()

for (b in seq_len(nrow(bands_df))) {
  lat_top_curr <- bands_df$lat_max[b]
  lat_bot_curr <- bands_df$lat_min[b]
  band_id      <- bands_df$band_id[b]
  
  message("Caching band: ", band_id)
  
  bbox <- st_bbox(
    c(xmin = lons[1], xmax = lons[2], ymin = lat_bot_curr, ymax = lat_top_curr),
    crs = 4326
  )
  bbox_sfc <- st_as_sfc(bbox)
  
  ice_subset <- ice_data[st_intersects(ice_data, bbox_sfc, sparse = FALSE), ]
  if (nrow(ice_subset) == 0) {
    message("  -> no data; skipping")
    next
  }
  
  # For caching daily area, you can either:
  # A) use st_intersection (accurate, slower), or
  # B) st_crop for speed (approx for edges)
  ice_bound <- suppressWarnings(st_intersection(ice_subset, bbox_sfc))
  
  daily_area <- ice_bound %>%
    st_drop_geometry() %>%
    mutate(
      date_obj = as.Date(date),
      year_num = year(date_obj),
      yday     = yday(date_obj)
    ) %>%
    group_by(year_num, yday, N_CT) %>%
    summarise(total_area = sum(AREA, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      band_id = band_id,
      lat_min = lat_bot_curr,
      lat_max = lat_top_curr
    )
  
  all_daily[[band_id]] <- daily_area
}

cache_df <- bind_rows(all_daily)

saveRDS(list(bands_df = bands_df, daily_area = cache_df), cache_file)
message("Saved cache: ", normalizePath(cache_file, winslash = "/"))



################################################################################
## Create "stacked" plot of ice coverage by day of year
  
# 1. Define the directory structure (this should be at the top of your script)
parent_root <- file.path("outputs", "freeze_0p5")
master_dir  <- file.path(parent_root, "master")

# 2. Construct the full path to the file
cache_file <- file.path(master_dir, "cache_daily_area_bands.rds")

# 3. Read the RDS file using the full path
cache <- readRDS(cache_file)

# 4. IMPORTANT: Extract the daily_area dataframe from the loaded list
daily_area <- cache$daily_area

# Load necessary libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr) # Used for file naming
})

# --- Helper function to ensure directory exists ---
ensure_dir <- function(path) {
  if (!dir.exists(path))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

# --- Step 1: Configuration & Data Loading ---
parent_root <- file.path("outputs", "freeze_0p5")
plot_output_dir <- ensure_dir(file.path(parent_root, "band_area_plots"))

# Plot sizing preferences
PLOT_BASE_SIZE    <- 14
AXIS_TITLE_SIZE   <- 14
AXIS_TEXT_SIZE    <- 16
STRIP_TEXT_SIZE   <- 18
PLOT_TITLE_SIZE   <- 16


# --- Step 2: Loop Through Each Band and Generate a Plot ---
all_bands <- unique(daily_area$band_id)
message("Generating plots for ", length(all_bands), " bands...")

for (band in all_bands) {
  band_data <- daily_area %>%
    filter(band_id == !!band) %>%
    mutate(N_CT_f = factor(N_CT, levels = sort(unique(N_CT))))
  
  if (nrow(band_data) == 0) {
    message("  -> Skipping ", band, ": No data.")
    next
  }
  
  band_dir <- ensure_dir(file.path(plot_output_dir, band))
  
  p_area <- ggplot(data = band_data,
                   aes(
                     x = yday,
                     y = total_area / 1e11,  # Scale the data down by 10^11
                     color = year_num,
                     group = year_num
                   )) +
    geom_line(alpha = 0.8,
              linewidth = 0.7,
              na.rm = TRUE) +
    ylab(expression("Total area x " * 10^11 ~ (m^2))) + 
    xlab("Day of year") +
    # Use "fixed" to only show labels on the outer edges of the 7-column grid
    facet_wrap( ~ N_CT_f, ncol = 4, scales = "fixed") + 
    
    scale_color_gradientn(colors = c("blue", "cyan", "orange", "red")) +
    scale_y_continuous() + 
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    theme(
      legend.position = "none",
      axis.title      = element_text(size = 24),
      axis.text       = element_text(size = 20),
      # Rotate X axis labels
      axis.text.x     = element_text(angle = 45, hjust = 1),
      # Rotate Y axis labels
      axis.text.y     = element_text(angle = 45, vjust = 1),
      strip.text      = element_text(
        size = STRIP_TEXT_SIZE,
        face = "bold",
        margin = margin(t = 5, b = 5)
      ),
      panel.spacing   = unit(1, "lines")
    )
  
  output_filename <- file.path(band_dir, paste0("area_vs_doy_by_NCT_4col_", band, ".png"))
  
  # num_facets <- nlevels(band_data$N_CT_f)
  # num_rows <- ceiling(num_facets / 2)
  # plot_height <- 3.5 * num_rows + 2
  
  ggsave(
    filename = output_filename,
    plot = p_area,
    width = 12,
    # Increased width slightly for the 2-column layout
    height = 8, #plot_height,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
  
  message("  -> Saved plot: ", output_filename)
}

message("Done.")
  

################################################################################
# =====================================================
# 02_run_scenarios_from_cache.R
# Compute freeze stats for scenarios using cached daily area and APPEND outputs
# =====================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

fmt_num <- function(x) sub("\\.", "p", format(x, trim = TRUE, scientific = FALSE))
scenario_tag <- function(start_day, pct, nct) paste0("SD", fmt_num(start_day), "_PCT", fmt_num(pct), "_NCT", fmt_num(nct))

# ---- Config ----
parent_root <- ensure_dir(file.path("outputs", "freeze_0p5"))
master_dir  <- ensure_dir(file.path(parent_root, "master"))
cache_file  <- file.path(master_dir, "cache_daily_area_bands.rds")

cache <- readRDS(cache_file)
bands_df   <- cache$bands_df
daily_area <- cache$daily_area

# Scenarios
scenarios <- expand.grid(
  look_start_day = c(250),
  pct_threshold  = c(0.05),
  target_nct     = c(9.7),
  stringsAsFactors = FALSE
)

# Master files to append into
master_freeze_csv <- file.path(master_dir, "scenario_results_master.csv")
master_daily_csv  <- file.path(master_dir, "scenario_daily_master.csv") # optional; can be large

append_csv_dedup <- function(new_df, out_csv, key_cols) {
  if (file.exists(out_csv)) {
    old <- readr::read_csv(out_csv, show_col_types = FALSE)
    combined <- bind_rows(old, new_df) %>%
      distinct(across(all_of(key_cols)), .keep_all = TRUE)
  } else {
    combined <- new_df
  }
  write_csv(combined, out_csv)
  invisible(combined)
}

# Run scenarios
all_new_freeze <- list()
all_new_daily  <- list()

for (j in seq_len(nrow(scenarios))) {
  
  curr_start <- scenarios$look_start_day[j]
  curr_pct   <- scenarios$pct_threshold[j]
  curr_nct   <- scenarios$target_nct[j]
  
  scen_tag <- scenario_tag(curr_start, curr_pct, curr_nct)
  scen_dir <- ensure_dir(file.path(parent_root, paste0("scenario_", scen_tag)))
  
  message("Running scenario: ", scen_tag)
  
  # Loop bands (using cached daily_area)
  for (b in seq_len(nrow(bands_df))) {
    
    band_id <- bands_df$band_id[b]
    lat_min <- bands_df$lat_min[b]
    lat_max <- bands_df$lat_max[b]
    
    band_dir <- ensure_dir(file.path(scen_dir, band_id))
    
    da_band <- daily_area %>% filter(band_id == !!band_id)
    if (nrow(da_band) == 0) next
    
    # Baseline: GLOBAL MAX over all years and ydays for this band & N_CT threshold
    baseline_area <- da_band %>%
      filter(N_CT >= curr_nct) %>%
      group_by(year_num, yday) %>%
      summarise(sum_area_N_CT = sum(total_area, na.rm = TRUE), .groups = "drop") %>%
      summarise(max_base = max(sum_area_N_CT, na.rm = TRUE), .groups = "drop") %>%
      pull(max_base)
    
    if (!is.finite(baseline_area) || is.na(baseline_area) || baseline_area <= 0) next
    
    # Daily series with percent-of-baseline and freeze day markers
    year_daily <- da_band %>%
      filter(N_CT >= curr_nct) %>%
      group_by(year_num, yday) %>%
      summarise(sum_area_N_CT = sum(total_area, na.rm = TRUE), .groups = "drop") %>%
      group_by(year_num) %>%
      complete(yday = curr_start:366, fill = list(sum_area_N_CT = 0)) %>%
      arrange(year_num, yday) %>%
      mutate(prcnt_of_baseline = (sum_area_N_CT / baseline_area) * 100) %>%
      group_modify(~ {
        freeze_idx <- which(.x$yday >= curr_start & .x$sum_area_N_CT >= (baseline_area * curr_pct))
        d_freeze   <- if (length(freeze_idx) > 0) .x$yday[min(freeze_idx)] else NA_real_
        
        .x %>%
          mutate(
            doy_freeze    = d_freeze,
            is_freeze_day = !is.na(d_freeze) & (yday == d_freeze)
          )
      }) %>%
      ungroup() %>%
      mutate(
        lat_min        = lat_min,
        lat_max        = lat_max,
        band_id        = band_id,
        look_start_day = curr_start,
        pct_threshold  = curr_pct,
        target_nct     = curr_nct,
        baseline_used  = baseline_area,
        scenario_tag   = scen_tag
      )
    
    # Per-year summary for trends (one row per year)
    year_freeze <- year_daily %>%
      group_by(year_num) %>%
      summarise(
        doy_freeze = unique(doy_freeze)[1],
        baseline_used = unique(baseline_used)[1],
        .groups = "drop"
      ) %>%
      mutate(
        lat_min        = lat_min,
        lat_max        = lat_max,
        band_id        = band_id,
        look_start_day = curr_start,
        pct_threshold  = curr_pct,
        target_nct     = curr_nct,
        scenario_tag   = scen_tag
      )
    
    # Save band-level CSVs for this run (optional but handy)
    write_csv(year_freeze, file.path(band_dir, paste0("year_freeze_", band_id, "_", scen_tag, ".csv")))
    write_csv(year_daily,  file.path(band_dir, paste0("year_daily_",  band_id, "_", scen_tag, ".csv")))
    
    all_new_freeze[[paste(band_id, scen_tag, sep="__")]] <- year_freeze
    all_new_daily[[paste(band_id, scen_tag, sep="__")]]  <- year_daily
  }
}

new_freeze_df <- bind_rows(all_new_freeze)
new_daily_df  <- bind_rows(all_new_daily)

# Append to master tables (dedup by keys)
if (nrow(new_freeze_df) > 0) {
  append_csv_dedup(
    new_freeze_df,
    master_freeze_csv,
    key_cols = c("band_id","year_num","look_start_day","pct_threshold","target_nct")
  )
  message("Appended master freeze results: ", normalizePath(master_freeze_csv, winslash = "/"))
}

# daily master can be very large—only enable if needed
if (nrow(new_daily_df) > 0) {
  append_csv_dedup(
    new_daily_df,
    master_daily_csv,
    key_cols = c("band_id","year_num","yday","look_start_day","pct_threshold","target_nct")
  )
  message("Appended master daily results: ", normalizePath(master_daily_csv, winslash = "/"))
}

################################################################################

# =====================================================
# 03_make_plots_from_master.R
# Create plots from master CSV outputs (no need to recompute stats)
# =====================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
})

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

fmt_num <- function(x) sub("\\.", "p", format(x, trim = TRUE, scientific = FALSE))
scenario_tag <- function(start_day, pct, nct) paste0("SD", fmt_num(start_day), "_PCT", fmt_num(pct), "_NCT", fmt_num(nct))

# Plot sizing
PLOT_BASE_SIZE    <- 16
PLOT_TITLE_SIZE   <- 18
PLOT_SUB_SIZE     <- 16
AXIS_TITLE_SIZE   <- 16
AXIS_TEXT_SIZE    <- 14
LEGEND_TITLE_SIZE <- 16
LEGEND_TEXT_SIZE  <- 16

parent_root <- ensure_dir(file.path("outputs", "freeze_0p5"))
master_dir  <- ensure_dir(file.path(parent_root, "master"))

master_freeze_csv <- file.path(master_dir, "scenario_results_master.csv")
master_daily_csv  <- file.path(master_dir, "scenario_daily_master.csv") # optional

freeze_df <- readr::read_csv(master_freeze_csv, show_col_types = FALSE)

# Combined trends plot per scenario
highlight_years <- c(1998, 1999, 2006, 2007, 2008)

scen_rows <- freeze_df %>%
  distinct(look_start_day, pct_threshold, target_nct) %>%
  arrange(look_start_day, pct_threshold, target_nct)

for (k in seq_len(nrow(scen_rows))) {
  s <- scen_rows[k, ]
  scen_tag <- scenario_tag(s$look_start_day, s$pct_threshold, s$target_nct)
  scen_dir <- ensure_dir(file.path(parent_root, paste0("scenario_", scen_tag)))
  
  plot_df <- freeze_df %>%
    dplyr::filter(
      look_start_day == s$look_start_day,
      pct_threshold  == s$pct_threshold,
      target_nct     == s$target_nct
    ) %>%
    dplyr::mutate(
      band_label = {
        raw <- gsub("^band_", "", band_id)
        parts <- strsplit(raw, "_")
        
        vapply(parts, function(p) {
          if (length(p) >= 2) {
            paste0(p[1], "\u00B0N to ", p[2], "\u00B0N")
          } else {
            paste0(raw, "\u00B0N")
          }
        }, character(1))
      },
      band_f = factor(band_label, levels = unique(band_label))
    )
  
  if (nrow(plot_df) == 0) next
  
  # Perform linear regression and correct for multiple testing
  trend_stats <- plot_df %>%
    dplyr::filter(!is.na(doy_freeze)) %>%
    dplyr::group_by(band_id, band_label) %>%
    tidyr::nest() %>%
    dplyr::filter(purrr::map_int(data, nrow) >= 3) %>%
    dplyr::mutate(model = purrr::map(data, ~ lm(doy_freeze ~ year_num, data = .x))) %>%
    dplyr::mutate(tidied = purrr::map(model, broom::tidy)) %>%
    tidyr::unnest(tidied) %>%
    dplyr::filter(term == "year_num") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      p_adj_bh = p.adjust(p.value, method = "BH"),
      is_significant = p_adj_bh < 0.05
    ) %>%
    dplyr::select(
      band_id,
      band_label,
      slope = estimate,
      std_error = std.error,
      statistic,
      p_value = p.value,
      p_adj_bh,
      is_significant
    )
  
  # Save the regression statistics to a CSV file in the scenario's directory
  stats_csv_path <- file.path(scen_dir, paste0("freeze_doy_trends_regression_stats_", scen_tag, ".csv"))
  readr::write_csv(trend_stats, stats_csv_path)
  
  message("  -> Saved regression stats (with correction) to: ", basename(stats_csv_path))
  
  # Keep only bands that are significant after BH correction
  sig_bands <- trend_stats %>%
    dplyr::filter(is_significant) %>%
    dplyr::distinct(band_id)
  
  # Data used only for plotting the regression lines
  plot_df_sig <- plot_df %>%
    dplyr::semi_join(sig_bands, by = "band_id")
  
  p_combined <- ggplot(
    plot_df,
    aes(x = year_num, y = doy_freeze, color = band_f, group = band_f)
  ) +
    geom_vline(xintercept = highlight_years, linetype = "dashed",
               color = "grey60", alpha = 0.8) +
    geom_point(alpha = 0.6, size = 2, na.rm = TRUE) +
    geom_smooth(
      data = plot_df_sig,
      method = "lm",
      se = FALSE,
      linewidth = 1,
      alpha = 1,
      na.rm = TRUE
    ) +
    theme_minimal(base_size = PLOT_BASE_SIZE) +
    scale_color_viridis_d(name = "Latitudinal section", end = 0.85) +
    labs(
      title    = NULL,
      subtitle = NULL,
      x        = "Year",
      y        = "First freeze date (day of year)"
    ) +
    theme(
      plot.title      = element_blank(),
      plot.subtitle   = element_blank(),
      axis.title.x    = element_text(size = AXIS_TITLE_SIZE),
      axis.title.y    = element_text(size = AXIS_TITLE_SIZE),
      axis.text       = element_text(size = AXIS_TEXT_SIZE),
      legend.title    = element_text(size = LEGEND_TITLE_SIZE),
      legend.text     = element_text(size = LEGEND_TEXT_SIZE),
      legend.position = "right"
    )
  
  ggsave(
    file.path(scen_dir, paste0("freeze_doy_firstfreeze_trends_", scen_tag, ".png")),
    p_combined,
    width = 12, height = 7, dpi = 300, bg = "white"
  )
}








# percent-of-baseline points plot (requires daily master)
if (file.exists(master_daily_csv)) {
  daily_df <- readr::read_csv(master_daily_csv, show_col_types = FALSE)
  
  scen_rows2 <- daily_df %>%
    distinct(look_start_day, pct_threshold, target_nct) %>%
    arrange(look_start_day, pct_threshold, target_nct)
  
  for (k in seq_len(nrow(scen_rows2))) {
    s <- scen_rows2[k, ]
    scen_tag <- scenario_tag(s$look_start_day, s$pct_threshold, s$target_nct)
    scen_dir <- ensure_dir(file.path(parent_root, paste0("scenario_", scen_tag)))
    
    d0 <- daily_df %>%
      filter(
        look_start_day == s$look_start_day,
        pct_threshold  == s$pct_threshold,
        target_nct     == s$target_nct
      )
    
    if (nrow(d0) == 0) next
    
    # Make one plot per band to avoid spaghetti
    for (band in unique(d0$band_id)) {
      dd <- d0 %>% filter(band_id == band)
      
      ## randomly sort so newers years don't always mask older years
      dd <- dd %>% dplyr::slice_sample(prop = 1)
      
      band_dir <- ensure_dir(file.path(scen_dir, band))
      
      p_pts <- ggplot(dd, aes(x = yday, y = prcnt_of_baseline, color = year_num)) +
        geom_point(data = dd %>% filter(!is_freeze_day),
                   shape = 1, size = 1.4, alpha = 0.35, stroke = 0.7, na.rm = TRUE) +
        geom_point(data = dd %>% filter(is_freeze_day),
                   aes(fill = year_num),
                   shape = 21, size = 3.0, alpha = 0.95, stroke = 0.9,
                   color = "black", na.rm = TRUE) +
        geom_hline(yintercept = s$pct_threshold * 100, linetype = "dashed", color = "black", linewidth = 0.7) +
        scale_color_gradientn(colors = c("blue","cyan","orange","red"), name = "Year") +
        scale_fill_gradientn(colors = c("blue","cyan","orange","red"), guide = "none") +
        theme_minimal(base_size = PLOT_BASE_SIZE) +
        labs(
          title = paste0("Percent of Baseline by DOY (", band, ")"),
          subtitle = "Open circles = non-freeze; filled = first threshold crossing",
          x = "Day of Year",
          y = "Percent of baseline (%)"
        )
      
      ggsave(file.path(band_dir, paste0("pct_of_baseline_points_FREEZEMARK_", band, "_", scen_tag, ".png")),
             p_pts, width = 11, height = 6.5, dpi = 300, bg = "white")
    }
  }
}

