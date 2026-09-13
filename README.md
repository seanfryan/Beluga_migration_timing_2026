# Beluga_migration_timing_2026

Analyses for Ryan et al. (2026):
"Stable decadal migration timing and temporal variation in dive behavior of an endangered beluga whale population"

## Repository overview

This repository contains the reproducible analysis pipeline for:

- Whale telemetry cleaning and filtering.
- Sea-ice processing and freeze timing analyses.
- Manuscript and supplementary figures/tables that can currently be regenerated from code in this repo.

Top-level locations:

- Source scripts: [R](R)
- Working data products: [data/working](data/working)
- Raw inputs: [data/raw](data/raw)
- Manuscript/supp source files: [ms](ms)
- Recreated outputs: [outputs](outputs)

## Active R scripts

- [R/data_processing.R](R/data_processing.R): whale ingest, filtering, and Figure 1 outputs.
- [R/ice_cover_timeseries.R](R/ice_cover_timeseries.R): ice cover summaries and time-series plots.
- [R/transect_projection.R](R/transect_projection.R): Figure 3 transect panels + map panel.
- [R/fine_scale_ice_analysis.R](R/fine_scale_ice_analysis.R): freeze scenario trends and regression outputs.
- [R/process_ice_data.R](R/process_ice_data.R): optional raw ice shapefile processing to `ice_data.rds`.
- [R/utils.R](R/utils.R): shared plotting helpers/theme.

## Recommended run order

1. [R/data_processing.R](R/data_processing.R)
2. [R/ice_cover_timeseries.R](R/ice_cover_timeseries.R)
3. [R/transect_projection.R](R/transect_projection.R)
4. [R/fine_scale_ice_analysis.R](R/fine_scale_ice_analysis.R)
5. [R/process_ice_data.R](R/process_ice_data.R) only when rebuilding ice input from raw shapefiles

## Manuscript and supplement artifact index

Status key:

- DONE: recreated from current scripts in this repo
- PENDING: placeholder only; still needs implementation

### Source manuscript/supp files in ms

- Main manuscript PDF: [ms/fmars-13-1727387.pdf](ms/fmars-13-1727387.pdf)
- Supplement image 1 (original): [ms/image 1.tiff](ms/image%201.tiff)
- Supplement image 2 (original): [ms/image 2.jpeg](ms/image%202.jpeg)
- Supplement image 3 (original): [ms/image 3.jpeg](ms/image%203.jpeg)
- Supplement table 1 (original): [ms/table 1.docx](ms/table%201.docx)
- Supplement table 2 (original): [ms/table 2.docx](ms/table%202.docx)

### Main manuscript artifacts

- Figure 1 (study area):
	- [outputs/figure1_study_area.png](outputs/figure1_study_area.png)
	- Supporting map components: [outputs/bathymetry_map.png](outputs/bathymetry_map.png), [outputs/bathymetry_map_outer.png](outputs/bathymetry_map_outer.png)
- Figure 2 (all whale locations by sampling day): PENDING
	- Placeholder: output file not yet created
- Figure 3 (transect position vs day with ice + map panel):
	- [outputs/scatter_whale_location_over_time_transect_with_ice.png](outputs/scatter_whale_location_over_time_transect_with_ice.png)
- Figure 4 (freeze timing trends):
	- Primary scenario currently used in this repo: [outputs/freeze_0p5/scenario_SD250_PCT0p05_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p05_NCT9p7.png](outputs/freeze_0p5/scenario_SD250_PCT0p05_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p05_NCT9p7.png)
	- Additional scenario variants:
		- [outputs/freeze_0p5/scenario_SD250_PCT0p01_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p01_NCT9p7.png](outputs/freeze_0p5/scenario_SD250_PCT0p01_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p01_NCT9p7.png)
		- [outputs/freeze_0p5/scenario_SD250_PCT0p1_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p1_NCT9p7.png](outputs/freeze_0p5/scenario_SD250_PCT0p1_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p1_NCT9p7.png)
		- [outputs/freeze_0p5/scenario_SD250_PCT0p5_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p5_NCT9p7.png](outputs/freeze_0p5/scenario_SD250_PCT0p5_NCT9p7/freeze_doy_firstfreeze_trends_SD250_PCT0p5_NCT9p7.png)
- Figure 5 (dive behavior boxplots): PENDING
	- Placeholder: output file not yet created
- Table 1: PARTIAL (data products exist; final manuscript table export not yet scripted)
	- Inputs used for table derivation: [data/working/whale_df_all.rds](data/working/whale_df_all.rds), [data/working/whale_df_water.rds](data/working/whale_df_water.rds)
- Table 2 (migration boundary crossings): PENDING
	- Placeholder: output file not yet created
- Table 3 (GLMM model comparison): PENDING
	- Placeholder: output file not yet created

### Supplementary artifacts recreated in outputs

- Freeze scenario master outputs:
	- [outputs/freeze_0p5/master/scenario_results_master.csv](outputs/freeze_0p5/master/scenario_results_master.csv)
	- [outputs/freeze_0p5/master/scenario_results_master.xlsx](outputs/freeze_0p5/master/scenario_results_master.xlsx)
	- [outputs/freeze_0p5/master/scenario_daily_master.csv](outputs/freeze_0p5/master/scenario_daily_master.csv)
	- [outputs/freeze_0p5/master/cache_daily_area_bands.rds](outputs/freeze_0p5/master/cache_daily_area_bands.rds)
- Scenario-level regression tables/plots:
	- [outputs/freeze_0p5/scenario_SD250_PCT0p01_NCT9p7](outputs/freeze_0p5/scenario_SD250_PCT0p01_NCT9p7)
	- [outputs/freeze_0p5/scenario_SD250_PCT0p05_NCT9p7](outputs/freeze_0p5/scenario_SD250_PCT0p05_NCT9p7)
	- [outputs/freeze_0p5/scenario_SD250_PCT0p1_NCT9p7](outputs/freeze_0p5/scenario_SD250_PCT0p1_NCT9p7)
	- [outputs/freeze_0p5/scenario_SD250_PCT0p5_NCT9p7](outputs/freeze_0p5/scenario_SD250_PCT0p5_NCT9p7)
- Band-level supplementary plots/tables (all bands per scenario):
	- [outputs/freeze_0p5/band_area_plots](outputs/freeze_0p5/band_area_plots)
	- Example band output (PCT0p05): [outputs/freeze_0p5/scenario_SD250_PCT0p05_NCT9p7/band_67_64/pct_of_baseline_points_FREEZEMARK_band_67_64_SD250_PCT0p05_NCT9p7.png](outputs/freeze_0p5/scenario_SD250_PCT0p05_NCT9p7/band_67_64/pct_of_baseline_points_FREEZEMARK_band_67_64_SD250_PCT0p05_NCT9p7.png)
- Ice-cover context plots used in manuscript interpretation:
	- [outputs/change_ice_cover_over_time_cat10.png](outputs/change_ice_cover_over_time_cat10.png)
	- [outputs/change_ice_cover_over_time_cat10_samplingyears.png](outputs/change_ice_cover_over_time_cat10_samplingyears.png)
	- [outputs/change_ice_cover_over_time_cat5above.png](outputs/change_ice_cover_over_time_cat5above.png)
	- [outputs/change_ice_cover_over_time_cat5above_samplingyears.png](outputs/change_ice_cover_over_time_cat5above_samplingyears.png)

## Pending work tracker

Open placeholders for missing manuscript artifacts and follow-up decisions are tracked in:

- [next_phase_plan.txt](next_phase_plan.txt)
