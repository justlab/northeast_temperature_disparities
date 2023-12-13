library(targets)
library(tarchetypes)
library(future)
library(future.callr)
source("code/Analysis_Functions.R")
plan(callr)

# The tigris option can be set as you have done:
options(tigris_use_cache = TRUE)

# The supplemental analysis can be skipped if desired:
options(SKIP_SUPPL_ANALYSIS = FALSE)

tar_option_set(
  packages = c(
    "tidyverse",
    "tidycensus",
    "here",
    "fst",
    "sf",
    "lubridate",
    "ggridges",
    "patchwork",
    "furrr",
    "broom",
    "DescTools",
    "metR",
    "readxl",
    "wec",
    "fixest",
    "mgcv",
    "elevatr",
    "matrixStats",
    "spdep",
    "tigris",
    "spatstat",
    "maptools",
    "gratia",
    "grid",
    "marginaleffects",
    "ggExtra",
    "ggpubr"
  ),
  format = 'rds', 
  workspace_on_error = TRUE
)



list(
  # LOAD FILES ####
  tar_target(temperatures_xgboost, {
    read_temperatures_xgboost()
  }),
  tar_target(fips_to_statename_crosswalk, {
    get_state_names_from_fips()
  }),
  tar_target(Tract_RaceEthn_Census, {
    get_Census_tract_data()
  }),
  tar_target(tract_centroids, {
    get_tract_xy_and_elevation()
  }),
  
  
  # SUMMARIZE FILES ####
  tar_target(Temperatures_XGBoost_summer_avgs, {
    clean_and_summarize_temperatures(temperatures_xgboost)
  }),
  
  #Plot 1 ####
  tar_target(
    plot1,
    {
      create_plot_1(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, fips_to_statename_crosswalk)
    }
  ),
  
  #Table 1 ####
  tar_target(
    table1,
    {
      create_table_1(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census,tract_centroids,fips_to_statename_crosswalk)
    }
  ),
  
  #SEGREGATION ####
  tar_target(
    Kernel_ResSeg_acrossNEMIA,
    create_resseg_across_NEMIA_allyears(tract_centroids,Tract_RaceEthn_Census)
  ),
  tar_target(
    Temperatures_w_resseg,
    {
      create_df_for_seg_analysis(
        temp_model = Temperatures_XGBoost_summer_avgs, 
        census_data = Tract_RaceEthn_Census, 
        seg_measures = Kernel_ResSeg_acrossNEMIA,
        tract_centroids
      )
    }
  ),
  tar_target(
    all_region_seg_bams,
    {
      make_all_region_seg_bams(Temperatures_w_resseg, knots = 3)
    }
  ),
  
  #SEGREGATION PLOTS####
  tar_target(
    plot2,
    {
      create_plot_2(Temperatures_w_resseg,all_region_seg_bams)
    }
  ),
  
  # Sensitivity and supplemental analyses ####
  tar_target(
    supp_table1,
    {
      #if (isTRUE(getOption("SKIP_SUPPL_ANALYSIS"))) return(NULL)
      create_supp_table_1(Temperatures_XGBoost_summer_avgs)
    }
  ),
  tar_target(
    all_states_seg_bams,
    {
      #if (isTRUE(getOption("SKIP_SUPPL_ANALYSIS"))) return(NULL)
      make_all_states_seg_bams(Temperatures_w_resseg)
    }
  ),
  tar_target(
    supp_state_plots,
    {
      #if (isTRUE(getOption("SKIP_SUPPL_ANALYSIS"))) return(NULL)
      create_supp_fig_states_seg_regression_plots(Temperatures_w_resseg, all_states_seg_bams)
    }
  ),
  tar_target(
    all_region_ice_bams,
    {
      #if (isTRUE(getOption("SKIP_SUPPL_ANALYSIS"))) return(NULL)
      make_all_region_ice_bams(Temperatures_w_resseg)
    }
  ),
  tar_target(
    all_ice_bam_plot,
    {
      #if (isTRUE(getOption("SKIP_SUPPL_ANALYSIS"))) return(NULL)
      create_ice_bam_plots(all_region_ice_bams, Temperatures_w_resseg)
    }
  ),
  tar_target(
    timeseries_plot,
    {
      #if (isTRUE(getOption("SKIP_SUPPL_ANALYSIS"))) return(NULL)
      create_timeseries_plots(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, tract_centroids)
    }
  ),
  tar_target(
    seg_bam_2knots,
    {
      #if (isTRUE(getOption("SKIP_SUPPL_ANALYSIS"))) return(NULL)
      make_all_region_seg_bams(Temperatures_w_resseg, knots = 2)
    }
  ),
  tar_target(
    seg_plot_2knots,
    {
      create_plot_2(Temperatures_w_resseg,all_region_seg_bams, body = F)
    }
  )
)

