library(targets)
library(tarchetypes)
library(future)
library(future.callr)
source("temp_disparities_functions.R")
plan(callr)
options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c(
    "tidyverse",
    "tidycensus",
    "fst",
    "sf",
    "lubridate",
    "ggridges",
    "patchwork",
    "furrr",
    "broom",
    "DescTools",
    "metR",
    "readxl"
  ),
  format = 'qs', 
  workspace_on_error = TRUE
)
NEMIA_States <- c("09", "23", "25", "33", "44", "50",
                  "34", "36", "42",
                  "10", "11", "24", "51", "54")
all_years <- as.character(seq.int(2003, 2019))

list(
  tar_target(summarized_temps_file,
    "data/summarized_daily_temp_preds_F.fst",
    format = "file"),
  
  tar_target(summarized_temps,
    read_fst(summarized_temps_file)),
  
  tar_target(Temperatures_XGBoost_summer_avgs,
    clean_and_summarize_temperatures(summarized_temps)),
  
  tar_target(RUCAs_2000,
    get_RUCAs_2000()),
  
  tar_target(RUCAs_2010,
    get_RUCAs_2010()),
  
  tar_target(Tract_RaceEthn_Census,
    get_Census_tract_data(RUCAs_2000, RUCAs_2010)),
  
  tar_target(race_hour_temps_NEMIA_metro,
      purrr::map_dfr(all_years, ~create_weighted_race_hours_NEMIA(.x, urban = "metropolitan", Tract_RaceEthn_Census), .progress = T) %>%
      tar_group(),
    iteration = "group"),
  
  tar_target(race_hour_temps_NEMIA_notmetro,
             purrr::map_dfr(all_years, ~create_weighted_race_hours_NEMIA(.x, urban = "not_metropolitan", Tract_RaceEthn_Census), .progress = T) %>%
               tar_group(),
             iteration = "group"),
  
  tar_target(contour_plots,
  make_contour_plot_by_race_and_development(race_hour_temps_NEMIA_metro, race_hour_temps_NEMIA_notmetro, 2010)),#update for year to plot
  
  tar_target(density_plots,
             plot_densities(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, "cdd_summer", 2010)),#update for NLDAS with Temperatures_XGBoost_summer_avgs
)
# Temperatures_XGBoost <- read_fst(here("data", "summarized_daily_temp_preds_F.fst")) 
# Temperatures_NLDAS <- read_fst(here("data", "summarized_daily_temp_NLDAS_F.fst")) 
