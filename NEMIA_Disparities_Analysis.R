library(tidyverse)
library(tidycensus)
library(here)
library(fst)
library(sf)
# library(lme4)
library(lubridate)
library(ggridges)
library(patchwork)
library(furrr)
library(broom)
#library(ggforce)
#library(Hmisc)
library(DescTools)
library(metR)
library(readxl)
library(wec)
library(fixest)
library(mgcv)
library(elevatr)
library(qqplotr)
#library(DHARMa)


#### Pull in data ####
# Temperatures <- read_rds(here("/data-coco/NEMIA_temperature/cdh/cdh_tractsSF_2019_06-08.rds")) #to erase
Temperatures_XGBoost <- read_fst(here("data", "summarized_daily_temp_preds_F.fst"))
#Temperatures_XGBoost <- read_fst(here("data", "summarized_daily_temp_preds_areal.fst")) 
#Temperatures_NLDAS <- read_fst(here("data", "summarized_daily_temp_NLDAS_F.fst")) 
KtoF = function(kelvins) (9/5) * kelvins - 459.67
NEMIA_States <- c("09", "23", "25", "33", "44", "50", 
                  "34", "36", "42",
                  "10", "11", "24", "51", "54")

get_state_names_from_fips <- function(){
  
  if(!file.exists(here("data", "census_fips_states.xlsx"))){
  download.file("https://www2.census.gov/programs-surveys/popest/geographies/2017/state-geocodes-v2017.xlsx",
                destfile = here("data", "census_fips_states.xlsx"))
  }
  state_fips_names <- read_xlsx(here("data", "census_fips_states.xlsx"), skip = 5) %>%
    rename(State_FIPS = "State (FIPS)") %>%
    filter(State_FIPS %in% NEMIA_States) %>%
    #mutate(State_FIPS = as.numeric(State_FIPS)) %>%
    select(Name, State_FIPS) %>%
    mutate(Name = if_else(Name == "District of Columbia", "Washington D.C.", Name))
  return(state_fips_names)
}



get_RUCAs_2000 <- function(){

  if(!file.exists(here("data", "ruca_2000.xls"))){
  
    download.file("https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca00.xls?v=543.7",
                destfile = here("data", "ruca_2000.xls"))
  
  }
  
  ruca_2000 <- read_xls(here("data", "ruca_2000.xls")) %>% 
    rename("GEOID" = "State County Tract Code",
           "RUCA_code1" = "RUCA Primary Code 2000",
           "RUCA_code2" = "RUCA Secondary Code 2000") %>%
    dplyr::select(GEOID, RUCA_code1, RUCA_code2) %>%
    mutate(census_year = 2000)
  
  return(ruca_2000)
}

get_RUCAs_2010 <- function(){
  
  if (!file.exists(here("data", "ruca_2010.xlsx"))) {
  
    download.file("https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx?v=543.7",
                  destfile = here("data", "ruca_2010.xlsx"))
  }
  
  ruca_2010 <- read_xlsx(here("data", "ruca_2010.xlsx"), skip = 1) %>%
    rename("GEOID" = "State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)",
           "RUCA_code1" = "Primary RUCA Code 2010",
           "RUCA_code2" = "Secondary RUCA Code, 2010 (see errata)") %>%
    dplyr::select(GEOID, RUCA_code1, RUCA_code2) %>%
    mutate(census_year = 2010)
  
  return(ruca_2010)
}

#### Data cleaning ####
temperatureexcess <- function(temperature, threshold = 65){
  pmax(temperature, threshold) - threshold
}

clean_and_summarize_temperatures <- function(temperature_df){ #rename this to reflect summerwide summaries with cdds
  
  cleaned_temp_df <- temperature_df %>%
    filter(month(date)>=5 & month(date)<=9) %>% 
    mutate(year = year(date),
           noaa_mean_cdd = temperatureexcess(KtoF(noaa_mean)))  %>%
    group_by(year, GEOID) %>%
    summarise(mean_temp_summer = KtoF(mean(mean_temp_daily)),
              #mean_htindx_summer = KtoF(mean(mean_htindx_daily)),
              cdd_summer = sum(cdd),
              nighttime.cdh_summer = sum(nighttime.cdh),
              noaa_cdd = sum(noaa_mean_cdd)) %>%
    mutate(census_year = if_else(year<=2009, 2000, 2010)) %>%
    ungroup()
  
  return(cleaned_temp_df)
}


Temperatures_XGBoost_summer_avgs <- clean_and_summarize_temperatures(Temperatures_XGBoost)
Temperatures_NLDAS_summer_avgs <- clean_and_summarize_temperatures(Temperatures_NLDAS)

# find_vars_2000dec <- load_variables(2000, "sf1")
# find_vars_2010dec <- load_variables(2010, "sf1")


get_Census_tract_data <- function(){
  
  metro = c(1.0, 1.1, 2.0, 2.1, 3.0, 4.1, 5.1, 7.1, 8.1, 10.1)
  
  Tract_RaceEthn_2000Census <- get_decennial(geography = "tract",
                                             variables = c("Black" = "P004006",
                                                           "White" = "P004005",
                                                           "Asian" = "P004008",
                                                           "Latino" = "P004002",
                                                           "Total_pop" = "P004001"),
                                             year = 2000,
                                             survey = "sf1",
                                             output = "wide",
                                             state = NEMIA_States) %>%
    mutate(census_year = 2000) %>%
    left_join(., get_RUCAs_2000(), by = c("GEOID", "census_year")) %>%
    mutate(urban = if_else(RUCA_code2 %in% metro, "metropolitan", "not_metropolitan"))
  
  Tract_RaceEthn_2010Census <- get_decennial(geography = "tract",
                                             variables = c("Black" = "P005004",
                                                           "White" = "P005003",
                                                           "Asian" = "P005006",
                                                           "Latino" = "P005010",
                                                           "Total_pop" = "P004001"),
                                             year = 2010,
                                             survey = "sf1",
                                             output = "wide",
                                             state = NEMIA_States) %>%
    mutate(census_year = 2010) %>%
    left_join(., get_RUCAs_2010(), by = c("GEOID", "census_year")) %>%
    mutate(urban = if_else(RUCA_code2 %in% metro, "metropolitan", "not_metropolitan"))
  
  Tract_RaceEthn_Census <- bind_rows(Tract_RaceEthn_2000Census, Tract_RaceEthn_2010Census) %>%
    mutate(State_FIPS = str_sub(GEOID, 1, 2),
           County_FIPS = str_sub(GEOID, 1, 5),
           Other = Total_pop - (Asian + Black + White + Latino),
           ICE_black_seg = (White - Black) / Total_pop,
           ICE_bipoc_seg = (White - (Total_pop - White)) / Total_pop,
           ICE_latinx_seg = (White - Latino) / Total_pop,
           ICE_asian_seg = (White - Asian)/ Total_pop)
  #BIPOC = Total_pop - White,
  return(Tract_RaceEthn_Census)

}

Tract_RaceEthn_Census <- get_Census_tract_data() 

#turn geometry into x and y coordinate columns  
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


tract_2000_shp <- get_decennial(geography = "tract",
                                variables = c("Total_pop" = "P001001"),
                                year = 2000,
                                output = "wide",
                                geometry = T, 
                                state = NEMIA_States) %>%
  mutate(census_year = 2000,
         County_FIPS = str_sub(GEOID, 1, 5)) %>%
  filter(!st_is_empty(.)) 

tract_2010_shp <- get_decennial(geography = "tract",
              variables = c("Total_pop" = "P001001"),
              year = 2010,
              output = "wide",
              geometry = T, 
              state = NEMIA_States) %>%
  mutate(census_year = 2010,
         County_FIPS = str_sub(GEOID, 1, 5)) %>%
  filter(!st_is_empty(.)) 

county_area_2000 <- tract_2000_shp %>% 
  mutate(area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  group_by(County_FIPS) %>%
  summarise(county_area = sum(area)) %>%
  left_join(tract_2000_shp, ., by = "County_FIPS") %>%#trying to merge 
  mutate(prop_county = as.numeric(st_area(geometry)/county_area))

county_area_2010 <- tract_2010_shp %>% 
  mutate(area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  group_by(County_FIPS) %>%
  summarise(county_area = sum(area)) %>%
  left_join(tract_2010_shp, ., by = "County_FIPS") %>%#trying to merge 
  mutate(prop_county = as.numeric(st_area(geometry)/county_area))

# tract_centroids <- bind_rows(tract_2000_shp, tract_2010_shp) %>%
#   st_centroid() 

tract_centroids <- bind_rows(county_area_2000, county_area_2010) %>% #not a shapefile/sf df -- must fix to get centroids
  st_centroid() 

#can potentially remove elevation 
tract_elevations <- get_elev_point(tract_centroids$geometry, prj = 4326, src = "aws")

tract_centroids1 <- bind_cols(tract_centroids, tract_elevations %>% st_drop_geometry(.) %>% dplyr::select(elevation))

# %>%
#   sfc_as_cols() %>%
#   st_drop_geometry() %>%
#   select(GEOID, x, y, census_year)
##


#### Contour plots ####

all_pred_filepaths <- list.files(here("data", "hourly_tract_preds"), full.names = T)
create_weighted_race_hours_NEMIA <- function(year, urban = c("metropolitan", "not_metropolitan", "all")){
  
  year_i_pred_paths <- enframe(all_pred_filepaths) %>% filter(str_detect(value, year))
  
  year_i_preds <- year_i_pred_paths$value %>%
    map_dfr(read_fst)
  
  summarized_day_hour <- year_i_preds %>%
    mutate(ground.time.nominal = format.POSIXct(ground.time.nominal, tz = "America/New_York"),
           year = year(ground.time.nominal),
           census_year = if_else(year<=2009, 2000, 2010),
           month = month(ground.time.nominal),
           day = day(ground.time.nominal),
           hour = hour(ground.time.nominal),
           day_of_yr = yday(ground.time.nominal)) %>%
    left_join(Tract_RaceEthn_Census, by = c("census_year", "GEOID")) %>%
    filter(month>=5 & month<=9 & Total_pop>0) 
  
  if(urban=="metropolitan"){
    summarized_day_hour <- summarized_day_hour %>%
      filter(urban == "metropolitan")
  }
  
  if(urban=="not_metropolitan"){
    summarized_day_hour <- summarized_day_hour %>%
      filter(urban != "metropolitan")
  }  
  
  summarized_day_hour <- summarized_day_hour %>%
    dplyr::select(year, month, hour, day_of_yr, w_temp, Black, White, Latino, Asian) %>% #change this for racial groups of interest
    pivot_longer(cols = c("Black", "White", "Latino", "Asian"), names_to = "race", values_to = "estimate") %>% #and this
    group_by(month, year, hour, day_of_yr, race) %>%
    summarise(monthhour_temp = matrixStats::weightedMedian(w_temp, estimate)) %>%
    mutate(temp_f = KtoF(monthhour_temp),
           date = as.Date(paste0(year,"-",day_of_yr), "%Y-%j")) %>% 
    select(-monthhour_temp) 
  
  return(summarized_day_hour)
  
}

plan(multisession(workers = 12))
all_years <- as.character(seq.int(2003, 2019))

race_hour_temps_NEMIA_metro <- all_years %>% 
  future_map_dfr(., ~create_weighted_race_hours_NEMIA(.x, urban = "metropolitan"), .progress = T)
race_hour_temps_NEMIA_notmetro <- all_years %>% 
  future_map_dfr(., ~create_weighted_race_hours_NEMIA(.x, urban = "not_metropolitan"), .progress = T)

make_contour_plot_by_race <- function(dataframe, year_to_plot){
  
  plot <- ggplot(dataframe %>% filter(year==year_to_plot), aes(date, hour, z = temp_f)) + #, breaks = c(0,12,18,24,30,36)
    geom_contour_fill(aes(fill = stat(level)), breaks = c(32,55,65,75,90,105)) + 
    facet_wrap(.~race, ncol = 1) + 
    scale_x_date(labels = function(x) format(x, "%b")) +
    scale_fill_divergent_discretised(low = "#4e714f", mid = "#e9cc78", high = "#984c45", midpoint = 65, 
                                     name = "Temperature (°F)", guide = guide_coloursteps(show.limits = T, barwidth = 12)) +
    scale_y_reverse(breaks = c(0,6,12,18,23), labels = c("12 AM", "6 AM", "12 PM", "6 PM", "11 PM")) + 
    ylab("Hour of day") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom", axis.title.x=element_blank(), axis.title.y = element_blank()) 

  return(plot)
}

make_contour_plot_by_race_and_development <- function(dataframe_metro, dataframe_nonmetro, year_to_plot){
  
  plot_metro <- make_contour_plot_by_race(dataframe_metro, year_to_plot) +
    ggtitle("Metropolitan") + 
    theme(axis.title.y = element_text("Hour of day", angle = 90, vjust = 0.9),
          plot.title = element_text(hjust = 0.5)) 
  
  plot_nonmetro <- make_contour_plot_by_race(dataframe_nonmetro, year_to_plot) + 
    ggtitle("Micropolitan and Rural") + 
    theme(axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  plot_metro_and_non <- (plot_metro + plot_nonmetro) + plot_layout(guides = 'collect') & 
    #plot_annotation(title = paste("Air temperature by development type in", year_to_plot))  
    theme(legend.position = 'bottom', plot.title = element_text(size = 14)) 

return(plot_metro_and_non)
  
}

make_contour_plot_by_race_and_development(race_hour_temps_NEMIA_metro, race_hour_temps_NEMIA_notmetro, 2013)



#### Visualize annual CDD density plots by race/ethnicity faceted by state #### 

plot_state_temps_density <- function(df, plot_year, state_fips, temp_measure, xmin, xmax){
  
  df1 <- df %>%
    filter(year == plot_year & State_FIPS == state_fips) %>%
    mutate(weight = estimate/sum(estimate))
  
  medians <- df1 %>%
    group_by(race) %>%
    summarise(median = matrixStats::weightedMedian(get(temp_measure), estimate))
  
  df2 <- df1 %>% 
    left_join(., medians, by = "race")
  
  State_name <- df1$Name[1]
  
  plot <- ggplot() +
    geom_density_ridges(data = df1, aes(x = cdd_summer, 
                            y = race, 
                            group = race,
                            height=..density.., 
                            weight=weight),    
                        scale= 0.95,
                        stat="density") +
    xlim(xmin, xmax) +
    geom_point(data = medians, aes(x = median, y = race), size = 2, shape = 23, color = "black", fill = "grey", position = position_nudge(y = 0.1)) +
    theme_minimal() +
    ggtitle(State_name) +
    theme(text = element_text(size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5)) 
  
  return(plot)
}

# plot_state_temps_density(temps_for_ggplot_density, 2010, "23", "cdd_summer", 0, 1100)

plot_densities <- function(temperature_df, census_df, temp_measure, plot_year){

  temperature_df1 <- temperature_df %>%
    left_join(., census_df, by = c("census_year", "GEOID")) %>%
    filter(Total_pop>0)
  
  temps_for_ggplot_density <- temperature_df1 %>%
    select(-census_year, -Total_pop, -ICE_black_seg, -ICE_bipoc_seg, -ICE_latinx_seg, -Other) %>% #modify this as needed 
    pivot_longer(cols = c("Black", "White", "Latino", "Asian"), names_to = "race", values_to = "estimate") %>%
     left_join(., get_state_names_from_fips(), by = "State_FIPS") 
  
  # first_column <- c("23", "33", "50", "25", "44")
  # second_column <- c("09", "36", "34", "42", "54")
  # third_column <- c("10", "24", "11", "51")
  
  # min_and_max_first_column <- temps_for_ggplot_density %>%
  #   filter(str_detect(State_FIPS, third_column)) %>%
  #   summarise(min_temp = min(get(temp_measure)),
  #             max_temp = max(get(temp_measure)))

  remove_x <- theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.x = element_blank())
  
  first <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, 23, "cdd_summer", 0, 1150) + remove_x)/
    (plot_state_temps_density(temps_for_ggplot_density, plot_year, 33, "cdd_summer", 0, 1150)+ remove_x)/
    (plot_state_temps_density(temps_for_ggplot_density, plot_year, 50, "cdd_summer", 0, 1150)+ remove_x)/
    (plot_state_temps_density(temps_for_ggplot_density, plot_year, 25, "cdd_summer", 0, 1150)+ remove_x)/
    plot_state_temps_density(temps_for_ggplot_density, plot_year, 44, "cdd_summer", 0, 1150)) 
    
  second <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, "09", "cdd_summer", 95, 1900) + remove_x)/
       (plot_state_temps_density(temps_for_ggplot_density, plot_year, 36, "cdd_summer", 95, 1900)+ remove_x)/
       (plot_state_temps_density(temps_for_ggplot_density, plot_year, 34, "cdd_summer", 95, 1900)+ remove_x)/
       (plot_state_temps_density(temps_for_ggplot_density, plot_year, 42, "cdd_summer", 95, 1900)+ remove_x)/
       plot_state_temps_density(temps_for_ggplot_density, plot_year, 54, "cdd_summer", 95, 1900))
  
  #below trims ~8 rows of data on the lower end of the distribution for XGBoost model  
  third <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, 10, "cdd_summer", 500, 2200) + remove_x)/
                 (plot_state_temps_density(temps_for_ggplot_density, plot_year, 24, "cdd_summer", 500, 2200)+ remove_x)/
                 (plot_state_temps_density(temps_for_ggplot_density, plot_year, 11, "cdd_summer", 500, 2200)+ remove_x)/
                 (plot_state_temps_density(temps_for_ggplot_density, plot_year, 51, "cdd_summer", 500, 2200)) / 
              plot_spacer() + theme(plot.margin = unit(c(15,0,0,0), "pt")))
    
    plot_densities <- (first | second| third) + plot_annotation(
      caption = "Cooling degree days (F)",
      theme = theme(
        plot.caption = element_text(size = 12, hjust = 0.5, face = "bold")
      )
    )
  
  return(plot_densities)
}

plot_densities(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, "cdd_summer", 2010)
plot_densities(Temperatures_NLDAS_summer_avgs, Tract_RaceEthn_Census, "cdd_summer", 2010)



#### Calculate mean differences by year, paired by county ####
wec_for_collapsed_data <- function (df, omitted) {
  
  frequencies <- as.table(c(
    Other = sum(df$estimate[df$race=="Other"]),
    Black = sum(df$estimate[df$race=="Black"]),
    Latino = sum(df$estimate[df$race=="Latino"]),
    Asian = sum(df$estimate[df$race=="Asian"]),
    White = sum(df$estimate[df$race=="White"])))
  
  n.cat <- length(table(df$race))
  omitted <- which(levels(df$race) == omitted)
  new.contrasts <- contr.treatment(n.cat, base = omitted)
  new.contrasts[omitted, ] <- -1 * frequencies[-omitted]/frequencies[omitted]
  colnames(new.contrasts) <- names(frequencies[-omitted])
  
  return(new.contrasts)
}

  
run_lmer_tempdisparity <- function(df_county_temp_and_race, temp_measure = c("cdd_summer", "nighttime.cdh_summer")){ #should rename since now feols
  
  state_fips1 <- str_sub(df_county_temp_and_race$County_FIPS[1], 1, 2)
  #contrasts(df_county_temp_and_race$race) <- wec_for_collapsed_data(df_county_temp_and_race, "Other")
  
  if(state_fips1==11){ #Washington DC only has one county -- so fixed effect for county is removed 
    
    feols_formula <- as.formula(paste(temp_measure, "~ race|year"))
    
    }else{
      
      feols_formula <- as.formula(paste(temp_measure, "~ race|County_FIPS + year"))
    }
  
  
  feols_temp_results1 <- feols(feols_formula, data = df_county_temp_and_race, ~GEOID) #weights = df_county_temp_and_race$estimate
  # feols_temp_results1 <- feols(feols_formula, data = df_county_temp_and_race, weights = df_county_temp_and_race$estimate, ~GEOID)
  # feols_temp_results1 <- feols(~ race|County_FIPS + year, data = df_county_temp_and_race, weights = df_county_temp_and_race$estimate, ~GEOID)
  
  results <- enframe(coef(feols_temp_results1)) %>%
    bind_cols(., as_tibble(confint(feols_temp_results1, 
                                   se = "cluster", 
                                   parm = c("raceBlack", "raceLatino", "raceAsian", "raceOther"), #, "raceWhite" 
                                   level = 0.95)) %>%
                rename(lower_ci = "2.5 %",
                       upper_ci = "97.5 %")) %>%
    mutate(State_FIPS = state_fips1)
  
  return(results)
}

Calculate_mean_diffs_by_race <- function(temp_model, census_data, temp_measure = c("cdd_summer", "nighttime.cdh_summer")){
  
  temp_measure <- "cdd_summer" #once fixed -- this all needs to be updated 
  temp_model <- Temperatures_XGBoost_summer_avgs
  census_data <- Tract_RaceEthn_Census 
  
  Temperature_w_Censusdata <- temp_model %>% 
    left_join(., census_data, by = c("GEOID", "census_year")) %>% 
    pivot_longer(cols = c("Black", "White", "Latino", "Asian", "Other"), names_to = "race", values_to = "estimate") %>%
    mutate(race = factor(race, levels = c("White", "Asian", "Black", "Latino", "Other"))) %>% 
    dplyr::select(State_FIPS, year, County_FIPS, temp_measure, race, estimate, GEOID, RUCA_code2, census_year, urban, RUCA_code1) 
  #contrasts(Temperature_w_Censusdata$race) <- wec_for_collapsed_data(Temperature_w_Censusdata, "Other")
  #Temperature_w_Censusdata$race <- relevel(Temperature_w_Censusdata$race, ref = "White")

      # %>% #Black, White, Asian, Latino, Other, GEOID, urban, State_FIPS
  #   filter(estimate!=0) 
  
  # Temperature_w_Censusdata <- Temperatures_NLDAS_summer_avgs %>% 
  #   left_join(., census_data, by = c("GEOID", "census_year")) %>% 
  #   pivot_longer(cols = c("Black", "White", "Latino", "Asian", "Other"), names_to = "race", values_to = "estimate") %>%
  #   mutate(race = factor(race, levels = c("Other", "Asian", "Black", "Latino", "White"))) %>% 
  #   dplyr::select(State_FIPS, year, County_FIPS, temp_measure, race, estimate, GEOID, RUCA_code2, census_year, urban, RUCA_code1) %>% #Black, White, Asian, Latino, Other, GEOID, urban, State_FIPS
  #   filter(estimate!=0) 

  #try to do this to run bam for the mean diffs   
  dat1 <- Temperature_w_Censusdata %>%
    left_join(., tract_centroids1, by = c("GEOID", "census_year", "County_FIPS"))
  dat2 <- dat1 %>% st_as_sf(.$geometry) %>% sfc_as_cols(geometry) %>% st_drop_geometry() %>%
    mutate(County_year = paste0(County_FIPS, "_", year),
           GEOID = as.factor(GEOID),
           year = as.factor(year),
           prop_race = estimate/Total_pop) 
  
  states <- Temperature_w_Censusdata %>%
    split(.$State_FIPS) %>%
    map_dfr(., ~run_lmer_tempdisparity(.x, temp_measure), .progress = T)
  
  # contrasts(Temperature_w_Censusdata$race) <- wec_for_collapsed_data(Temperature_w_Censusdata, "Other")
  # feols_formula <- as.formula(paste(temp_measure, "~ race + splines::ns(elevation, df = 10)|factor(County_FIPS) + factor(year) + 
  #                                   factor(RUCA_code2)")) #log((elevation+100))
  # #feols_temp_results1 <- feols(feols_formula, data = Temperature_w_Censusdata, weights = Temperature_w_Censusdata$estimate, ~GEOID)
  # #feols_temp_results1 <- feols(feols_formula, data = dat2, weights = dat2$estimate, ~GEOID)
  # feols_temp_results1 <- feols(noaa_cdd ~ race + splines::ns(elevation)|factor(County_FIPS) + factor(year) + 
  #                                   factor(RUCA_code2), data = dat2, weights = dat2$estimate, ~GEOID)
  # 
  # qqnorm(scale(residuals(feols_temp_results1)));qqline(scale(residuals(feols_temp_results1)))
  # 
  # dat2_2000 <- dat2 %>% filter(census_year==2000)
  # feols_temp_results2000 <- feols(feols_formula, data = dat2_2000, weights = dat2_2000$estimate, ~GEOID)
  # qqnorm(residuals(feols_temp_results2000));qqline(residuals(feols_temp_results2000))
  # 
  # dat2_2010 <- dat2 %>% filter(census_year==2010)
  # feols_temp_results2010 <- feols(feols_formula, data = dat2_2010, weights = dat2_2010$estimate, ~GEOID)
  # qqnorm(residuals(feols_temp_results2010));qqline(residuals(feols_temp_results2010))
  
  # library(MASS)
  # 
  # rlm_temp_results <- rlm(cdd_summer ~ race + splines::ns(elevation, df = 4) + factor(County_FIPS) + factor(year) + factor(RUCA_code2), data = dat2, 
  #     weights = dat2$estimate)
  # 
  # 
  # gam_mean_temp_diff <- bam(round(cdd_summer,0) ~ race + factor(County_FIPS) + factor(year), family = quasipoisson(), weights = estimate,
  #                           data = Temperature_w_Censusdata, discrete = T, nthreads = 2)
  # start <- proc.time()
  
  
  # bam_model2 = mgcv::bam(noaa_cdd ~  race + factor(County_FIPS) +   
  #                          s(elevation, bs = "cr") + factor(RUCA_code2) +
  #                          te(x, y, year, d = c(2,1), bs = c("cr", "re")), 
  #                        family = gaussian(), weights = estimate, data=dat2, 
  #                        discrete = T, nthreads = 2)
  
  # library(lme4)
  
  feols_try <- feols(scale(noaa_cdd) ~ prop_race + splines::ns(elevation, df =5) |County_FIPS + RUCA_code2 + year, data = dat2 %>% filter(race == "Black"), 
                     panel.id=c('GEOID', 'year'), ~GEOID) # %>% filter(year==2010)) , weights = dat2$estimate[dat2$race=="Black"]
  
  looky <- feols(scale(mean_temp_summer) ~ race*County_FIPS + splines::ns(elevation, df =5) | year + RUCA_code2, data = dat2, weights = dat2$estimate, ~GEOID) 
  residuals <- tibble(resids = residuals(feols_try))
  # + splines::ns(elevation, df =5) 
  qqnorm(residuals(feols_try));qqline(residuals(feols_try))
  qqnorm(residuals(looky));qqline(residuals(looky))
  
  ggplot(data = residuals, mapping = aes(sample = resids)) +
    stat_qq_band(bandType = "pointwise", alpha = 0.5) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  hist(residuals$resids)
  params <- MASS::fitdistr(residuals$resids, "t")
  ggplot(data = residuals, mapping = aes(sample = resids)) +
    stat_qq_band(distribution = "t", dparams = list(ncp = -0.0205, df = 17.53)) +
    stat_qq_line(distribution = "t", dparams = list(ncp = -0.0205, df = 17.53)) +
    stat_qq_point(distribution = "t", dparams = list(ncp = -0.0205, df = 17.53)) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  #regression takes a long time  
  # lmer_model <- lmer(scale(cdd_summer) ~  race + factor(County_FIPS) +   
  #        splines::ns(elevation, df = 5) + (1|GEOID) + (1|year), data=dat2)
  qqnorm(residuals(lmer_model));qqline(residuals(lmer_model))
  
  bam_model2 = mgcv::bam(scale(cdd_summer) ~  race + factor(County_FIPS) +   
                           s(elevation, bs = "cr", k =4) + #factor(RUCA_code2) +
                           s(year, bs = "re") +
                           s(GEOID, bs = "re"), 
                         family = gaussian(), data=dat2, #weights = estimate,
                         discrete = T, nthreads = 4)
  qqnorm(residuals(bam_model2));qqline(residuals(bam_model2))
  
  bam_model3 = mgcv::bam(noaa_cdd ~  race + factor(County_FIPS) +   
                           s(elevation, bs = "cr") + #factor(RUCA_code2) +
                           te(x, y, d = 2, bs = "cr", by = year) +
                           s(GEOID, bs = "re"), 
                         family = gaussian(), weights = estimate, data=dat2, 
                         discrete = T, nthreads = 2)
  qqnorm(residuals(bam_model3));qqline(residuals(bam_model3))
  
  bam_model4 = mgcv::bam(mean_temp_summer ~  race + factor(County_FIPS) +   
                           s(elevation, bs = "cr") + factor(RUCA_code2) +
                           te(x, y, year, d = c(2,1), bs = c("cr", "re")), 
                         family = gaussian(), weights = estimate, data=dat2, 
                         discrete = T, nthreads = 2)

  residuals_bam_model4 <- tibble(resids = residuals(bam_model4))
  ggplot(data = residuals_bam_model5, mapping = aes(sample = resids)) +
    stat_qq_band(bandType = "ks", alpha = 0.5) +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  qqnorm(residuals(bam_model4));qqline(residuals(bam_model4))
  
    bam_model5 = mgcv::bam(noaa_cdd ~ prop_race + s(elevation, bs = "ps", k = 10) + s(GEOID, bs = "re") + 
                             te(x, y, year, d = c(2,1), bs = c("sos", "re"), k = 800), 
                           family = gaussian(),
                           data=dat2 %>% filter(race=="Black" & (year==2006|year==2007|year==2008) & !is.na(noaa_cdd)),
                           discrete = T, nthreads = 2)
    gam.check(bam_model5)
    plot(bam_model5,scale=0,se=2,shade=TRUE,resid=TRUE,pages=2)
    qqnorm(residuals(bam_model5));qqline(residuals(bam_model5))
    residuals_bam_model5 <- tibble(resids = residuals(bam_model5))
    
    
    bam_model_2009 = mgcv::bam(mean_temp_summer ~  s(elevation, bs = "ps", k = 25) + factor(RUCA_code2) +
                             te(x, y, d = 2, bs = "gp", k = 800), 
                           family = gaussian(), data=dat2 %>% distinct(year, GEOID, .keep_all = T) %>% filter(year ==2009), 
                           discrete = T, nthreads = 5)
    gam.check(bam_model_2009)
    # ggplot(data = residuals_bam_model5, mapping = aes(sample = resids)) +
    #   stat_qq_band(bandType = "ks", alpha = 0.5) +
    #   stat_qq_line() +
    #   stat_qq_point() +
    #   labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
    hist(residuals_bam_model5$resids)
    params <- MASS::fitdistr(residuals_bam_model5$resids, "t")
    ggplot(data = residuals_bam_model5, mapping = aes(sample = resids)) +
      stat_qq_band(distribution = "t", dparams = list(ncp = -1.53, df = 1.371)) +
      stat_qq_line(distribution = "t", dparams = list(ncp = -1.53, df = 1.371)) +
      stat_qq_point(distribution = "t", dparams = list(ncp = -1.53, df = 1.371)) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
    
  #race + factor(County_FIPS) + factor(RUCA_code2) + , weights = estimate
  qqnorm_threewaysmooth <- qqnorm(residuals(bam_model5));qqline(residuals(bam_model5))
  
  bam_model5 <- bam(scale(cdd_summer) ~ race + factor(County_FIPS) + factor(year) +
                                factor(RUCA_code2) + te(x, y, d = 2, bs = "gp", by = factor(year)), #try ts and cr isntead te(x, y, d = 2, bs = "gp", by = factor(year)  s(x, y, bs = "cr")
                             family = scat(), weights = estimate,
                             data = dat2, discrete = T, nthreads = 2) 
  
  
  dat2 %>%
    distinct(GEOID, year, .keep_all = T) %>%
    ggplot() +
    geom_histogram(aes(cdd_summer)) + 
    facet_wrap(~year)
  
  qqnorm(residuals(gam_mean_temp_diff1));qqline(residuals(gam_mean_temp_diff1))
  qqnorm(residuals(bam_model2));qqline(residuals(bam_model2))
  plot(residuals(gam_mean_temp_diff1))
proc.time() - start

# 
# treg_mean_temp_diff <- treg(ols_mean_temp_diff, r = 4.5)
# # treg_mean_temp_diff1 <- treg(ols_mean_temp_diff, r = 2.5)
# # treg_mean_temp_diff2 <- treg(ols_mean_temp_diff, r = 3)
# qqnorm(residuals(treg_mean_temp_diff));qqline(residuals(treg_mean_temp_diff))
# dat3_resids <- bind_cols(dat2, tibble(resid = scale(residuals(feols_temp_results1)))) %>%
#   st_as_sf(.$geometry)
# 
# dat3_resids %>% ggplot() + geom_sf()
# 
# big_resids <- dat3_resids %>% mutate(big_resid = if_else(resid > 3, "positive",
#                                            if_else(resid < -3, "negative", "between"))) %>%
#   filter(big_resid != "between")

# library(heavy)  
# ?heavyLm.fit
# ny_uncount <- Temperature_w_Censusdata %>%
#   filter(State_FIPS=="36") %>%
#   dplyr::select(year, County_FIPS, cdd_summer, race, RUCA_code2, estimate) %>%
#   uncount(estimate)
# 
# heavylm_mean_temp_diff  <- heavyLm(cdd_summer ~ race + factor(County_FIPS) + factor(year) + 
#           factor(RUCA_code2), data = ny_uncount)
# 
#   ggplot() + geom_sf(data = ) geom_sf(data = big_resids, aes(color = big_resid)) + facet_wrap(.~year)
#   #sigma <- feols_temp_results1$sigma2
#   #plot(feols_temp_results1$residuals/(sigma*sqrt(1-(hat(model.matrix(feols_temp_results1)))))) #studentized residuals
#   hist(residuals(feols_temp_results1), density=20, breaks=1000, probability = T)
#   curve(dnorm(x, mean=mean(residuals(feols_temp_results1)), sd=sd(residuals(feols_temp_results1))),
#         col="darkblue", lwd=2, add=TRUE, yaxt="n")
#   
#   results <- enframe(coef(feols_temp_results1)) %>%
#     bind_cols(., as_tibble(confint(feols_temp_results1, 
#                                    se = "cluster", 
#                                    parm = c("raceBlack", "raceLatino", "raceAsian", "raceWhite"), 
#                                    level = 0.95)) %>%
#                 rename(lower_ci = "2.5 %",
#                        upper_ci = "97.5 %")) %>%
#     mutate(State_FIPS = as.character(00))
  
  
  results1 <- bind_rows(results, states)
  
  return(results1)
  
}

Temperature_w_Censusdata %>%
  filter(State_FIPS=="36") %>%
  run_lmer_tempdisparity(df_county_temp_and_race = ., temp_measure = "cdd_summer")

feols_results_race_meandiffs <- Calculate_mean_diffs_by_race(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, "cdd_summer")


plot_state_temps_mean_diffs <- function(df, state_fips, xmin, xmax){

  df1 <- df %>%
    filter(State_FIPS == state_fips) %>%
    left_join(., get_state_names_from_fips(), by = "State_FIPS")
  
  State_name <- df1$Name[1]
  
  plot <- df1 %>%
    mutate(race = factor(name, levels = c("raceWhite", "raceLatino", "raceBlack", "raceAsian"), 
                         labels = c("White", "Latino", "Black", "Asian"))) %>%
    ggplot() +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    geom_pointrange(aes(x = race, y = value, ymax = upper_ci, ymin = lower_ci)) +
    coord_flip(ylim = c(xmin, xmax)) +
    theme_minimal() +
    ggtitle(State_name) +
    theme(text = element_text(size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5)) 
  
  return(plot)
}

plot_temp_mean_diffs <- function(df_from_feols, xmin, xmax){
 
  # df_from_feols <- feols_results_race_meandiffs
  # xmin = -20
  # xmax = 60
  
  first <- plot_state_temps_mean_diffs(df_from_feols, "23", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "33", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "50", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "25", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "44", xmin, xmax)
  
  second <- plot_state_temps_mean_diffs(df_from_feols, "09", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "36", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "34", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "42", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "54", xmin, xmax)
  
  third <- plot_state_temps_mean_diffs(df_from_feols, "10", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "24", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "11", xmin, xmax)/
    plot_state_temps_mean_diffs(df_from_feols, "51", xmin, xmax)/
    (plot_spacer() + theme(plot.margin = unit(c(25,0,0,0), "pt")))
    
  plot <- (first | second | third) + 
    plot_annotation(
      caption = "Difference from county mean (F°)",
      theme = theme(
        plot.caption = element_text(size = 12, face = "bold", hjust = 0.5)
      )
    )
  
  return(plot)
}

plot_temp_mean_diffs(feols_results_race_meandiffs, -20, 60)

jackknife_resids_for_fixest <- function(model){
  
  std_resid <- scale(model$residuals)
  n <- model$nobs
  k <- model$nparams
  
  resids <- std_resid*((((n - k - 2)/(n - k - 1 - std_resid^2)))^(1/2))
  
  return(resids)
}
#### Look at relationship between segregation and temperature ####

pct_seg <- Tract_RaceEthn_Census %>%
  filter(Total_pop>0) %>%
  mutate_at(vars(Black, Latino, Asian, White), ~./Total_pop) %>%
  rename(pct_asian = Asian,
         pct_black = Black,
         pct_latinx = Latino,
         pct_white = White) %>% 
  select(GEOID, pct_black, pct_latinx, pct_asian, pct_white, ICE_black_seg, ICE_bipoc_seg, ICE_latinx_seg, 
         ICE_asian_seg, Total_pop, census_year, State_FIPS, County_FIPS, RUCA_code1, RUCA_code2, urban) 

# Temperatures_NLDAS_resseg <- Temperatures_NLDAS %>%
#   left_join(., pct_seg, by = c("census_year", "GEOID")) %>%
#   mutate(year = factor(year),
#          County_FIPS = as.factor(County_FIPS)) %>%
#   select(-noaa_cdd, -census_year, -mean_temp_summer) %>%
#   pivot_longer(cols = ends_with("seg"), names_to = "res_seg", values_to = "pct") %>%
#   pivot_longer(cols = c("cdd_summer", "nighttime.cdh_summer", "mean_htindx_summer"), names_to = "temp_measure", values_to = "temp") %>%
#   mutate(model = "NLDAS",
#          split = paste(model, res_seg, temp_measure, year, sep = "."))

# Temperatures_NLDAS_resseg %>% filter(temp_measure=="cdd_summer") %>%
# ggplot(aes(x = ICE_black, y = temp)) + geom_point() + geom_smooth(method = "loess")

Temperatures_XGBoost_resseg <- Temperatures_XGBoost_summer_avgs %>%
  left_join(., pct_seg, by = c("census_year", "GEOID")) %>%
  filter(Total_pop>0) %>%
  mutate(year = as.factor(year), #, ordered = T
         County_FIPS = as.factor(County_FIPS),
         ICE_bipoc_above_point5 = if_else(ICE_bipoc_seg>=.5, "high_white", "low_white")) %>%
  inner_join(., sfc_as_cols(tract_centroids1), by = c("GEOID", "census_year")) 

# start <- proc.time()
# bam_model = mgcv::bam(cdd_summer ~  s(ICE_bipoc_seg, bs = "tp", k = 4) + County_FIPS + year + factor(RUCA_code2) +
#                         te(x, y, elevation, year, d = c(3,1), bs = c("gp", "re")), 
#                       family = inverse.gaussian(), data=Temperatures_XGBoost_resseg, 
#                       discrete = T, nthreads = 2)
# proc.time() - start 

bam_model2 = mgcv::bam(cdd_summer ~  s(ICE_bipoc_seg, bs = "cr", k = 4) + County_FIPS +  
                         s(elevation, bs = "cr", k = 4) + factor(RUCA_code2) +
                           te(x, y, year, d = c(2,1), bs = c("gp", "re"), m = 2), 
                      family = scat(), data=Temperatures_XGBoost_resseg, 
                      discrete = T, nthreads = 2)
#According to this: https://stats.stackexchange.com/questions/401278/step-failure-in-theta-estimation-using-bam-in-mgcv
#the scat distribution always throws an error but produces valid results 
sim_dharma_bam <- simulateResiduals(bam_model2, n = 1000)
testUniformity(simulationOutput = sim_dharma_bam)

dharmaRes <- createDHARMa(simulatedResponse = sim_dharma_bam$scaledResiduals, 
                          observedResponse = scale(bam_model2$residuals))

plot(dharmaRes, quantreg = FALSE)
dharma_bam <- createDHARMa(bam_model2$fitted.values, bam_model2$y)

resids <- Temperatures_XGBoost_resseg %>% bind_cols(., resids = scale(residuals(bam_model)))%>% filter(abs(resids)>3) %>%   
  st_as_sf(.,coords=c("x","y"),crs=4326) %>%
  mutate(resids1 = factor(if_else(resids>0, "high", "low")))


ggplot() + 
  geom_sf(data = Temperatures_XGBoost_resseg %>% 
            filter(!GEOID %in% resids$GEOID) %>% 
            st_as_sf(., coords=c("x", "y"), crs=4326)) +
  geom_sf(data = resids, aes(color = resids1)) +
  facet_wrap(~year)

#6:08 County_FIPS +
#splines::ns(ICE_black_seg,2) + splines::ns(ICE_latinx_seg,2) + splines::ns(ICE_asian_seg, 2)




feols_bipoc_results <- feols(cdd_summer ~ factor(ICE_bipoc_above_point5)|factor(County_FIPS) + factor(year), 
                             panel.id=c('GEOID', 'year'), data = Temperatures_XGBoost_resseg, ~GEOID)
plot(residuals(feols_bipoc_results))
sigma <- feols_bipoc_results$sigma2
plot(feols_bipoc_results$residuals/(sigma*sqrt(1-(hat(model.matrix(feols_bipoc_results)))))) #studentized residuals
hist(residuals(feols_bipoc_results), density=20, breaks=20, probability = T)
curve(dnorm(x, mean=mean(residuals(feols_bipoc_results)), sd=sd(residuals(feols_bipoc_results))),
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
qqnorm(residuals(feols_bipoc_results));qqline(residuals(feols_bipoc_results))
plot(jackknife_resids_for_fixest(feols_bipoc_results))
Temperatures_XGBoost_resseg %>%
  filter(year == "2011" & Total_pop>0) %>%
  ggplot(aes(x = cdd_summer, y = ICE_bipoc_seg)) + 
  geom_point() + 
  geom_smooth(method = "loess") + 
  facet_wrap(~State_FIPS, scales = "free_x")

Temperatures_XGBoost_resseg %>%
  filter(year == "2016" & Total_pop>0) %>%
  #mutate(ICE_black_greater0 = if_else(ICE_black_seg>))
  ggplot(aes(x = cdd_summer, y = ICE_black_seg)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ splines::ns(x, 3), se = T)  
  facet_wrap(~State_FIPS, scales = "free_x")


feols()

length(unique(Tract_RaceEthn_2000Census$GEOID[Tract_RaceEthn_2000Census$Total_pop>0]))
length(unique(Tract_RaceEthn_2000Census$GEOID[Tract_RaceEthn_2010Census$Total_pop>0]))

pct_seg %>%
  group_by(census_year) %>%
  mutate_at(vars(starts_with("pct")), ~if_else(.>=.9, 1, 0)) %>%
  summarise_at(vars(starts_with("pct")), ~sum(.))

pct_seg %>%
  group_by(census_year) %>%
  mutate_at(vars(starts_with("ICE")), ~if_else(.>=.75, 1, 0)) %>%
  summarise_at(vars(starts_with("ICE")), ~sum(.))


run_lmer_tempdisparity <- function(df_temp_and_seg){
  
  # df_temp_and_seg <- Temperatures_NLDAS_resseg %>% filter(split=="NLDAS.pct_black.cdd_summer")
  
  dataset_name <- paste(df_temp_and_seg[1,9], df_temp_and_seg[1,5], df_temp_and_seg[1,7], df_temp_and_seg[1,1], sep = ".")
  # pct_measure <- "pct_black"
  # temp_measure <- df_temp_and_seg$temp[1]
  # pct_race_ethnic <- df_temp_and_seg$pct[1]
  
  # lmer_formula <- as.formula(paste(temp," ~ ", pct," + (1+ ", pct," | year/County_FIPS)"))
  # lmer_formula <- as.formula(temp ~ pct + (1+ pct | year/County_FIPS))
  
  lmer_output <- lmer(temp ~ pct + (1+ pct | County_FIPS), data = df_temp_and_seg) #year/County_FIPS
  
  confint_lmer <- as_tibble(confint(lmer_output, "pct", level = 0.95)) %>% rename(lower_ci = "2.5 %",
                                                                                  upper_ci = "97.5 %")
  
  results <- tibble(coef = c("intercept", "pct"), value = fixef(lmer_output)) %>% filter(coef != "intercept") %>% mutate(model = dataset_name) %>% bind_cols(., confint_lmer)
  
  return(results)
}

# models_to_run <- crossing(df_temp_and_seg = c("Temperatures_NLDAS_resseg", "Temperatures_XGBoost_resseg"), 
#                           temp_measure = c("cdd_summer", "nighttime.cdh_summer", "mean_htindx_summer"), 
#                           pct_race_ethnic = c("pct_asian", "pct_black", "pct_latinx", "pct_poc")) 

plan(multisession(workers = 8))

# trial_run <- Temperatures_NLDAS_resseg %>%
#   filter(res_seg == "ICE_black_seg", year == 2009, temp_measure =="cdd_summer")
# 
# lets_see <- lmer(temp ~ scale(pct) + (scale(pct) || County_FIPS), data = trial_run)
# summary(lets_see)
# plot(lets_see)

results_lmer_NLDAS <- Temperatures_NLDAS_resseg %>% 
  split(.$split) %>%
  future_map_dfr(., ~run_lmer_tempdisparity(.x), .progress = T)

results_lmer_XGBoost <- Temperatures_XGBoost_resseg %>% 
  split(.$split) %>%
  future_map_dfr(~run_lmer_tempdisparity(.x), .progress = T)

results_lmer_merged <- bind_rows(results_lmer_NLDAS, results_lmer_XGBoost) %>%
  mutate(temp_model = str_extract(model, "XGBoost|NLDAS"),
         seg_measure = str_extract(model, "pct_black|pct_bipoc|pct_latinx|ICE_black|ICE_bipoc|ICE_latinx"),
         temp_measure = str_extract(model, "cdd_summer|mean_htindx_summer|nighttime.cdh_summer"),
         year = as.numeric(str_extract(model, "[:digit:]{1,2}"))+2002) #make a year column, add 2002

# results_lmer_merged %>% 
#   filter(temp_measure=="cdd_summer" & str_detect(seg_measure, "pct") & !str_detect(seg_measure, "latinx")) %>%
#   ggplot(aes(color = seg_measure)) + #group = interaction(seg_measure, temp_model) 
#   geom_point(aes(x = temp_model, 
#                  y = value, shape = seg_measure), size = 2.5, position = position_dodge(width = 1/2)) + 
#   geom_linerange(aes(x = temp_model, 
#                      ymin = lower_ci,
#                      ymax = upper_ci), position = position_dodge(width = 1/2)) + 
#   facet_grid(.~year) +
#   theme_light() +
#   theme(axis.text.x = element_text(angle = 45, vjust = .75, hjust = .75))
#   theme(panel.grid.minor.x = element_blank())

CDD_lmer_plot <- results_lmer_merged %>% 
  # filter(temp_measure=="cdd_summer" & str_detect(seg_measure, "ICE") & !str_detect(seg_measure, "latinx")) %>% 
  filter(temp_measure=="cdd_summer" & seg_measure == "ICE_black") %>% 
  mutate(seg_measure = if_else(seg_measure=="ICE_bipoc", "ICE BIPOC", "ICE Black")) %>%
  ggplot(aes(shape = temp_model)) + #aes(color = temp_model)
  geom_point(aes(x = year, 
                 y = value), size = 4) + #, position = position_dodge(width = 1/2)
  geom_linerange(aes(x = year, 
                     ymin = lower_ci,
                     ymax = upper_ci)) + #, position = position_dodge(width = 1/2)
  #facet_grid(.~year) +
  scale_x_continuous(n.breaks = 15) +
  theme_light() +
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 45, vjust = .75, hjust = .75), axis.title.x=element_blank(), axis.line.y = element_blank()) +
  ylab("Cooling Degree Days (C°)") + 
  labs(color = "Temperature\nModel", shape = "Temperature\nModel")

CDD_lmer_plot

CDH_lmer_plot  <- results_lmer_merged %>% 
  filter(temp_measure=="nighttime.cdh_summer" & seg_measure == "ICE_black") %>%
  mutate(seg_measure = if_else(seg_measure=="ICE_bipoc", "ICE BIPOC", "ICE Black")) %>%
  ggplot(aes(shape = temp_model)) + #aes(color = temp_model)
  geom_point(aes(x = year, 
                 y = value), size = 4) + #, position = position_dodge(width = 1/2)
  geom_linerange(aes(x = year, 
                     ymin = lower_ci,
                     ymax = upper_ci)) + #, position = position_dodge(width = 1/2)
  #facet_grid(.~year) +
  scale_x_continuous(n.breaks = 15) +
  theme_light() +
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 45, vjust = .75, hjust = .75), axis.title.x=element_blank(), axis.line.y = element_blank()) +
  ylab("Nighttime Cooling Degree Hours (C°)") + 
  labs(color = "Temperature\nModel", shape = "Temperature\nModel")

CDH_lmer_plot

#CDD_lmer_plot + CDH_lmer_plot

##Exploring to get the regressions to run correctly

long_census_data <- census_data %>% 
  pivot_longer(cols = c("Black", "White", "Latino", "Asian", "Other"), names_to = "race", values_to = "estimate") %>%
  mutate(race = factor(race, levels = c("Other", "Asian", "Black", "Latino", "White"))) %>% 
  dplyr::select(GEOID, race, estimate, census_year) 

Temperatures_XGBoost1 <- Temperatures_XGBoost %>%
  filter(month(date)>=5 & month(date)<=9) %>% 
  mutate(year = year(date),
         day_of_year = yday(date),
         doy_year = paste0(day_of_year, "_", year),
         census_year = if_else(year<=2009, 2000, 2010)
         ) %>%
  dplyr::select(day_of_year, year, GEOID, mean_temp_daily, census_year) 
  
dat1 <- Temperatures_XGBoost1 %>%
  left_join(., tract_centroids1, by = c("GEOID", "census_year")) %>%
  dplyr::select(day_of_year, year, GEOID, mean_temp_daily, geometry, elevation, County_FIPS) %>% #doy_year, census_year
  st_as_sf(.$geometry) %>% sfc_as_cols(geometry) %>% st_drop_geometry() %>%
  dplyr::select(-geometry) #%>%
  left_join(., long_census_data, by = c("GEOID", "census_year"))
  
NLDAS_dat1 <- Temperatures_NLDAS %>%
  filter(month(date)>=5 & month(date)<=9) %>% 
  mutate(year = year(date),
         day_of_year = yday(date),
         doy_year = paste0(day_of_year, "_", year),
         census_year = if_else(year<=2009, 2000, 2010)) %>%
  dplyr::select(day_of_year, year, GEOID, mean_temp_daily, census_year) %>%
    left_join(., tract_centroids1, by = c("GEOID", "census_year")) %>%
    dplyr::select(day_of_year, year, GEOID, mean_temp_daily, geometry, elevation, County_FIPS) %>% #doy_year, census_year
    st_as_sf(.$geometry) %>% sfc_as_cols(geometry) %>% st_drop_geometry() %>%
    dplyr::select(-geometry) #%>%
  left_join(., long_census_data, by = c("GEOID", "census_year"))

big_bam_model = mgcv::bam(mean_temp_daily ~ s(elevation, bs = "cr") + 
                         te(day_of_year, x, y, d = c(1,2), bs = c("cc","cr"), by = year), 
                       family = gaussian(), data= dat1, 
                       discrete = T, nthreads = 2)
nortest::ad.test(residuals(big_bam_model))

big_bam_model1 = mgcv::bam(mean_temp_daily ~ s(elevation, bs = "cr") +
                            te(day_of_year, x, y, d = c(1,2), bs = c("cc","cr"), by = year), 
                          family = scat(), data= NLDAS_dat1, 
                          discrete = T, nthreads = 2)

#install.packages("qqplotr")


params <- MASS::fitdistr(residuals(big_bam_model1), "t")

ggplot(data = smp, mapping = aes(sample = norm)) +
  geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")

df <- data.frame(y = rt(200, df = 5))

ggplot(data = tibble(resids = sample(residuals(big_bam_model1), 1000000)), mapping = aes(sample = resids)) +
  #stat_qq_band(distribution = "t", dparams = params$estimate) +
  stat_qq_line(distribution = "t", dparams = as.list(params$estimate)) +
  stat_qq_point(distribution = "t", dparams = as.list(params$estimate)) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

ggplot(data = tibble(resids = sample(residuals(big_bam_model1), 1500000)), mapping = aes(sample = resids)) +
  stat_qq_band(distribution = "t", dparams = list(ncp = 0, df = 588.53), bandType = "boot", alpha = .5) +
  stat_qq_line(distribution = "t", dparams = list(ncp = 0, df = 588.53)) +
  stat_qq_point(distribution = "t", dparams = list(ncp = 0, df = 588.53)) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

sample_resids <- sample(residuals(big_bam_model1), 1000000)
qqnorm(sample_resids);qqline(sample_resids)
nortest::ad.test(residuals(big_bam_model1))

qqplot(qt(ppoints(n), df = nu_est), resids * sqrt(tau_est),
       xlab = "Theoretical quantile", ylab = "residuals")
qqline(resids * sqrt(tau_est), lty = 2)

big_bam_model = mgcv::bam(cdd ~  race + factor(County_FIPS) +   
                         s(elevation, bs = "cr") +
                         te(x, y, doy_year, d = c(2,1), bs = c("gp", "re"), m = 2), 
                       family = gaussian(), na.action = na.exclude, data=dat1, 
                       discrete = T, nthreads = 2)

