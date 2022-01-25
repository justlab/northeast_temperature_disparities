library(tidyverse)
library(tidycensus)
library(here)
library(fst)
library(sf)
#install.packages("lme4")
library(lme4)
library(lubridate)
library(ggridges)
library(patchwork)
library(furrr)
library(broom)
library(ggforce)
library(Hmisc)
library(DescTools)
library(metR)
library(readxl)
library(tmap)

#### Pull in data ####
# Temperatures <- read_rds(here("/data-coco/NEMIA_temperature/cdh/cdh_tractsSF_2019_06-08.rds")) #to erase
Temperatures_XGBoost <- read_fst(here("data", "summarized_daily_temp_preds_F.fst")) 
Temperatures_NLDAS <- read_fst(here("data", "summarized_daily_temp_NLDAS_F.fst")) 
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
    select(GEOID, RUCA_code1, RUCA_code2) %>%
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
    select(GEOID, RUCA_code1, RUCA_code2) %>%
    mutate(census_year = 2010)
  
  return(ruca_2010)
}

#### Data cleaning ####

clean_and_summarize_temperatures <- function(temperature_df){ #rename this to reflect summerwide summaries with cdds
  
  cleaned_temp_df <- temperature_df %>%
    filter(month(date)>=5 & month(date)<=9) %>% 
    mutate(year = year(date),
           noaa_mean_cdd = KtoF(noaa_mean) - 65,
           noaa_mean_cdd = ifelse(noaa_mean_cdd<0, 0, noaa_mean_cdd)) %>%
    group_by(year, GEOID) %>%
    summarise(mean_temp_summer = KtoF(mean(mean_temp_daily)),
              mean_htindx_summer = KtoF(mean(mean_htindx_daily)),
              cdd_summer = sum(cdd),
              nighttime.cdh_summer = sum(nighttime.cdh),
              noaa_cdd = sum(noaa_mean_cdd)) %>%
    mutate(census_year = if_else(year<=2009, 2000, 2010)) %>%
    ungroup()
  
  return(cleaned_temp_df)
}


Temperatures_XGBoost_summer_avgs <- clean_and_summarize_temperatures(Temperatures_XGBoost)
Temperatures_NLDAS_summer_avgs <- clean_and_summarize_temperatures(Temperatures_NLDAS)

find_vars_2000dec <- load_variables(2000, "sf1")
find_vars_2010dec <- load_variables(2010, "sf1")


get_Census_tract_data <- function(){
  
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
    left_join(., get_RUCAs_2000(), by = c("GEOID", "census_year"))
  
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
    left_join(., get_RUCAs_2010(), by = c("GEOID", "census_year"))
  
  Tract_RaceEthn_Census <- bind_rows(Tract_RaceEthn_2000Census, Tract_RaceEthn_2010Census) %>%
    mutate(State_FIPS = str_sub(GEOID, 1, 2),
           County_FIPS = str_sub(GEOID, 1, 5),
           Other = Total_pop - (Asian + Black + White + Latino),
           ICE_black_seg = (White - Black) / Total_pop,
           ICE_bipoc_seg = (White - (Total_pop - White)) / Total_pop,
           ICE_latinx_seg = (White - Latino) / Total_pop)
  #BIPOC = Total_pop - White,
  return(Tract_RaceEthn_Census)

}

Tract_RaceEthn_Census <- get_Census_tract_data() 

#grab a map to visualize 
tract_2010_shp <- get_decennial(geography = "tract",
              variables = c("Total_pop" = "P004001"),
              year = 2010,
              survey = "sf1",
              output = "wide",
              geometry = T, 
              state = NEMIA_States)

tract_2010_shp %>% 
  inner_join(., Tract_RaceEthn_Census %>% filter(census_year == 2010 & !RUCA_code2 %in% metropolitan), by = "GEOID") %>%
  ggplot() + 
  geom_sf()

#### Contour plots ####

all_pred_filepaths <- list.files(here("data", "hourly_tract_preds"), full.names = T)
create_weighted_race_hours_NEMIA <- function(year, urban = c("metropolitan", "not_metropolitan", "all")){
  
  metro = c(1.0, 1.1, 2.0, 2.1, 3.0, 4.1, 5.1, 7.1, 8.1, 10.1)
  
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
    filter(month>=5 & month<=9) %>%
    left_join(Tract_RaceEthn_Census, by = c("census_year", "GEOID")) 
  
  if(urban=="metropolitan"){
    summarized_day_hour <- summarized_day_hour %>%
      filter(RUCA_code2 %in% metro)
  }
  
  if(urban=="not_metropolitan"){
    summarized_day_hour <- summarized_day_hour %>%
      filter(!RUCA_code2 %in% metro)
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
    select(-census_year, -Total_pop, -ICE_black_seg, -ICE_bipoc_seg, -ICE_latinx_seg, -BIPOC) %>% #modify this as needed 
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
    
    plot_densities <- first | second| third
  
  return(plot_densities)
}

plot_densities(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, "cdd_summer", 2010)
plot_densities(Temperatures_NLDAS_summer_avgs, Tract_RaceEthn_Census, "cdd_summer", 2010)



#### Calculate mean differences by year, paired by county ####

run_lmer_tempdisparity <- function(df_county_temp_and_race, temp_measure = c("cdd_summer", "nighttime.cdh_summer")){
  
  state_fips1 <- str_sub(df_county_temp_and_race$County_FIPS[1], 1, 2)
  
  if(state_fips1==11){ #Washington DC only has one county -- so fixed effect for county is removed 
    
        lmer_formula <- as.formula(paste(temp_measure, "~ race + (1|year)"))
    
    }else{
      
        lmer_formula <- as.formula(paste(temp_measure, "~ race + County_FIPS + (1|year)"))
    }
  
  lmer_output <- lmer(lmer_formula, data = df_county_temp_and_race) 
  
  confint_lmer <- as_tibble(confint(lmer_output, c("raceBlack", "raceLatino", "raceBIPOC"), level = 0.95)) %>% 
    rename(lower_ci = "2.5 %",
           upper_ci = "97.5 %")
  
  results <- enframe(fixef(lmer_output)) %>% 
    filter(str_detect(name, "race")) %>% bind_cols(., confint_lmer) %>% mutate(state_fips = as.numeric(state_fips1))
  
  return(results)
}

Calculate_mean_diffs_by_race <- function(temp_model, census_data, temp_measure = c("cdd_summer", "nighttime.cdh_summer")){
  
  temp_model <- Temperatures_XGBoost_summer_avgs
  census_data <- Tract_RaceEthn_Census
  temp_measure = "cdd_summer"
  
  Temperature_w_Censusdata <- temp_model %>%
    left_join(., census_data, by = c("GEOID", "census_year")) %>% 
    select(-starts_with("ICE")) %>%
    pivot_longer(cols = c("Black", "White", "Latino", "Asian"), names_to = "race", values_to = "estimate") %>%
    group_by(County_FIPS, race, year) %>%
    summarise_at(vars(mean_htindx_summer:nighttime.cdh_summer), ~ weighted.mean(., estimate)) %>%
    mutate(race = factor(race, levels = c("White", "Black", "Latino", "Asian")),
      state = str_sub(County_FIPS, 1, 2))
  
  lmer_formula <- as.formula(paste(temp_measure, "~ race + County_FIPS + (1|year)"))
  
  lmer_temp_results <- lmer(lmer_formula, data = Temperature_w_Censusdata)
  
  confint_lmer <- as_tibble(confint(lmer_temp_results, c("raceBlack", "raceLatino", "raceBIPOC"), level = 0.95)) %>% 
    rename(lower_ci = "2.5 %",
           upper_ci = "97.5 %")
  
  results <- enframe(fixef(lmer_temp_results)) %>% 
    filter(str_detect(name, "race")) %>% bind_cols(., confint_lmer) %>% mutate(state_fips = 00)
  
  states <- Temperature_w_Censusdata %>%
    split(.$state) %>%
    map_dfr(., ~run_lmer_tempdisparity(.x, temp_measure), .progress = T)
  
  results1 <- bind_rows(results, states)
  
  return(results1)
  
}

lmer_results_race_meandiffs <- Calculate_mean_diffs_by_race(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, "cdd_summer")

#next

County_CDDs_2012 <- Temperatures_XGBoost_summer_avgs %>%
  filter(year==2012) %>%
  left_join(., Tract_RaceEthn_Census %>% select(GEOID, Total_pop, census_year, State_FIPS, County_FIPS), by = c("GEOID", "census_year")) %>%
  group_by(County_FIPS, State_FIPS) %>%
  summarise(avg_cdd = weighted.mean(cdd_summer, Total_pop))

# tract_2010_shp %>% 
#   select(-Total_pop) %>% 
#   mutate(County_FIPS = str_sub(GEOID, 1, 5)) %>%
#   group_by(County_FIPS) %>%
#   st_union() %>%
#   st_cast() %>%
#   ggplot() + 
#   geom_sf()

county_shp <- get_estimates(geography = "county",
              variables = "RNETMIG",
              year = 2015,
              geometry = TRUE,
              resolution = "20m", 
              state = NEMIA_States)

County_CDDs_2012_shp <- county_shp %>%
  select(-variable, -value) %>%
  left_join(., County_CDDs_2012, by = c("GEOID" = "County_FIPS"))

County_CDDs_2012_shp %>%
  ggplot(aes(fill = avg_cdd)) + 
  geom_sf() +
  tm_facets(by = "State_FIPS")

County_CDDs_2012_shp %>%
tm_shape() +
  tm_polygons(col = 'avg_cdd',
              title = "Avg CDDs",
              style = 'cont') +               
  tm_facets(by = 'State_FIPS', ncol = 1)

create_state_map <- function(df, state){
  
  plot_state <- df %>% 
    filter(State_FIPS == state) %>%
    ggplot(aes(fill = avg_cdd)) + 
    geom_sf() + 
    theme_no_axes() +
    #coord_sf(expand = F) +
    theme(panel.border = element_blank()) +
    scale_fill_gradient2(
      name = "Cooling degree days (F)",
      low = scales::muted("blue"),
      mid = "yellow",
      high = scales::muted("red"),
      midpoint = 890,
      #breaks = c(-20, 0, 20, 40),
      limits = c(197, 1600)) 
  
  return(plot_state)
}


table_lmer_results <- lmer_results_race_meandiffs %>%
  mutate(estimate = paste0(round(value, 1), " (", round(lower_ci,1), "-", round(upper_ci, 1),")")) %>%
    select(name, state_fips, estimate) %>%
    mutate(name = str_remove(name, "race")) %>%
    pivot_wider(names_from = name, values_from = estimate) %>%
    left_join(., get_state_names_from_fips(), by = "state_fips") %>%
    rename("State" = "Name") %>%
    mutate(State = if_else(state_fips==0, "Entire region", State)) %>%
    select(State, Black, Latino, BIPOC)

entire_region_plot <- County_CDDs_2012_shp %>% 
  ggplot(aes(fill = avg_cdd)) + 
  geom_sf() + 
  #theme_no_axes()  +
  scale_fill_gradient2(
    name = "Cooling degree days (F)",
    low = scales::muted("blue"),
    mid = "yellow",
    high = scales::muted("red"),
    midpoint = 890,
    limits = c(197, 1600)) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        legend.position = c(.2, .8),
        legend.key = element_rect(color = "transparent", fill = "transparent"))

entire_region_plot + annotation_custom(gridExtra::tableGrob(table_lmer_results, rows = NULL, 
                                                            theme = ttheme_default(base_size = 9, 
                                                                                   padding = unit(c(1.5,1.5), "mm"))),
                                       xmin=-74.5, xmax=-65, ymin=36, ymax=40) 
# 
# (create_state_map(County_CDDs_2012_shp, 23) / create_state_map(County_CDDs_2012_shp, "33")/ 
#     create_state_map(County_CDDs_2012_shp, 50)/ create_state_map(County_CDDs_2012_shp, 25)/
#     create_state_map(County_CDDs_2012_shp, 44)/ create_state_map(County_CDDs_2012_shp, "09")/
#     create_state_map(County_CDDs_2012_shp, 36)/ create_state_map(County_CDDs_2012_shp, 34)/
#     create_state_map(County_CDDs_2012_shp, 42)/create_state_map(County_CDDs_2012_shp, 54)/
#     create_state_map(County_CDDs_2012_shp, 10)/ create_state_map(County_CDDs_2012_shp, 24)/
#     create_state_map(County_CDDs_2012_shp, 11)/ create_state_map(County_CDDs_2012_shp, 51)) + 
#   plot_layout(guides = 'collect') +
#   plot_layout(ncol = 2) & 
#   theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))


  
## try with LMER





#then state by state?
lmer_temp_cdds_ny <- lmer(cdd_summer ~ race + County_FIPS + (1|year), data = Temperature_w_Censusdata %>% filter(state == "36"))
summary(lmer_temp_cdds_ny)

White_temps <- Temperature_w_Censusdata %>%
  filter(race=="White")

Temperature_w_Censusdata1 <- Temperature_w_Censusdata %>%
  ungroup() %>%
  filter(race != "White") %>%
  left_join(., White_temps, by = c("County_FIPS", "year")) %>%
  mutate(split_var = paste(year, race.x, race.y, sep = "."))

split_names <- Temperature_w_Censusdata1 %>%
  distinct(split_var)

CDD_results <- Temperature_w_Censusdata1 %>%
  split(.$split_var) %>%
  map_dfr(., ~tidy(t.test(.$cdd_summer.x, .$cdd_summer.y, paired = T))) %>% #how to identify them??
  mutate(temp = "cdd") %>%
  bind_cols(., split_names)

# mean_diff_results1 <- mean_diff_results %>%
#   group_by(race, temp_model, temp) %>%
#   mutate_at(vars(estimate, conf.low, conf.high), ~round(., 2)) %>%
#   summarise(min = min(estimate), 
#             max = max(estimate),
#             ci_range = paste0(min(conf.low), "-", max(conf.high))) %>%
#   ungroup() %>%
#   pivot_longer(cols = c("min", "max"), names_to = "estimate_type", values_to = "value") %>%
#   mutate(paired = rep(1:(n()/2),each=2))

# md_cdd <- mean_diff_results1 %>% 
#   filter(temp == "cdd") %>%
#   ggplot(aes(x= value, y= temp_model)) +
#   geom_line(aes(group = paired))+
#   geom_point(size=4) + #aes(color=estimate_type),
#   theme(legend.position="top") +
#   facet_grid(race ~ .) + 
#   theme_light() + 
#   theme(axis.title.y=element_blank(), legend.position = "none",  strip.background = element_blank(),
#         strip.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(1, 0, 1, 0))+
#   xlab(label = "Cooling Degree Days (C°)") 
# 
# md_cdh <- mean_diff_results1 %>% 
#   filter(temp == "nighttime_cdh") %>%
#   ggplot(aes(x= value, y= temp_model)) +
#   geom_line(aes(group = paired))+
#   geom_point(size=4) + #aes(color=estimate_type), 
#   theme(legend.position="top") +
#   facet_grid(race ~ .) + 
#   theme_light() + 
#   theme(axis.title.y=element_blank(), legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(1, .25, 1, 0))+
#   xlab(label = "Nighttime Cooling Degree Hours (C°)") 
# 
# md_heatindx <- mean_diff_results1 %>% 
#   filter(temp == "heat_index") %>%
#   ggplot(aes(x= value, y= temp_model)) +
#   geom_line(aes(group = paired))+
#   geom_point(size=4) + #aes(color=estimate_type), 
#   theme(legend.position="top") +
#   facet_grid(race ~ .) + 
#   theme_light() + 
#   theme(axis.title.y=element_blank(), legend.position = "none", strip.background = element_blank(),
#         strip.text.x = element_blank(), plot.margin = margin(1, 0, 1, 1)) +
#   xlab(label = "Average Heat Index (C°)") 
# 
# md_compiled <- md_heatindx + md_cdd + md_cdh

median_estimates <- mean_diff_results %>%
  group_by(temp, race, temp_model) %>%
  summarise(median_estimate = median(estimate))

mean_diff_results_medianest <- mean_diff_results %>%
  left_join(., median_estimates, by = c("temp", "race", "temp_model")) %>%
  mutate(x1 = if_else(temp_model == "XGBoost", 1.5, 0.5),
         x2 = if_else(temp_model == "XGBoost", 2.5, 1.5))

md_cdd <- mean_diff_results_medianest %>% 
  filter(temp == "cdd") %>%
  ggplot(aes(x = temp_model, y = estimate)) +
  #geom_line(aes(group = paired))+
  geom_sina() +
  theme(legend.position="top") +
  facet_grid(race ~ .) + 
  geom_segment(aes(x = x1, xend = x2, y = median_estimate, yend = median_estimate), color = "red", linetype = 2) + 
  coord_flip() +
  theme_light() + 
  theme(axis.title.y=element_blank(), legend.position = "none", strip.background = element_blank(),
                 strip.text.x = element_blank(), plot.margin = margin(1, 0, 1, 1))+
  ylab(label = "Cooling Degree Days (C°)") 

md_cdh <- mean_diff_results_medianest %>% 
  filter(temp == "nighttime_cdh") %>%
  ggplot(aes(x = temp_model, y = estimate)) +
  #geom_line(aes(group = paired))+
  geom_sina() +
  theme(legend.position="top") +
  facet_grid(race ~ .) + 
  geom_segment(aes(x = x1, xend = x2, y = median_estimate, yend = median_estimate), color = "red", linetype = 2) + 
  coord_flip() +
  theme_light() + 
  theme(axis.title.y=element_blank(), legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(1, .25, 1, 0))+
  ylab(label = "Nighttime Cooling Degree Hours (C°)") 

# md_heatindx <- mean_diff_results1 %>% 
#   filter(temp == "heat_index") %>%
#   ggplot(aes(x= value, y= temp_model)) +
#   geom_line(aes(group = paired))+
#   geom_point(aes(color=estimate_type), size=4) +
#   theme(legend.position="top") +
#   facet_grid(race ~ .) + 
#   theme_light() + 
#   theme(axis.title.y=element_blank(), legend.position = "none", strip.background = element_blank(),
#         strip.text.x = element_blank(), plot.margin = margin(1, 0, 1, 1)) +
#   xlab(label = "Average Heat Index (C°)") 

md_compiled <- md_cdd + md_cdh

md_compiled





#### Look at relationship between segregation and temperature ####

pct_seg <- Tract_RaceEthn_Census %>%
  filter(Total_pop>0) %>%
  mutate_at(vars(Black, Latinx, BIPOC, White), ~./Total_pop) %>%
  rename(pct_bipoc_seg = BIPOC,
         pct_black_seg = Black,
         pct_latinx_seg = Latinx) %>% #pct_white_seg = White
  select(GEOID, pct_black_seg, pct_latinx_seg, pct_bipoc_seg, ICE_black_seg, ICE_bipoc_seg, ICE_latinx_seg, census_year, State_FIPS, County_FIPS) #pct_white_seg,

Temperatures_NLDAS_resseg <- Temperatures_NLDAS %>%
  left_join(., pct_seg, by = c("census_year", "GEOID")) %>%
  mutate(year = factor(year),
         County_FIPS = as.factor(County_FIPS)) %>%
  select(-noaa_cdd, -census_year, -mean_temp_summer) %>%
  pivot_longer(cols = ends_with("seg"), names_to = "res_seg", values_to = "pct") %>%
  pivot_longer(cols = c("cdd_summer", "nighttime.cdh_summer", "mean_htindx_summer"), names_to = "temp_measure", values_to = "temp") %>%
  mutate(model = "NLDAS",
         split = paste(model, res_seg, temp_measure, year, sep = "."))

# Temperatures_NLDAS_resseg %>% filter(temp_measure=="cdd_summer") %>%
# ggplot(aes(x = ICE_black, y = temp)) + geom_point() + geom_smooth(method = "loess")

Temperatures_XGBoost_resseg <- Temperatures_XGBoost %>%
  left_join(., pct_seg, by = c("census_year", "GEOID")) %>%
  mutate(year = as.factor(year),
         County_FIPS = as.factor(County_FIPS)) %>%
  select(-noaa_cdd, -census_year, -mean_temp_summer) %>%
  pivot_longer(cols = ends_with("seg"), names_to = "res_seg", values_to = "pct") %>%
  pivot_longer(cols = c("cdd_summer", "nighttime.cdh_summer", "mean_htindx_summer"), names_to = "temp_measure", values_to = "temp") %>%
  mutate(model = "XGBoost",
         split = paste(model, res_seg, temp_measure, year, sep = ".")) 

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
