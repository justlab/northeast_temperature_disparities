

#Define Vector: States and codes
NEMIA_States <- c("09", "23", "25", "33", "44", "50", 
                  "34", "36", "42",
                  "10", "11", "24", "51", "54")

# Read temperatures_xgboost
read_temperatures_xgboost <- function(){
  download.file("https://zenodo.org/records/10557980/files/summarized_daily_temp_preds.parquet?download=1", destfile = here("data","summarized_daily_temp_preds.parquet"))
  read_parquet(here("data", "summarized_daily_temp_preds.parquet")) 
}


#Define Function: Fips to state names
get_state_names_from_fips <- function(){
  
  if(!file.exists(here("data", "census_fips_states.xlsx"))){
    download.file("https://www2.census.gov/programs-surveys/popest/geographies/2017/state-geocodes-v2017.xlsx",
                  destfile = here("data", "census_fips_states.xlsx"))
  }
  state_fips_names <- read_xlsx(here("data", "census_fips_states.xlsx"), skip = 5) %>%
    rename(State_FIPS = "State (FIPS)") %>%
    filter(State_FIPS %in% NEMIA_States) %>%
    select(Name, State_FIPS) %>%
    mutate(Name = if_else(Name == "District of Columbia", "Washington D.C.", Name))
  return(state_fips_names)
}

#Clean and summarize temperatures
KtoF = function(kelvins) (9/5) * kelvins - 459.67

create_cdds_f = function(temp_in_f) {
  cdds_wo_threshold <- temp_in_f - 65
  cdds_w_threshold <- ifelse(cdds_wo_threshold<0, 0, cdds_wo_threshold)
  return(cdds_w_threshold)
}

clean_and_summarize_temperatures <- function(temperature_df){ 
    
  cleaned_temp_df <- temperature_df %>%
    filter(month(date)>=5 & month(date)<=9) %>% 
    mutate(year = year(date),
           noaa_mean = ((max_temp_daily_cK + min_temp_daily_cK)/2)/100, #data in centikelvin
           noaa_mean_cdd = create_cdds_f(KtoF(noaa_mean)),
           hourly_mean = mean_temp_daily_cK/100,
           mean_24hour_cdd = create_cdds_f(KtoF(hourly_mean))) %>%
    group_by(year, GEOID) %>%
    summarise(cdd_summer = sum(mean_24hour_cdd),
              noaa_cdd = sum(noaa_mean_cdd)) %>%
    mutate(census_year = if_else(year<=2009, 2000, 2010)) %>%
    ungroup()
  
  return(cleaned_temp_df)
}

#Obtain Census tract data
get_Census_tract_data <- function(){
  
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
           ICE_hisplatino_seg = (White - Latino) / Total_pop,
           ICE_asian_seg = (White - Asian)/ Total_pop)
  
  return(Tract_RaceEthn_Census)
  
}

#Define Function: get xy coordinates from an sf geometry object 
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

get_tract_xy_and_elevation <- function(){
  tract_2000_shp <- get_decennial(geography = "tract",
                                  variables = c("Total_pop" = "P001001"),
                                  year = 2000,
                                  output = "wide",
                                  geometry = T, 
                                  state = NEMIA_States) %>%
    mutate(census_year = 2000) %>%
    filter(!st_is_empty(.))
  
  tract_2010_shp <- get_decennial(geography = "tract",
                                  variables = c("Total_pop" = "P001001"),
                                  year = 2010,
                                  output = "wide",
                                  geometry = T, 
                                  state = NEMIA_States) %>%
    mutate(census_year = 2010) %>%
    filter(!st_is_empty(.)) 
  
  tract_centroids <- bind_rows(tract_2000_shp, tract_2010_shp) %>%
    st_centroid() %>% 
    sfc_as_cols() %>% 
    rename(longitude = x, latitude = y)
  
  elevations <- get_elev_point(tract_centroids, prj = 4326, src = "aws")
  
  tract_centroids <- cbind(tract_centroids, elevations)
  
  return(tract_centroids)
  
} 

#### Visualize annual CDD density plots by race/ethnicity faceted by state #### 
#Define plot function
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
    geom_density_ridges(data = df1, aes(x = get(temp_measure),
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

plot_densities <- function(temperature_df, census_df, temp_measure, plot_year,fips_to_statename_crosswalk){
  
  temperature_df1 <- temperature_df %>% 
    left_join(., census_df, by = c("census_year", "GEOID")) %>%
    filter(Total_pop>0)
  
  temps_for_ggplot_density <- temperature_df1 %>%
    select(-census_year, -Total_pop, -ICE_black_seg, -ICE_bipoc_seg, -ICE_hisplatino_seg, -ICE_asian_seg, -Other) %>% #modify this as needed 
    pivot_longer(cols = c("Black", "White", "Latino", "Asian"), names_to = "race", values_to = "estimate") %>%
    left_join(., fips_to_statename_crosswalk, by = "State_FIPS") 
  
  
  first_column <- c("23", "33", "50", "25", "44")
  second_column <- c("09", "36", "34", "42", "54")
  third_column <- c("10", "24", "11", "51")
  
  min_and_max_first_column <- temps_for_ggplot_density %>%
    filter(State_FIPS %in% third_column) %>%
    summarise(min_temp = min(get(temp_measure)),
              max_temp = max(get(temp_measure)))
  
  remove_x <- theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.x = element_blank())
  
  first <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, 23, temp_measure, 0, 1150) + remove_x)/ 
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 33, temp_measure, 0, 1200)+ remove_x)/
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 50, temp_measure, 0, 1200)+ remove_x)/
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 25, temp_measure, 0, 1200)+ remove_x)/
              plot_state_temps_density(temps_for_ggplot_density, plot_year, 44, temp_measure, 0, 1200)) 
  
  second <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, "09", temp_measure, 0, 1920) + remove_x)/
               (plot_state_temps_density(temps_for_ggplot_density, plot_year, 36, temp_measure, 0, 1920)+ remove_x)/
               (plot_state_temps_density(temps_for_ggplot_density, plot_year, 34, temp_measure, 0, 1920)+ remove_x)/
               (plot_state_temps_density(temps_for_ggplot_density, plot_year, 42, temp_measure, 0, 1920)+ remove_x)/
               plot_state_temps_density(temps_for_ggplot_density, plot_year, 54, temp_measure, 0, 1920))
  
  #below trims ~8 rows of data on the lower end of the distribution for XGBoost model  
  third <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, 10, temp_measure, 250, 2210) + remove_x)/
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 24, temp_measure, 250, 2210)+ remove_x)/
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 11, temp_measure, 250, 2210)+ remove_x)/
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 51, temp_measure, 250, 2210)) / 
              plot_spacer() + theme(plot.margin = unit(c(15,0,0,0), "pt")))
  
  plot_densities <- (first | second| third) + plot_annotation(
    caption = "Cooling degree days (F)",
    theme = theme(
      plot.caption = element_text(size = 12, hjust = 0.5, face = "bold")
    )
  )
  
  return(plot_densities)
}

create_plot_1 <- function(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, fips_to_statename_crosswalk){
  plot_object <- plot_densities(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, "noaa_cdd", 2010,fips_to_statename_crosswalk = fips_to_statename_crosswalk)
  ggsave(here("paper", "plot1.png"),plot = plot_object, width = 7, height = 6.5, units = "in", dpi = 600)
}

#### Calculate mean differences by year, paired by county ####
make_wec_contrasts_tracts <- function(data, ref = "Other"){
  levels <- levels(data$race)
  index <- which(levels == ref)
  tracts <- data %>% group_by(race) %>% arrange(race) %>% summarise(n=sum(wec_weight))
  contrasts <-  contr.wec(data$race, ref)
  contrasts[index, ] <- -1 * tracts$n[-index]/tracts$n[index]
  return(contrasts)
}

run_feols_tempdisparity <- function(df_county_temp_and_race, temp_measure = c("cdd_summer", "noaa_cdd")){ 
  
  state_fips1 <- str_sub(df_county_temp_and_race$County_FIPS[1], 1, 2)
  contrasts(df_county_temp_and_race$race) <- make_wec_contrasts_tracts(df_county_temp_and_race, "Other")
  
  if(state_fips1==11){ #Washington DC only is one county -- so fixed effect for county is removed 
    
    feols_formula <- as.formula(paste(temp_measure, "~ race|year"))
    
  }else{
    
    feols_formula <- as.formula(paste(temp_measure, "~ race|County_FIPS + year"))
  }
  
  feols_temp_results1 <- feols(feols_formula, data = df_county_temp_and_race, weights = df_county_temp_and_race$estimate, conley(cutoff = 10))  
  
  results <- enframe(coef(feols_temp_results1)) %>%
    left_join(., as_tibble(confint(feols_temp_results1, 
                                   vcov = "conley",
                                   parm = c("raceBlack", "raceLatino", "raceAsian", "raceWhite"), 
                                   level = 0.95, 
                                   coef.col = T)),
              by = c("name" = "coefficient")) %>%
    rename(lower_ci = "2.5 %",
           upper_ci = "97.5 %") %>%
    mutate(State_FIPS = state_fips1,
           name = str_remove(name, "race")) %>%
    mutate(across(c("value", "lower_ci", "upper_ci"), ~round(.x, 1)))
  
  model_name <- paste0("feols_", state_fips1, "_results")
  assign(x = model_name, value = feols_temp_results1, envir = .GlobalEnv)
  
  return(results)
}

run_feols_tempdisparity_state <- function(df_county_temp_and_race, temp_measure = c("cdd_summer", "noaa_cdd")){ 
  state_fips1 <- str_sub(df_county_temp_and_race$County_FIPS[1], 1, 2)
  contrasts(df_county_temp_and_race$race) <- make_wec_contrasts_tracts(df_county_temp_and_race, "Other")
  
  if(state_fips1==11){ #Washington DC only is one county -- so fixed effect for county is removed 
    
    feols_formula <- as.formula(paste(temp_measure, "~ race|year"))
    
  }else{
    
    feols_formula <- as.formula(paste(temp_measure, "~ race|County_FIPS + year"))
  }
  
  feols_temp_results1 <- feols(feols_formula, data = df_county_temp_and_race, weights = df_county_temp_and_race$estimate, conley(cutoff = 10)) 
  
  return(feols_temp_results1)
}


create_meandiff_table <- function(temp_model, census_data, temp_measure = c("cdd_summer", "noaa_cdd"),tract_centroids,fips_to_statename_crosswalk){ 
  
  temp_model1 <<- temp_model 
  census_data1 <<- census_data
  
  Temperature_w_Censusdata <<- temp_model1 %>% 
    left_join(., census_data1, by = c("GEOID", "census_year")) %>% 
    pivot_longer(cols = c("Black", "White", "Latino", "Asian", "Other"), names_to = "race", values_to = "estimate") %>%
    mutate(race = factor(race, levels = c("Other", "Asian", "Black", "Latino", "White"))) %>% 
    group_by(State_FIPS) %>%
    mutate(wec_weight = estimate/Total_pop) %>% 
    ungroup() %>%
    dplyr::select(State_FIPS, year, County_FIPS, all_of(temp_measure), race, estimate, GEOID, RUCA_code2, census_year, urban, RUCA_code1, wec_weight) %>% 
    filter(estimate!=0) %>%
    left_join(., tract_centroids %>% st_drop_geometry() %>% select(GEOID, census_year, longitude, latitude, elevation), by = c("GEOID", "census_year"))
  
  
  states <- Temperature_w_Censusdata %>%
    split(.$State_FIPS) %>%
    map_dfr(., ~run_feols_tempdisparity(.x, temp_measure), .progress = T) %>%
    mutate(val_and_ci = paste0(value, " (", lower_ci, ", ", upper_ci, ")"), .keep = "unused") %>%
    pivot_wider(names_from = "name", values_from = val_and_ci) %>%
    left_join(., fips_to_statename_crosswalk, by = "State_FIPS") %>%
    select(Name, Asian, Black, Latino, White) %>%
    arrange(Name)
  
  rm(temp_model1, census_data1, envir = .GlobalEnv)
  
  return(states)
  
}

create_feols_objects <- function(temp_model, 
                                 census_data,
                                 state_code,
                                 temp_measure = "noaa_cdd",
                                 tract_centroids){ 
  
  temp_model1 <<- temp_model 
  census_data1 <<- census_data
  
  Temperature_w_Censusdata <<- temp_model1 %>% 
    left_join(., census_data1, by = c("GEOID", "census_year")) %>% 
    pivot_longer(cols = c("Black", "White", "Latino", "Asian", "Other"), names_to = "race", values_to = "estimate") %>%
    mutate(race = factor(race, levels = c("Other", "Asian", "Black", "Latino", "White"))) %>% 
    group_by(State_FIPS) %>%
    mutate(wec_weight = estimate/Total_pop) %>% 
    ungroup() %>%
    dplyr::select(State_FIPS, year, County_FIPS, all_of(temp_measure), race, estimate, GEOID, RUCA_code2, census_year, urban, RUCA_code1, wec_weight) %>% 
    filter(estimate!=0) %>%
    left_join(., tract_centroids %>% st_drop_geometry() %>% select(GEOID, census_year, longitude, latitude, elevation), by = c("GEOID", "census_year"))
  
  feols_data <- Temperature_w_Censusdata %>%
    filter(State_FIPS == state_code)
  feols_results <- run_feols_tempdisparity_state(feols_data, temp_measure)
  
  return(feols_results)
  
}

create_table_1 <- function(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census,tract_centroids,fips_to_statename_crosswalk){
  table_data <- create_meandiff_table(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, temp_measure = "noaa_cdd",tract_centroids,fips_to_statename_crosswalk)
  write_csv(table_data, here("paper", "table1.csv"))
  return(table_data)
}

#### Calculating spatial measures of segregation ####
counties_to_join <- tibble::tibble(problem_county = c("51045", "51720", "51017", "51005", "51560", "51580", "51640", "51515", "51530", "51678", "51685", "51610", "51115", "54105"), 
                           merge_with =     c("51775", "51195", "51091", "51091", "51091", "51091", "51035", "51019", "51163", "51163", "51683", "51013", "51073", "54103")) 

calculate_spatial_seg_measure_per_county <- function(county_fip, census_year1) {
  
  state_plane_codes <- read_csv("https://gist.githubusercontent.com/fitnr/10795511/raw/7468f4fca23631019644bd18f1aa5dfc69ed1b1a/county-epsg.csv")
  epsg_county <- (state_plane_codes %>% filter(COUNTYFIPS == county_fip))$EPSG 
  
  #projecting the County "window" in stateplane system 
  specific_county_polygon <- CT_counties_trimmed %>%
    filter(COUNTYFIPS==county_fip) 
  
  st_geometry(specific_county_polygon) <- st_collection_extract(x = st_geometry(specific_county_polygon), 
                                                                type = "POLYGON")
  specific_county_polygon <- st_transform(specific_county_polygon, crs = st_crs(epsg_county))
  
  
  cty_as_owin <- as.owin(as_Spatial(specific_county_polygon)) 
  
  #projecting the centroids in the stateplane system 
  county_2010_popcentroids1 <- CT_popcentroids %>% 
    mutate(COUNTYFIPS = paste0(STATEFP, COUNTYFP)) %>%
    left_join(., counties_to_join, by = c("COUNTYFIPS" = "problem_county")) %>%
    mutate(COUNTYFIPS = if_else(!is.na(merge_with), merge_with, COUNTYFIPS)) %>%
    dplyr::select(-merge_with) %>%
    st_transform(., st_crs(epsg_county)) %>%
    st_filter(specific_county_polygon, .predicate = st_intersects)
  
  county_2010_popcentroids_for_ppp <- tibble::tibble(X = st_coordinates(county_2010_popcentroids1)[,"X"], Y = st_coordinates(county_2010_popcentroids1)[,"Y"])
  
  county_2010_popcentroids2 <- county_2010_popcentroids1 %>%
    bind_cols(., county_2010_popcentroids_for_ppp) %>%
    inner_join(., Statewide_Census_data %>% filter(census_year == census_year1), by = "GEOID") %>%
    st_drop_geometry()
  
  
  total_ppp <- ppp(county_2010_popcentroids2$X, county_2010_popcentroids2$Y, window=cty_as_owin, marks = county_2010_popcentroids2$Total_pop)
  asian_ppp <- ppp(county_2010_popcentroids2$X, county_2010_popcentroids2$Y, window=cty_as_owin, marks = county_2010_popcentroids2$Asian)
  black_ppp <- ppp(county_2010_popcentroids2$X, county_2010_popcentroids2$Y, window=cty_as_owin, marks = county_2010_popcentroids2$Black)
  white_ppp <- ppp(county_2010_popcentroids2$X, county_2010_popcentroids2$Y, window=cty_as_owin, marks = county_2010_popcentroids2$White)
  hisplatino_ppp <- ppp(county_2010_popcentroids2$X, county_2010_popcentroids2$Y, window=cty_as_owin, marks = county_2010_popcentroids2$Latino)
  
  suppressWarnings(kernel_smoothed_total <- Smooth.ppp(total_ppp, at = "points",leaveoneout = T))
  county_sigma <- attr(kernel_smoothed_total, "sigma")
  suppressWarnings(kernel_smoothed_asian <- Smooth.ppp(asian_ppp, at = "points",leaveoneout = T, sigma = county_sigma))
  suppressWarnings(kernel_smoothed_black <- Smooth.ppp(black_ppp, at = "points",leaveoneout = T, sigma = county_sigma))
  suppressWarnings(kernel_smoothed_hisplatino <- Smooth.ppp(hisplatino_ppp, at = "points",leaveoneout = T, sigma = county_sigma))
  suppressWarnings(kernel_smoothed_white <- Smooth.ppp(white_ppp, at = "points",leaveoneout = T, sigma = county_sigma))
  
  
  spatial_seg_measures <- tibble::tibble(black = (kernel_smoothed_black/kernel_smoothed_total)*100,
                                 white = (kernel_smoothed_white/kernel_smoothed_total)*100,
                                 asian = (kernel_smoothed_asian/kernel_smoothed_total)*100,
                                 hisplatino = (kernel_smoothed_hisplatino/kernel_smoothed_total)*100)
  
  spatial_seg_measures1 <- county_2010_popcentroids1 %>% st_drop_geometry() %>% dplyr::select(GEOID, COUNTYFIPS) %>% bind_cols(., spatial_seg_measures)
  
  return(spatial_seg_measures1)
}

calculate_all_resseg_by_state <- function(state_fips, census_year1,tract_centroids,Tract_RaceEthn_Census){ 
  
  
  if(census_year1==2010){
    
    CT_popcentroids <<- read_csv(paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR", state_fips, ".txt")) %>% 
      mutate(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
      st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(4249)) 
    
  }else{
    
    CT_2000_popcentroids <<- read_csv(paste0("https://www2.census.gov/geo/docs/reference/cenpop2000/tract/tract_pop.txt"),
                                      col_names = c("STATEFP", "COUNTYFP", "TRACTCE", "POPULATION", "LATITUDE", "LONGITUDE")) %>%
      mutate(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE),
             LATITUDE = na_if(LATITUDE, "+."),
             LONGITUDE = na_if(LONGITUDE, "-.")) 
    
    centroids_for_missing <- CT_2000_popcentroids %>%
      filter(STATEFP == state_fips) %>%
      filter(is.na(LATITUDE)) %>%
      dplyr::select(STATEFP, COUNTYFP, TRACTCE, POPULATION, GEOID) %>%
      inner_join(., tract_centroids, by = "GEOID") %>%
      rename("LONGITUDE" = "longitude",
             "LATITUDE" = "latitude")
    
    CT_popcentroids <<- CT_2000_popcentroids %>%
      filter(STATEFP == state_fips) %>%
      anti_join(., centroids_for_missing, by = "GEOID") %>%
      filter(!is.na(LATITUDE)) %>%
      st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(4249))
    
  }
  
  Statewide_Census_data <<- Tract_RaceEthn_Census %>%
    filter(census_year==census_year1) %>%
    select(GEOID, Black, White, Asian, Latino, Total_pop, State_FIPS, County_FIPS, census_year)
  
  if(state_fips=="54"|state_fips=="51"){
    
    CT_counties <- counties(state = state_fips, cb = TRUE, year = census_year1) %>%
      mutate(COUNTYFIPS = paste0(STATEFP, COUNTYFP)) %>% 
      left_join(., counties_to_join, by = c("COUNTYFIPS" = "problem_county")) %>%
      mutate(COUNTYFIPS = if_else(!is.na(merge_with), merge_with, COUNTYFIPS)) %>%
      group_by(COUNTYFIPS) %>%
      summarize(geometry = st_combine(geometry)) %>%
      st_make_valid()
    
  }else{
    
    CT_counties <- counties(state = state_fips, cb = TRUE, year = census_year1) %>%  
      mutate(COUNTYFIPS = paste0(STATEFP, COUNTYFP))
  }
  
  CT_counties_trimmed <<- erase_water(CT_counties, area_threshold = .995)
  county_fips_per_state <- unique(CT_counties$COUNTYFIPS)
  
  state_resseg_output <- map_dfr(county_fips_per_state, ~calculate_spatial_seg_measure_per_county(.x, census_year1), .progress = T) %>%
    mutate(census_year = census_year1)
  
  return(state_resseg_output)
}

create_resseg_across_NEMIA_allyears <- function(tract_centroids,Tract_RaceEthn_Census){
  
  All_State_ResSeg_Measures_2000 <- map_dfr(NEMIA_States, ~calculate_all_resseg_by_state(.x, 2000,tract_centroids,Tract_RaceEthn_Census), .progress = T)
  All_State_ResSeg_Measures_2010 <- map_dfr(NEMIA_States, ~calculate_all_resseg_by_state(.x, 2010,tract_centroids,Tract_RaceEthn_Census), .progress = T)
  All_State_ResSeg_Measures <- bind_rows(All_State_ResSeg_Measures_2000, All_State_ResSeg_Measures_2010)
  
  return(All_State_ResSeg_Measures)
  
}

#### Segregation regressions ####
create_df_for_seg_analysis <- function(temp_model, census_data, seg_measures,tract_centroids){
  
  Temperatures_XGBoost_resseg <- temp_model %>%
    left_join(., census_data %>% dplyr::select(census_year, GEOID, State_FIPS, County_FIPS, contains("ICE"),), by = c("census_year", "GEOID")) %>%
    inner_join(., sfc_as_cols(tract_centroids) %>% dplyr::select(-NAME, -Total_pop), by = c("GEOID", "census_year")) %>%
    left_join(., seg_measures, by = c("GEOID", "census_year")) %>%
    mutate(year = as.factor(year), 
           County_FIPS = as.factor(County_FIPS))  
  
  return(Temperatures_XGBoost_resseg)
  
}

make_all_region_seg_bams <- function(Temperatures_w_resseg, knots){
  
  conduct_gam_by_race_and_census_region <- function(region = c("new_england", "mid_atlantic", "south_atlantic"), race = c("asian", "black", "hisplatino", "white"), Temperatures_w_resseg){
    
    if(region=="mid_atlantic"){
      fips_in_region <- c("36", "34", "42")}
    
    if(region=="south_atlantic"){
      fips_in_region <- c("24", "51", "54", "10", "11")}
    
    if(region=="new_england"){
      fips_in_region <- c("23", "25", "50", "09", "44", "33")}
    
    bam_formula <- as.formula(paste0("noaa_cdd ~ s(", race, ", bs = 'cr', fx = T, k = ", knots, ") + County_FIPS + year + 
                         te(x, y, d = 2, by = year, bs = 'gp', m = 2)")) 
    
    bam_model = mgcv::bam(bam_formula, 
                          family = gaussian(), data=Temperatures_w_resseg %>% filter(State_FIPS %in% fips_in_region) %>% st_drop_geometry(), 
                          discrete = F)
    return(bam_model)
  }
  
  
  region_race_for_iteration <- tibble::tibble(region = rep(c("new_england", "mid_atlantic", "south_atlantic"), each = 4), race = rep(c("asian","black", "hisplatino", "white"), 3))
  all_region_seg_bams <- future_map2(region_race_for_iteration$region, region_race_for_iteration$race, ~conduct_gam_by_race_and_census_region(.x, .y, Temperatures_w_resseg))
  names(all_region_seg_bams) <- paste0(region_race_for_iteration$region, "_", region_race_for_iteration$race)
  
  return(all_region_seg_bams)
}

#### Segregation regressions Plots ####
compile_all_bams_for_figure <- function(Temperatures_w_resseg,all_region_seg_bams) {
  
  #Define plot function
  create_bam_smooth_plot <- function(bam_model, race = c("asian", "black", "hisplatino", "white"), region = c("new_england", "mid_atlantic", "south_atlantic"),
                                     yaxis = TRUE, title = TRUE, Temperatures_w_resseg){
    
    if(region=="mid_atlantic"){
      fips_in_region <- c("36", "34", "42")
      region_name <- "Mid Atlantic"
    }
    
    if(region=="south_atlantic"){
      fips_in_region <- c("24", "51", "54", "10", "11")
      region_name <- "South Atlantic"
    }
    
    if(region=="new_england"){
      fips_in_region <- c("23", "25", "50", "09", "44", "33")
      region_name <- "New England"
    }
    
    plot_title <- if_else(race=="white", "White",
                          if_else(race == "asian", "Asian",
                                  if_else(race=="black", "Black", "Latino")))
    
    smooth_object <- smooth_estimates(bam_model, smooth = 1) %>% add_confint(coverage = .9983) 
    
    temp1 <- Temperatures_w_resseg %>% 
      filter(State_FIPS %in% fips_in_region,
             !is.na(get(race))) %>%
      add_partial_residuals(bam_model, select =1) 
    
    smooth = paste0("s(", race, ")")
    
    if(yaxis == TRUE & title == TRUE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        stat_bin_hex(aes(x = get(race), y = get(smooth)), data = temp1, alpha = .8, bins = 50) +
        scale_fill_continuous(low = "#91cbfa", high = "#132B43") + 
        geom_line(aes(y = lower_ci, x = get(race)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(race)), linetype = 2) +
        geom_line(aes(x = get(race), y = est), lwd = .5, alpha = .5) +
        labs(y = as.expression(region_name), title = as.expression(plot_title)) + 
        ylab(region_name) +
        ylim(-400,400) + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              text = element_text(size = 14),
              legend.position = "none")
      
    }
    
    if(yaxis == TRUE & title == FALSE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        stat_bin_hex(aes(x = get(race), y = get(smooth)), data = temp1, alpha = .8, bins = 50) +
        scale_fill_continuous(low = "#91cbfa", high = "#132B43") +
        geom_line(aes(y = lower_ci, x = get(race)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(race)), linetype = 2) +
        geom_line(aes(x = get(race), y = est), lwd = .5, alpha = .5) +
        labs(y = region_name, title = as.expression(plot_title)) + 
        ylim(-400,400) + 
        theme(plot.title = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              text = element_text(size = 14),
              legend.position = "none")
      
    }
    
    
    if(yaxis == FALSE & title == TRUE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        stat_bin_hex(aes(x = get(race), y = get(smooth)), data = temp1, alpha = .8, bins = 50) +
        scale_fill_continuous(low = "#91cbfa", high = "#132B43") +
        geom_line(aes(y = lower_ci, x = get(race)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(race)), linetype = 2) +
        geom_line(aes(x = get(race), y = est), lwd = .5, alpha = .5) +
        labs(y = as.expression(region_name), title = as.expression(plot_title)) + 
        ylab(region_name) +
        ylim(-400,400) + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              text = element_text(size = 14),
              legend.position = "none")
      
    }
    
    if(yaxis == FALSE & title == FALSE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        stat_bin_hex(aes(x = get(race), y = get(smooth)), data = temp1, alpha = .8, bins = 50) +
        scale_fill_continuous(low = "#91cbfa", high = "#132B43") +
        geom_line(aes(y = lower_ci, x = get(race)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(race)), linetype = 2) +
        geom_line(aes(x = get(race), y = est), lwd = .5, alpha = .5) +
        labs(y = region_name, title = as.expression(plot_title)) + 
        ylim(-400,400) + 
        theme(plot.title = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              text = element_text(size = 14),
              legend.position = "none")
      
    }
    
    return(smooth_plot)
  }
  
  p1 <- create_bam_smooth_plot(all_region_seg_bams[[1]], race = "asian", region = "new_england", yaxis = TRUE, title = TRUE, Temperatures_w_resseg) |
    create_bam_smooth_plot(all_region_seg_bams[[2]], race = "black", region = "new_england", yaxis = FALSE, title = TRUE, Temperatures_w_resseg) | 
    create_bam_smooth_plot(all_region_seg_bams[[3]], race = "hisplatino", region = "new_england", yaxis = FALSE, title = TRUE, Temperatures_w_resseg) |
    create_bam_smooth_plot(all_region_seg_bams[[4]], race = "white", region = "new_england", yaxis = FALSE, title = TRUE, Temperatures_w_resseg)  
  
  
  p1row <- wrap_elements(panel = textGrob("New England", gp=gpar(fontsize=20)), clip = T)
  
  p1_total <- wrap_elements(p1row/p1 + plot_layout(nrow = 2, heights = c(.17, 1)))
  
  p2 <- create_bam_smooth_plot(all_region_seg_bams[[5]], race = "asian", region = "mid_atlantic", yaxis = TRUE, title = FALSE, Temperatures_w_resseg) | 
    create_bam_smooth_plot(all_region_seg_bams[[6]], race = "black", region = "mid_atlantic", yaxis = FALSE, title = FALSE, Temperatures_w_resseg) | 
    create_bam_smooth_plot(all_region_seg_bams[[7]], race = "hisplatino", region = "mid_atlantic", yaxis = FALSE, title = FALSE, Temperatures_w_resseg) |
    create_bam_smooth_plot(all_region_seg_bams[[8]], race = "white", region = "mid_atlantic", yaxis = FALSE, title = FALSE, Temperatures_w_resseg) 
  
  p2row <- wrap_elements(textGrob("Mid Atlantic", gp=gpar(fontsize=20)), clip = T)
  
  p2_total <- wrap_elements(p2row/p2 + plot_layout(nrow = 2, heights = c(.2, 1)))
  
  p3 <- create_bam_smooth_plot(all_region_seg_bams[[9]], race = "asian", region = "south_atlantic", yaxis = TRUE, title = FALSE, Temperatures_w_resseg) | 
    create_bam_smooth_plot(all_region_seg_bams[[10]], race = "black", region = "south_atlantic", yaxis = FALSE, title = FALSE, Temperatures_w_resseg) |
    create_bam_smooth_plot(all_region_seg_bams[[11]], race = "hisplatino", region = "south_atlantic", yaxis = FALSE, title = FALSE, Temperatures_w_resseg) | 
    create_bam_smooth_plot(all_region_seg_bams[[12]], race = "white", region = "south_atlantic", yaxis = FALSE, title = FALSE, Temperatures_w_resseg) 
  
  p3row <- wrap_elements(textGrob("South Atlantic", gp=gpar(fontsize=20)), clip = T)
  
  p3_total <- wrap_elements(p3row/p3 + plot_layout(nrow = 2, heights = c(.2, 1)))
  
  yaxis_title <- wrap_elements(textGrob("Partial effect (CDDs [F])", gp = gpar(fontsize = 20, fontface = "bold"), rot = 90), clip = T)
  
  plot <- (yaxis_title|(p1_total / p2_total / p3_total)) + plot_layout(ncol = 2, widths = c(.075, 2)) +
    plot_annotation(caption = "Concentration (%)", theme = theme(plot.caption = element_text(size = 20, hjust = .55, face = "bold"))) & 
    theme(plot.margin = unit(c(.05, .05, .001, .05), "cm"))
  
  
  return(plot)
}

create_plot_2 <- function(Temperatures_w_resseg, all_region_seg_bams, body = TRUE){
  plot2_object <- compile_all_bams_for_figure(Temperatures_w_resseg,all_region_seg_bams)
  
  if(body==TRUE){
    ggsave(here("paper", "plot2.pdf"), device = "pdf", plot = plot2_object, width = 11, height = 9.5, units = "in")
  }else{
    ggsave(here("paper", "supp_plot_2knots.pdf"), device = "pdf", plot = plot2_object, width = 11, height = 9.5, units = "in")
  }
  
  return(plot2_object)
}

# Sensitivity and supplemental analyses ####
create_supp_table_1 <- function(Temperatures_XGBoost_summer_avgs){
  cdds_per_year_data <- Temperatures_XGBoost_summer_avgs %>%
    mutate(State_FIPS = str_sub(GEOID, 1,2)) %>%
    group_by(State_FIPS, year) %>%
    summarise(mean_noaa_cdds = round(mean(noaa_cdd, na.rm = TRUE), 0)) %>%
    pivot_wider(names_from = year, values_from = mean_noaa_cdds)
  
  write_csv(cdds_per_year_data, here("paper", "supp_table1.csv"))
  return(cdds_per_year_data)
}



make_all_states_seg_bams <- function(Temperatures_w_resseg){
  
  #Function for gam
  conduct_gam_by_race_state <- function(race = c("asian", "black", "hisplatino", "white"), state_fips= c("09", "23", "25", "33", "44", "50", "34", "36", "42", "10", "11", "24", "51", "54"),Temperatures_w_resseg){
    
    
    if(state_fips!="11"){
      bam_formula <- as.formula(paste0("noaa_cdd ~ s(", race, ", bs = 'cr', k = 3) + County_FIPS + year + 
                         te(x, y, d = 2, by = year, bs = 'gp', m = 2)")) 
    }
    
    if(state_fips=="11"){#remove county fips for dc since not applicable 
      bam_formula <- as.formula(paste0("noaa_cdd ~ s(", race, ", bs = 'cr', k = 3) + year + 
                         te(x, y, d = 2, by = year, bs = 'gp', m = 2)"))
    }
    
    bam_model = mgcv::bam(bam_formula, 
                          family = gaussian(), data=Temperatures_w_resseg %>% filter(State_FIPS==state_fips), 
                          discrete = F) 
    
    return(bam_model)
  }
  
  state_race_for_iteration <- data.frame(state = rep(NEMIA_States, each = 4), race = rep(c("asian", "black", "hisplatino", "white"), 14))
  all_states_seg_bams <- map2(.x = state_race_for_iteration$race, .y = state_race_for_iteration$state, ~conduct_gam_by_race_state(.x, .y,Temperatures_w_resseg), .progress = T)
  names(all_states_seg_bams) <- paste0("bam_", state_race_for_iteration$race, "_", state_race_for_iteration$state)
  
  return(all_states_seg_bams)
}

#### Visualizing state specific regressions ####
get_df_w_partialresids  <- function(model_name){
  
  state_fip <- str_sub(model_name, -2)
  
  state_data <- df_to_iterate %>%
    filter(State_FIPS == state_fip)
  
  df_for_plotting <- state_data %>%
    add_partial_residuals(., subset_state_bams[[model_name]], select = 1)
  
  return(df_for_plotting)
}

create_state_seg_bam_smooth_plot <- function(bam_model, ethnorace = c("asian", "black", "hisplatino", "white"), states, yaxis = TRUE, legend = TRUE, color_set = c("Set1", "Set2"), region,Temperatures_w_resseg){ #yaxis = TRUE, title = TRUE
  
  state_code_to_abbreviation <- tibble::tibble(State_FIPS = c("09", "10", "11", "23", "24", "25", "33", "34", "36", "42", "44", "50", "51", "54"),
                                               State = c("CT", "DE", "DC", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT", "VA", "WV"))
  #Define model smooth functions
  get_model_smooths <- function(bam_model, model_name){
    
    model_smooth <- smooth_estimates(bam_model, smooth = 1) %>% 
      add_confint(coverage = .95) %>% mutate(name = model_name)
    
    return(model_smooth)
  }
  
  state_race_for_iteration <- data.frame(state = rep(NEMIA_States, each = 4), race = rep(c("asian", "black", "hisplatino", "white"), 14))
  df_to_iterate <<- Temperatures_w_resseg %>%
    filter(State_FIPS %in% states,
           !is.na(get(ethnorace))) %>%
    left_join(., state_code_to_abbreviation, by = "State_FIPS")
  
  bams_to_plot <- state_race_for_iteration %>% filter(race==ethnorace & state %in% states) %>%
    mutate(bam_name = paste0("bam_", race, "_", state))
  
  subset_state_bams <<- bam_model[bams_to_plot$bam_name]
  smooths_for_select_states <- map2_dfr(.x = subset_state_bams, .y = bams_to_plot$bam_name, ~get_model_smooths(.x, .y)) %>%
    mutate(State_FIPS = str_sub(name, -2)) %>%
    left_join(., state_code_to_abbreviation, by = "State_FIPS")
  
  df_to_plot_partialresids <- bams_to_plot %>%
    split(.$state) %>%
    map_dfr(., ~get_df_w_partialresids(.$bam_name))
  
  plot_title <- if_else(ethnorace=="white", "White",
                        if_else(ethnorace == "asian", "Asian",
                                if_else(ethnorace=="black", "Black", "Latino")))
  
  smooth = paste0("s(", ethnorace, ")")
  
  if(yaxis == TRUE & legend == FALSE){
    
    smooth_plot <- smooths_for_select_states %>%
      ggplot() +
      theme_bw() +
      geom_line(aes(x = get(ethnorace), y = est, color = State), lwd = .5, alpha = .5) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = get(ethnorace), color = State), alpha = .2) + 
      geom_point(aes(x = get(ethnorace), y = get(smooth), color = State), data = df_to_plot_partialresids, alpha = .0) +
      labs(y = as.expression(region), title = as.expression(plot_title)) + 
      ylab("Partial effect (CDDs)") +
      ylim(-200,200) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            text = element_text(size = 15),
            legend.position = "none") +
      scale_color_brewer(type = "qual", palette = color_set)
    smooth_plot1 <- ggMarginal(smooth_plot, groupColour = TRUE, groupFill = TRUE, margins = "both") 
  }
  if(yaxis == FALSE & legend == FALSE){
    
    
    smooth_plot <- smooths_for_select_states %>%
      ggplot() +
      theme_bw() +
      geom_line(aes(x = get(ethnorace), y = est, color = State), lwd = .5, alpha = .5) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = get(ethnorace), color = State), alpha = .2) + 
      geom_point(aes(x = get(ethnorace), y = get(smooth), color = State), data = df_to_plot_partialresids, alpha = .0) +
      labs(y = as.expression(region), title = as.expression(plot_title)) + 
      ylab("Partial effect (CDDs)") +
      ylim(-200,200) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size = 15),
            legend.position = "none") +
      scale_color_brewer(type = "qual", palette = color_set)
    smooth_plot1 <- ggMarginal(smooth_plot, groupColour = TRUE, groupFill = TRUE, margins = "both") 
    
  }
  
  if(yaxis == FALSE & legend == TRUE){
    
    
    smooth_plot <- smooths_for_select_states %>%
      ggplot() +
      theme_bw() +
      geom_line(aes(x = get(ethnorace), y = est, color = State), lwd = .5, alpha = .5) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = get(ethnorace), color = State), alpha = .2) + 
      geom_point(aes(x = get(ethnorace), y = get(smooth), color = State), data = df_to_plot_partialresids, alpha = .0) +
      labs(y = as.expression(region), title = as.expression(plot_title)) + 
      ylab("Partial effect (CDDs)") +
      ylim(-200,200) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size = 15),
            legend.position = "right") + 
      scale_color_brewer(type = "qual", palette = color_set)
    
    smooth_plot_legend <- get_legend(smooth_plot)
    
    smooth_plot_a<- smooth_plot + theme(legend.position = "none")
    
    smooth_plot_b <- ggMarginal(smooth_plot_a, groupColour = TRUE, groupFill = TRUE, margins = "both")
    
    smooth_plot1 <- wrap_elements(smooth_plot_b) + smooth_plot_legend + plot_layout(ncol =2, widths = c(5,1))
  }
  
  return(smooth_plot1)
  
}

plot_supp_fig_states_seg_regressions <- function(states1, states2, regions,Temperatures_w_resseg,all_states_seg_bams){
  
  states1_plot <- wrap_elements(wrap_elements(create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "asian", states = states1, legend = F, color_set = "Set1", region=regions[1],
                                                                               Temperatures_w_resseg = Temperatures_w_resseg)) +
                                  wrap_elements(create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "black", states = states1, yaxis = F, legend = F, color_set = "Set1", region=regions[1],
                                                                                 Temperatures_w_resseg = Temperatures_w_resseg)) +
                                  wrap_elements(create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "hisplatino", states = states1, yaxis = F, legend = F, color_set = "Set1", region=regions[1],
                                                                                 Temperatures_w_resseg = Temperatures_w_resseg)) +
                                  create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "white", states = states1, yaxis = F, legend = T, color_set = "Set1", region=regions[1],
                                                                   Temperatures_w_resseg = Temperatures_w_resseg) +
                                  plot_layout(ncol =4, nrow = 1)) 
  
  if(!is.null(states2)){
    states2_plot <- wrap_elements(wrap_elements(create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "asian", states = states2, yaxis = T, legend = F, color_set = "Set2", region=regions[2],
                                                                                 Temperatures_w_resseg = Temperatures_w_resseg)) +
                                    wrap_elements(create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "black", states = states2, yaxis = F, legend = F, color_set = "Set2", region=regions[2],
                                                                                   Temperatures_w_resseg = Temperatures_w_resseg)) +
                                    wrap_elements(create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "hisplatino", states = states2, yaxis = F, legend = F, color_set = "Set2", region=regions[2],
                                                                                   Temperatures_w_resseg = Temperatures_w_resseg)) +
                                    create_state_seg_bam_smooth_plot(bam_model = all_states_seg_bams, ethnorace = "white", states = states2, yaxis = F, legend = T, color_set = "Set2", region=regions[2],
                                                                     Temperatures_w_resseg = Temperatures_w_resseg) + 
                                    plot_layout(ncol =4, nrow = 1)) 
    
    states_seg_plots <- states1_plot/states2_plot + plot_annotation(caption = "Ethnoracial concentration (%)", 
                                                                    theme = theme(plot.caption = element_text(hjust = 0.5, size = 15)))
    
  }else{
    
    states_seg_plots <- states1_plot + plot_annotation(caption = "Ethnoracial concentration (%)", 
                                                       theme = theme(plot.caption = element_text(hjust = 0.5, size = 15)))
    
  }
  
  return(states_seg_plots)
  
}

create_supp_fig_states_seg_regression_plots <- function(Temperatures_w_resseg,all_states_seg_bams){
  # Plot 1
  plot1 <- plot_supp_fig_states_seg_regressions(
    states1 = c("23", "33", "50"),
    states2 = c("25", "44", "09"),
    regions = c("New England", "New England"),
    Temperatures_w_resseg = Temperatures_w_resseg,
    all_states_seg_bams = all_states_seg_bams
  )
  ggsave(here("paper", "supp_state_plot_1.png"), plot = plot1, width = 10, height = 8, units = "in")
  
  # Plot 2
  plot2 <- plot_supp_fig_states_seg_regressions(
    states1 = c("36", "34", "42"),
    states2 = c("10", "24", "54"),
    regions = c("Mid Atlantic", "South Atlantic"),
    Temperatures_w_resseg = Temperatures_w_resseg,
    all_states_seg_bams = all_states_seg_bams
  )
  ggsave(here("paper", "supp_state_plot_2.png"), plot = plot2, width = 10, height = 8, units = "in")
  
  # Plot 3
  plot3 <- plot_supp_fig_states_seg_regressions(
    states1 = c("11", "51"),
    states2 = NULL,
    regions = c("South Atlantic", "South Atlantic"),
    Temperatures_w_resseg = Temperatures_w_resseg,
    all_states_seg_bams = all_states_seg_bams
  )
  ggsave(here("paper", "supp_state_plot_3.png"), plot = plot3, width = 10, height = 5, units = "in")
  
  # Optionally return the plots as a list if you might want to inspect them later.
  list(plot1, plot2, plot3)
  
}

#### ICE regional regressions ####
make_all_region_ice_bams <- function(Temperatures_w_resseg){
  
  #ICE GAM function
  conduct_ice_gam_by_race_and_census_region <- function(region = c("new_england", "mid_atlantic", "south_atlantic"), race = c("asian", "black", "hisplatino", "bipoc"),Temperatures_w_resseg){
    
    if(region=="mid_atlantic"){
      fips_in_region <- c("36", "34", "42")}
    
    if(region=="south_atlantic"){
      fips_in_region <- c("24", "51", "54", "10", "11")}
    
    if(region=="new_england"){
      fips_in_region <- c("23", "25", "50", "09", "44", "33")}
    
    ice_var <- paste0("ICE_", race, "_seg")
    
    bam_formula <- as.formula(paste0("noaa_cdd ~ s(", ice_var, ", bs = 'cr', fx = T, k = 3) + County_FIPS + year + 
                         te(x, y, d = 2, by = year, bs = 'gp', m = 2)")) 
    
    bam_model = mgcv::bam(bam_formula, 
                          family = gaussian(), data=Temperatures_w_resseg %>% filter(State_FIPS %in% fips_in_region) %>% st_drop_geometry(), 
                          discrete = F)
    
    return(bam_model)
  }
  
  
  region_race_for_ice_iteration <- tibble::tibble(region = rep(c("new_england", "mid_atlantic", "south_atlantic"), each = 4), race = rep(c("asian","black", "hisplatino", "bipoc"), 3))
  all_region_ice_bams <- future_map2(region_race_for_ice_iteration$region, region_race_for_ice_iteration$race, ~conduct_ice_gam_by_race_and_census_region(.x, .y,Temperatures_w_resseg))
  names(all_region_ice_bams) <- paste0(region_race_for_ice_iteration$region, "_", region_race_for_ice_iteration$race)
  
  return(all_region_ice_bams)
}


all_ice_bam_plots <- function(all_region_ice_bams,Temperatures_w_resseg) {
  
  #Define plot function
  create_ice_bam_smooth_plot <- function(bam_model, race = c("asian", "black", "hisplatino", "bipoc"), region = c("new_england", "mid_atlantic", "south_atlantic"),
                                         yaxis = TRUE, title = TRUE){
    
    if(region=="mid_atlantic"){
      fips_in_region <- c("36", "34", "42")
      region_name <- "Mid Atlantic"
    }
    
    if(region=="south_atlantic"){
      fips_in_region <- c("24", "51", "54", "10", "11")
      region_name <- "South Atlantic"
    }
    
    if(region=="new_england"){
      fips_in_region <- c("23", "25", "50", "09", "44", "33")
      region_name <- "New England"
    }
    
    
    plot_title <- if_else(race=="bipoc", "All except white",
                          if_else(race == "asian", "Asian",
                                  if_else(race=="black", "Black", "Latino")))
    
    smooth_object <- smooth_estimates(bam_model, smooth = 1) %>% add_confint(coverage = .9983) #bam_model replacement here
    
    ice_var <- paste0("ICE_", race, "_seg")
    
    temp1 <- Temperatures_w_resseg %>% 
      filter(State_FIPS %in% fips_in_region,
             !is.na(get(ice_var))) %>%
      add_partial_residuals(bam_model, select =1) 
    
    smooth = paste0("s(", ice_var, ")")
    
    if(yaxis == TRUE & title == TRUE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        geom_rug(aes(x = get(ice_var)),
                 data = temp1,
                 sides = "b", length = grid::unit(0.01, "npc")) +
        stat_bin_hex(aes(x = get(ice_var), y = get(smooth)), data = temp1, alpha = .8, bins = 40) +
        scale_fill_continuous(low = "#56B1F7", high = "#132B43") +
        geom_line(aes(y = lower_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(x = get(ice_var), y = est), lwd = .5, alpha = .5) +
        labs(y = as.expression(region_name), title = as.expression(plot_title)) + 
        ylab(region_name) +
        ylim(-400,400) + 
        xlim(-1,1) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.x = element_blank(),
              text = element_text(size = 15),
              legend.position = "none")
      
    }
    
    if(yaxis == TRUE & title == FALSE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        geom_rug(aes(x = get(ice_var)),
                 data = temp1,
                 sides = "b", length = grid::unit(0.01, "npc")) +
        stat_bin_hex(aes(x = get(ice_var), y = get(smooth)), data = temp1, alpha = .8, bins = 40) +
        scale_fill_continuous(low = "#56B1F7", high = "#132B43") +
        geom_line(aes(y = lower_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(x = get(ice_var), y = est), lwd = .5, alpha = .5) +
        labs(y = region_name, title = as.expression(plot_title)) + 
        ylim(-400,400) + 
        xlim(-1,1) +
        theme(plot.title = element_blank(),
              axis.title.x = element_blank(),
              text = element_text(size = 15),
              legend.position = "none")
      
    }
    
    if(yaxis == FALSE & title == TRUE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        geom_rug(aes(x = get(ice_var)),
                 data = temp1,
                 sides = "b", length = grid::unit(0.01, "npc")) +
        stat_bin_hex(aes(x = get(ice_var), y = get(smooth)), data = temp1, alpha = .8, bins = 40) +
        scale_fill_continuous(low = "#56B1F7", high = "#132B43") +
        geom_line(aes(y = lower_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(x = get(ice_var), y = est), lwd = .5, alpha = .5) +
        labs(y = as.expression(region_name), title = as.expression(plot_title)) + 
        ylab(region_name) +
        ylim(-400,400) + 
        xlim(-1,1) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              text = element_text(size = 15),
              legend.position = "none")
      
    }
    
    if(yaxis == FALSE & title == FALSE){
      
      smooth_plot <- smooth_object %>%
        ggplot() +
        theme_bw() +
        geom_rug(aes(x = get(ice_var)),
                 data = temp1,
                 sides = "b", length = grid::unit(0.01, "npc")) +
        stat_bin_hex(aes(x = get(ice_var), y = get(smooth)), data = temp1, alpha = .8, bins = 40) +
        scale_fill_continuous(low = "#56B1F7", high = "#132B43") +
        geom_line(aes(y = lower_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(y = upper_ci, x = get(ice_var)), linetype = 2) +
        geom_line(aes(x = get(ice_var), y = est), lwd = .5, alpha = .5) +
        labs(y = region_name, title = as.expression(plot_title)) + 
        ylim(-400,400) + 
        xlim(-1,1) +
        theme(plot.title = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              text = element_text(size = 15),
              legend.position = "none")
      
    }
    
    return(smooth_plot)
  }
  
  #Generate plots
  plot <- create_ice_bam_smooth_plot(all_region_ice_bams[[1]], race = "asian", region = "new_england", yaxis = TRUE, title = TRUE) +
    create_ice_bam_smooth_plot(all_region_ice_bams[[2]], race = "black", region = "new_england", yaxis = FALSE, title = TRUE) + 
    create_ice_bam_smooth_plot(all_region_ice_bams[[3]], race = "hisplatino", region = "new_england", yaxis = FALSE, title = TRUE) +
    create_ice_bam_smooth_plot(all_region_ice_bams[[4]], race = "bipoc", region = "new_england", yaxis = FALSE, title = TRUE) + 
    create_ice_bam_smooth_plot(all_region_ice_bams[[5]], race = "asian", region = "mid_atlantic", yaxis = TRUE, title = FALSE) + 
    create_ice_bam_smooth_plot(all_region_ice_bams[[6]], race = "black", region = "mid_atlantic", yaxis = FALSE, title = FALSE) + 
    create_ice_bam_smooth_plot(all_region_ice_bams[[7]], race = "hisplatino", region = "mid_atlantic", yaxis = FALSE, title = FALSE) +
    create_ice_bam_smooth_plot(all_region_ice_bams[[8]], race = "bipoc", region = "mid_atlantic", yaxis = FALSE, title = FALSE) + 
    create_ice_bam_smooth_plot(all_region_ice_bams[[9]], race = "asian", region = "south_atlantic", yaxis = TRUE, title = FALSE) + 
    create_ice_bam_smooth_plot(all_region_ice_bams[[10]], race = "black", region = "south_atlantic", yaxis = FALSE, title = FALSE) +
    create_ice_bam_smooth_plot(all_region_ice_bams[[11]], race = "hisplatino", region = "south_atlantic", yaxis = FALSE, title = FALSE) + 
    create_ice_bam_smooth_plot(all_region_ice_bams[[12]], race = "bipoc", region = "south_atlantic", yaxis = FALSE, title = FALSE) +
    plot_layout(ncol = 4, nrow = 3) + 
    plot_annotation(
      caption = "Index of Concentration at the Extremes by Census tract",
      theme = theme(plot.caption = element_text(hjust = 0.5, size = 20))
    )
  
  return(plot)
}

create_ice_bam_plots <- function(all_region_ice_bams,Temperatures_w_resseg){
  plot_object <- all_ice_bam_plots(all_region_ice_bams,Temperatures_w_resseg)
  ggsave(here("paper", "supp_ice_bam_plots.png"), plot = plot_object, width = 11, height = 8, units = "in", dpi = 600)
}

#Time series plots
create_plot_of_predictions <- function(state_code, county_code, county_name, xaxislabel = F, yaxislabel = F,Temperatures_XGBoost_summer_avgs,Tract_RaceEthn_Census,tract_centroids){
  
  #make time series plots 
  new_data  <- tibble(year=rep(c(2003:2019), each = 5), race = rep(c("Black", "White", "Latino", "Asian", "Other"), 17))
  
  new_data1 <- new_data %>%
    mutate(State_FIPS = state_code,
           County_FIPS = county_code)
  
  feols_object <- paste0("feols_", state_code, "_results")
  feols_object <- create_feols_objects(temp_model = Temperatures_XGBoost_summer_avgs, 
                                       census_data = Tract_RaceEthn_Census,
                                       state_code = state_code,
                                       temp_measure = "noaa_cdd",
                                       tract_centroids)
  
  new_preds <- predictions(feols_object, newdata = new_data1) %>% select(year, County_FIPS, race, estimate, conf.low, conf.high)
  new_data2 <- new_data1 %>%
    left_join(., new_preds, by = c("County_FIPS", "year", "race"))
  
  county_mean <- new_data2 %>% filter(race == "Other") %>%
    rename(county_mean = "estimate") %>%
    select(year, County_FIPS, county_mean)
  
  new_data_to_plot <- new_data2 %>%
    left_join(., county_mean, by = c("year", "County_FIPS")) %>%
    filter(race != "Other") %>%
    mutate(pct_diff = (estimate - county_mean)/county_mean,
           pct_diff_low = (conf.low - county_mean)/county_mean,
           pct_diff_high = (conf.high - county_mean)/county_mean)
  
  
  if(xaxislabel == TRUE & yaxislabel == FALSE){
    
    plot <- ggplot(new_data_to_plot, aes(x = year)) + 
      geom_ribbon(aes(ymin = pct_diff_low, ymax = pct_diff_high, group = race), alpha=0.1) + 
      geom_line(aes(y = pct_diff, color = race)) + 
      theme_minimal(base_size = 12) + 
      scale_x_continuous(breaks = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019), expand = c(0,0)) +
      ylab("Percent difference from county mean") + 
      ggtitle(county_name) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust=1, face = "bold"),
            axis.title = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 13),
            legend.position = "right",
            plot.margin = unit(c(0, 0, 0, 0),"inches")) + 
      scale_y_continuous(labels = scales::percent)
  }
  
  if(xaxislabel == FALSE & yaxislabel == TRUE){
    
    plot <- ggplot(new_data_to_plot, aes(x = year)) + 
      geom_ribbon(aes(ymin = pct_diff_low, ymax = pct_diff_high, group = race), alpha=0.1) + 
      geom_line(aes(y = pct_diff, color = race)) + 
      theme_minimal(base_size = 12) + 
      scale_x_continuous(breaks = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019), expand = c(0,0)) +
      ylab("Percent difference from county mean") + 
      ggtitle(county_name) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_text(size = 14),
            legend.title = element_blank(),
            legend.text = element_text(size = 13),
            legend.position = "right",
            plot.margin = unit(c(0, 0, 0, 0),"inches")) + 
      scale_y_continuous(labels = scales::percent)
    
  }
  
  if(xaxislabel == FALSE & yaxislabel == FALSE){
    
    plot <- ggplot(new_data_to_plot, aes(x = year)) + 
      geom_ribbon(aes(ymin = pct_diff_low, ymax = pct_diff_high, group = race), alpha=0.1) + 
      geom_line(aes(y = pct_diff, color = race)) + 
      theme_minimal(base_size = 12) + 
      scale_x_continuous(breaks = c(2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019), expand = c(0,0)) +
      ylab("Percent difference from county mean") + 
      ggtitle(county_name) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 13),
            legend.position = "right",
            plot.margin = unit(c(0, 0, 0, 0),"inches")) + 
      scale_y_continuous(labels = scales::percent)
    
    
  }
  return(plot)
}

plot_nemia_county_maps <- function(state_codes_to_plot, county_codes_to_plot, nemia_states_sf){
  
  nemia_counties_sf <- counties(state = NEMIA_States) %>%
    mutate(countyfips = paste0(STATEFP, COUNTYFP)) 
  
  nemiawide_state1 <- ggplot() +
    geom_sf(data = nemia_states_sf, fill = "white") + 
    geom_sf(data = nemia_states_sf %>% filter(STATEFP==state_codes_to_plot), fill = "gray") +
    geom_sf(data = nemia_counties_sf %>% filter(countyfips == county_codes_to_plot), fill = "red") +
    theme(axis.line = element_line(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.border = element_rect(color = "black", fill=NA, linewidth = 1),
          plot.margin = unit(c(0, 0, 0, 0),"inches")) 
  
  nemia_state1_counties <- ggplot() + 
    geom_sf(data = nemia_counties_sf %>% filter(STATEFP == state_codes_to_plot), fill = "gray") + 
    geom_sf(data = nemia_counties_sf %>% filter(countyfips == county_codes_to_plot), fill = "red") + 
    theme_minimal() +
    theme(axis.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45))
  
  map_layout <- '
ABB
'
  map_plot <- nemiawide_state1 + nemia_state1_counties + plot_layout(widths = c(1, 2.2))
  
  return(map_plot)
}



create_figure_of_timeseries <- function(state_codes_to_plot, county_codes_to_plot, county_names_to_plot,Temperatures_XGBoost_summer_avgs,Tract_RaceEthn_Census,tract_centroids){
  
  nemia_states_sf <- states() %>%
    filter(STATEFP %in% NEMIA_States)
  
  map1 <- plot_nemia_county_maps(state_codes_to_plot[1], county_codes_to_plot[1],nemia_states_sf)
  map2 <- plot_nemia_county_maps(state_codes_to_plot[2], county_codes_to_plot[2],nemia_states_sf)
  map3 <- plot_nemia_county_maps(state_codes_to_plot[3], county_codes_to_plot[3],nemia_states_sf)
  
  plot1 <- create_plot_of_predictions(state_code = state_codes_to_plot[1], county_code = county_codes_to_plot[1], county_names_to_plot[1], yaxislabel = F,
                                      Temperatures_XGBoost_summer_avgs = Temperatures_XGBoost_summer_avgs,
                                      Tract_RaceEthn_Census = Tract_RaceEthn_Census,
                                      tract_centroids = tract_centroids)
  plot2 <- create_plot_of_predictions(state_code = state_codes_to_plot[2], county_code = county_codes_to_plot[2], county_names_to_plot[2], yaxislabel = T,
                                      Temperatures_XGBoost_summer_avgs = Temperatures_XGBoost_summer_avgs,
                                      Tract_RaceEthn_Census = Tract_RaceEthn_Census,
                                      tract_centroids = tract_centroids)
  plot3 <- create_plot_of_predictions(state_code = state_codes_to_plot[3], county_code = county_codes_to_plot[3], county_names_to_plot[3], xaxislabel = T,
                                      Temperatures_XGBoost_summer_avgs = Temperatures_XGBoost_summer_avgs,
                                      Tract_RaceEthn_Census = Tract_RaceEthn_Census,
                                      tract_centroids = tract_centroids)
  
  layout <- '
  ABBB
  CDDD
  EFFF
  '
  plot <- wrap_plots(A = map1, B = plot1, C = map2, D = plot2, E = map3, F = plot3, design = layout) + plot_layout(guides = "collect")
  
  return(plot)
}

create_timeseries_plots <- function(Temperatures_XGBoost_summer_avgs,Tract_RaceEthn_Census,tract_centroids){
  plot_object <- create_figure_of_timeseries(state_codes_to_plot = c("23", "34", "54"), 
                                             county_codes_to_plot = c("23005", "34021", "54039"),
                                             county_names_to_plot = c("A. Cumberland County, ME", "B. Mercer County, NJ", "C. Kanawha County, WV"),
                                             Temperatures_XGBoost_summer_avgs = Temperatures_XGBoost_summer_avgs,
                                             Tract_RaceEthn_Census = Tract_RaceEthn_Census,
                                             tract_centroids)
  ggsave(here("paper", "timeseries_plot.png"), plot = plot_object, width = 10, height = 7, units = "in", dpi = 600)
}
