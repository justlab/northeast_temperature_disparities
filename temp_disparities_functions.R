
# Temperatures_NLDAS <- read_fst(here("data", "summarized_daily_temp_NLDAS_F.fst")) 
KtoF = function(kelvins) (9/5) * kelvins - 459.67

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

  if(!file.exists("data/ruca_2000.xls")){

    download.file("https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca00.xls?v=543.7",
                  destfile = "data/ruca_2000.xls")

  }

  ruca_2000 <- read_xls("data/ruca_2000.xls") %>%
    rename("GEOID" = "State County Tract Code",
           "RUCA_code1" = "RUCA Primary Code 2000",
           "RUCA_code2" = "RUCA Secondary Code 2000") %>%
    dplyr::select(GEOID, RUCA_code1, RUCA_code2) %>%
    mutate(census_year = 2000)

  return(ruca_2000)
}

get_RUCAs_2010 <- function(){

  if (!file.exists("data/ruca_2010.xlsx")) {

    download.file("https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx?v=543.7",
                  destfile = "data/ruca_2010.xlsx")
  }

  ruca_2010 <- read_xlsx("data/ruca_2010.xlsx", skip = 1) %>%
    rename("GEOID" = "State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)",
           "RUCA_code1" = "Primary RUCA Code 2010",
           "RUCA_code2" = "Secondary RUCA Code, 2010 (see errata)") %>%
    dplyr::select(GEOID, RUCA_code1, RUCA_code2) %>%
    mutate(census_year = 2010)

  return(ruca_2010)
}
 
# #### Data cleaning ####
 
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
# 
# 
get_Census_tract_data <- function(RUCAs_2000, RUCAs_2010){

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
    left_join(., RUCAs_2000, by = c("GEOID", "census_year")) %>%
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
    left_join(., RUCAs_2010, by = c("GEOID", "census_year")) %>%
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
# 
# # Tract_RaceEthn_Census <- get_Census_tract_data() 
# 
# #grab a map to visualize 
# sfc_as_cols <- function(x, geometry, names = c("x","y")) {
#   if (missing(geometry)) {
#     geometry <- sf::st_geometry(x)
#   } else {
#     geometry <- rlang::eval_tidy(enquo(geometry), x)
#   }
#   stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
#   ret <- sf::st_coordinates(geometry)
#   ret <- tibble::as_tibble(ret)
#   stopifnot(length(names) == ncol(ret))
#   x <- x[ , !names(x) %in% names]
#   ret <- setNames(ret,names)
#   dplyr::bind_cols(x,ret)
# }
# 
# ##Functionalize this to get xy coords 
# tract_2000_shp <- get_decennial(geography = "tract",
#                                 variables = c("Total_pop" = "P001001"),
#                                 year = 2000,
#                                 output = "wide",
#                                 geometry = T, 
#                                 state = NEMIA_States) %>%
#   mutate(census_year = 2000) %>%
#   filter(!st_is_empty(.))
# 
# tract_2010_shp <- get_decennial(geography = "tract",
#                                 variables = c("Total_pop" = "P001001"),
#                                 year = 2010,
#                                 output = "wide",
#                                 geometry = T, 
#                                 state = NEMIA_States) %>%
#   mutate(census_year = 2010) %>%
#   filter(!st_is_empty(.)) 
# 
# tract_centroids <- bind_rows(tract_2000_shp, tract_2010_shp) %>%
#   st_centroid() 
# 
# #can potentially remove elevation 
# tract_elevations <- get_elev_point(tract_centroids$geometry, prj = 4326, src = "epqs")
# 
# tract_centroids1 <- bind_cols(tract_centroids, tract_elevations %>% st_drop_geometry(.) %>% dplyr::select(elevation))
# 
# # %>%
# #   sfc_as_cols() %>%
# #   st_drop_geometry() %>%
# #   select(GEOID, x, y, census_year)
# ##
# 
# 
# #### Contour plots ####
# 

create_weighted_race_hours_NEMIA <- function(year, urban = c("metropolitan", "not_metropolitan", "all"), Tract_RaceEthn_Census){
  
  all_pred_filepaths <- list.files("data/hourly_tract_preds", full.names = T)
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

# #### Visualize annual CDD density plots by race/ethnicity faceted by state #### 
 
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
# 
# # plot_state_temps_density(temps_for_ggplot_density, 2010, "23", "cdd_summer", 0, 1100)
# 
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

  first <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, 23, "cdd_summer", 0, 1150) + remove_x)/ #later -- find way to programmatically adjust x axis limits?
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 33, "cdd_summer", 0, 1150)+ remove_x)/
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 50, "cdd_summer", 0, 1150)+ remove_x)/
              (plot_state_temps_density(temps_for_ggplot_density, plot_year, 25, "cdd_summer", 0, 1150)+ remove_x)/
              plot_state_temps_density(temps_for_ggplot_density, plot_year, 44, "cdd_summer", 0, 1150))

  second <- ((plot_state_temps_density(temps_for_ggplot_density, plot_year, "09", "cdd_summer", 95, 1900) + remove_x)/
               (plot_state_temps_density(temps_for_ggplot_density, plot_year, 36, "cdd_summer", 95, 1900)+ remove_x)/
               (plot_state_temps_density(temps_for_ggplot_density, plot_year, 34, "cdd_summer", 95, 1900)+ remove_x)/
               (plot_state_temps_density(temps_for_ggplot_density, plot_year, 42, "cdd_summer", 95, 1900)+ remove_x)/
               plot_state_temps_density(temps_for_ggplot_density, plot_year, 54, "cdd_summer", 95, 1900))

  #below currently trims ~8 rows of data on the lower end of the distribution for XGBoost model
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
# 
# plot_densities(Temperatures_XGBoost_summer_avgs, Tract_RaceEthn_Census, "cdd_summer", 2010)
# plot_densities(Temperatures_NLDAS_summer_avgs, Tract_RaceEthn_Census, "cdd_summer", 2010)
