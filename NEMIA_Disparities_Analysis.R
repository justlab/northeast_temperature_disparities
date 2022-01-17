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

#### Pull in data ####
# Temperatures <- read_rds(here("/data-coco/NEMIA_temperature/cdh/cdh_tractsSF_2019_06-08.rds")) #to erase
Temperatures_XGBoost <- read_fst(here("data", "summarized_daily_temp_preds_F.fst")) 
Temperatures_NLDAS <- read_fst(here("data", "summarized_daily_temp_NLDAS_F.fst")) 
KtoF = function(kelvins) (9/5) * kelvins - 459.67
NEMIA_States <- c("09", "23", "25", "33", "44", "50", 
                  "34", "36", "42",
                  "10", "11", "24", "51", "54")

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
           BIPOC = Total_pop - White,
           ICE_black_seg = (White - Black) / Total_pop,
           ICE_bipoc_seg = (White - BIPOC) / Total_pop,
           ICE_latinx_seg = (White - Latino) / Total_pop)
  
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
    dplyr::select(year, month, hour, day_of_yr, w_temp, Black, White, Latino) %>% #change this for racial groups of interest
    pivot_longer(cols = c("Black", "White", "Latino"), names_to = "race", values_to = "estimate") %>% #and this
    group_by(month, year, hour, day_of_yr, race) %>%
    summarise(monthhour_temp = matrixStats::weightedMedian(w_temp, estimate)) %>%
    mutate(temp_f = KtoF(monthhour_temp),
           date = as.Date(paste0(year,"-",day_of_yr), "%Y-%j")) %>% #try to make the y axis prettier
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

make_contour_plot_by_race_and_development <- function(year_to_plot){
  
  plot_metro <- make_contour_plot_by_race(race_hour_temps_NEMIA_metro, year_to_plot) +
    ggtitle("Metropolitan") + 
    theme(axis.title.y = element_text("Hour of day", angle = 90),
          plot.title = element_text(hjust = 0.5, vjust = 0.1)) 
  
  plot_nonmetro <- make_contour_plot_by_race(race_hour_temps_NEMIA_notmetro, year_to_plot) + 
    ggtitle("Micropolitan and Rural") + 
    theme(axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  plot_metro_and_non <- (plot_metro + plot_nonmetro) + plot_layout(guides = 'collect') & 
    #plot_annotation(title = paste("Air temperature by development type in", year_to_plot))  
    theme(legend.position = 'bottom', plot.title = element_text(size = 14)) 

return(plot_metro_and_non)
  
}

make_contour_plot_by_race_and_development(2011)

##leftoff here on 2022-01-16


#### Visualize annual CDD density plots by race/ethnicity #### 
plot_densities <- function(temperature_df, census_df, temp_measure, start_year, end_year){
  
  temperature_df1 <- temperature_df %>%
    left_join(., census_df, by = c("census_year", "GEOID")) %>%
    filter(Total_pop>0)
  
  temps_for_ggplot_density <- temperature_df1 %>%
    select(-census_year, -Total_pop, -ICE_black_seg, -ICE_bipoc_seg, -ICE_latinx_seg, -Latinx, -BIPOC) %>% #modify this as needed 
    pivot_longer(cols = c("Black", "White"), names_to = "race", values_to = "estimate") #, "Latinx", "BIPOC"
  
  plot_densities <- temps_for_ggplot_density %>%
    filter(year >= start_year & year <= end_year) %>%
    ggplot(.) +
    geom_density(aes(x = get(temp_measure), fill = race, weights=estimate/sum(estimate)), alpha = .4) +
    scale_fill_manual(values = c("#4053d3", "#ddb310")) + 
    facet_grid(year~.) + 
    xlab("Cooling Degree Days") +
    theme_minimal() +
    theme(text = element_text(size = 16)) +
    xlim(c(0,950)) #10750
  
  return(plot_densities)
}

medians_for_XGBoost <- Temperatures_XGBoost %>%
  left_join(., Tract_RaceEthn_Census, by = c("census_year", "GEOID")) %>%
  filter(Total_pop>0) %>%
  select(-census_year, -Total_pop, -ICE_black_seg, -ICE_bipoc_seg, -ICE_latinx_seg, -Latinx, -BIPOC) %>% #modify this as needed 
  pivot_longer(cols = c("Black", "White"), names_to = "race", values_to = "estimate") %>%
  group_by(race, year) %>%
  summarise_at(vars(mean_temp_summer:noaa_cdd),  ~matrixStats::weightedMedian(., estimate)) 

medians_for_NLDAS <- Temperatures_NLDAS %>%
  left_join(., Tract_RaceEthn_Census, by = c("census_year", "GEOID")) %>%
  filter(Total_pop>0) %>%
  select(-census_year, -Total_pop, -ICE_black_seg, -ICE_bipoc_seg, -ICE_latinx_seg, -Latinx, -BIPOC) %>% #modify this as needed 
  pivot_longer(cols = c("Black", "White"), names_to = "race", values_to = "estimate") %>%
  group_by(race, year) %>%
  summarise_at(vars(mean_temp_summer:noaa_cdd),  ~matrixStats::weightedMedian(., estimate, na.rm = T)) 



dens_xgboost <- plot_densities(Temperatures_XGBoost, Tract_RaceEthn_Census, "cdd_summer", 2003, 2003)
dens_nldas <- plot_densities(Temperatures_NLDAS, Tract_RaceEthn_Census, "cdd_summer", 2003, 2003)

(dens_xgboost / dens_nldas) + plot_layout(ncol = 1)

plot_avg_annaul_temp <- function(temperature_df, census_df, EIA_regions, 
                                 temp_measure= c("cdd_summer", "mean_temp_summer", "mean_htindx_summer", "nighttime.cdh_summer", "noaa_cdd"), 
                                 FUN,
                                 region = c("Middle Atlantic", "South Atlantic", "New England")){
  
  # temperature_df <- Temperatures_NLDAS
  # census_df <- Tract_RaceEthn_Census
  # FUN <- matrixStats::weightedMedian
  # temp_measure <- "cdd_summer"
  
  temperature_df1 <- temperature_df %>%
    left_join(., census_df, by = c("census_year", "GEOID")) %>%
    left_join(., EIA_regions, by = "State_FIPS") %>%
    filter(Total_pop>0)
  
  median_annual_cdd_summer <- temperature_df1 %>%
    select(-census_year, -Total_pop, -ICE_black_seg, -ICE_bipoc_seg, -ICE_latinx_seg, -Latinx, -BIPOC) %>%
    pivot_longer(cols = c("Black", "White"), names_to = "race", values_to = "estimate") #, "Latinx", "BIPOC"
  
  median_annual_cdd_summer1 <- median_annual_cdd_summer %>%
    group_by(race, year) %>% #, Climate_Region
    summarise(avg_temp = FUN(x = get(temp_measure), w = estimate, na.rm = T)) %>% #what are the NAs here???
    ungroup() 
  # %>%
  #   filter(Climate_Region==region)
  
  table <- median_annual_cdd_summer1 %>%
    group_by(year) %>% #, Climate_Region
    pivot_wider(names_from = race, values_from = avg_temp) %>%
    mutate_at(vars(Black), ~round((((.-White)/White)*100),1)+100) %>% #vars(Black, Latinx, BIPOC)
    #filter(Climate_Region==region) %>%
    ungroup() %>%
    select(-White) %>% #, -Climate_Region
    pivot_longer(-year) 
  
  plot1 <- ggplot() +
    geom_point(data = median_annual_cdd_summer1, aes(x = year, y = avg_temp, color = race), shape = "-", size = 15) +
    geom_line(data = median_annual_cdd_summer1, aes(x = year, y = avg_temp, color = race)) + 
    scale_x_continuous(n.breaks = 17) +
    #ylab(as.expression(temp_measure)) + 
    scale_color_manual(values = c("#4053d3", "#FF5733")) + 
    ylab("Nighttime Cooling Degree\nHours (median)") +
    #ylab("Cooling degree days (median)") +
    ylim(c(3000, 9000)) +#c(300, 850)
    theme_minimal() + 
    theme(text = element_text(size = 16), panel.grid.minor.x = element_blank(), axis.title.x = element_blank())
  
  table_plot <- ggplot(data = table, aes(x = year, y = factor(name, levels = c("Black")))) + #levels = c("Black", "BIPOC", "Latinx")
    geom_tile(fill = "white", alpha = .4, color = "black") +
    geom_text(aes(label = paste0(value, "%"))) +
    scale_x_discrete(expand = expansion(mult = c(0, 0))) +
    labs(y = "", x = NULL) +
    theme_minimal() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(),
          panel.grid = element_blank(), strip.text = element_blank(), panel.spacing.x = unit(0, "mm"))
  
  combined_plot <- (plot1 / table_plot) +  plot_layout(ncol = 1, heights = c(8, 1))
  
  return(combined_plot)
  
}

midAtl_cdd_NLDAS <- plot_avg_annaul_temp(Temperatures_NLDAS, Tract_RaceEthn_Census, EIA_regions, "cdd_summer", matrixStats::weightedMedian,
                     "Middle Atlantic")
midAtl_cdd_XGBoost <- plot_avg_annaul_temp(Temperatures_XGBoost, Tract_RaceEthn_Census, EIA_regions, "cdd_summer", matrixStats::weightedMedian,
                     "Middle Atlantic")

midAtl_cdd_NLDAS / midAtl_cdd_XGBoost + plot_layout(nrow = 4)

midAtl_cdh_NLDAS <- plot_avg_annaul_temp(Temperatures_NLDAS, Tract_RaceEthn_Census, EIA_regions, "nighttime.cdh_summer", matrixStats::weightedMedian,
                     "Middle Atlantic")
midAtl_cdh_XGBoost <- plot_avg_annaul_temp(Temperatures_XGBoost, Tract_RaceEthn_Census, EIA_regions, "nighttime.cdh_summer", matrixStats::weightedMedian,
                     "Middle Atlantic")

midAtl_cdh_NLDAS / midAtl_cdh_XGBoost + plot_layout(nrow=4)

plot_avg_annaul_temp(Temperatures_NLDAS, Tract_RaceEthn_Census, EIA_regions, "nighttime.cdh_summer", matrixStats::weightedMedian,
                     "New England")
plot_avg_annaul_temp(Temperatures_XGBoost, Tract_RaceEthn_Census, EIA_regions, "nighttime.cdh_summer", matrixStats::weightedMedian,
                     "New England")

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

#### Calculate mean differences by year, paired by county ####


Calculate_mean_diffs_by_race <- function(temp_model, census_data){
  
  Temperature_w_Censusdata <- temp_model %>%
    left_join(., census_data, by = c("GEOID", "census_year")) %>% 
    select(-starts_with("ICE")) %>%
    pivot_longer(cols = c("Black", "White", "Latinx", "BIPOC"), names_to = "race", values_to = "estimate") %>%
    group_by(County_FIPS, race, year) %>%
    summarise_at(vars(mean_htindx_summer:nighttime.cdh_summer), ~ weighted.mean(., estimate)) 
  
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
  
  CDH_results <- Temperature_w_Censusdata1 %>%
    split(.$split_var) %>%
    map_dfr(., ~tidy(t.test(.$nighttime.cdh_summer.x, .$nighttime.cdh_summer.y, paired = T))) %>% #how to identify them??
    mutate(temp = "nighttime_cdh") %>%
    bind_cols(., split_names)
  
  HtIndx_results <- Temperature_w_Censusdata1 %>%
    split(.$split_var) %>%
    map_dfr(., ~tidy(t.test(.$mean_htindx_summer.x, .$mean_htindx_summer.y, paired = T))) %>% #how to identify them??
    mutate(temp = "heat_index") %>%
    bind_cols(., split_names)
  
  results <- bind_rows(CDD_results, CDH_results, HtIndx_results)
  
  return(results)
  
}

mean_diff_results <- bind_rows(tibble(Calculate_mean_diffs_by_race(Temperatures_NLDAS, Tract_RaceEthn_Census) %>%
            mutate(temp_model = "NLDAS-2")),
          tibble(Calculate_mean_diffs_by_race(Temperatures_XGBoost, Tract_RaceEthn_Census) %>%
            mutate(temp_model = "XGBoost"))) %>%
  mutate(race = str_extract(split_var, "BIPOC|Black|Latinx"))

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
#### Analysis with CDC WONDER CVD deaths ####

acs5_2009 <- tidycensus::load_variables(2009, "acs5")

get_pop_counties_65plus <- function(year){

  ACS_data_tract_race_65plus <- get_acs(geography = "tract", ##this is not done -- need asian and update non-hispanic black
                                        variables = c("Total_m_65_66" = "B01001_020",
                                                      "Total_m_67_69" = "B01001_021",
                                                      "Total_m_70_74" = "B01001_022",
                                                      "Total_m_75_79" = "B01001_023",
                                                      "Total_m_80_84" = "B01001_024",
                                                      "Total_m_84_up" = "B01001_025",
                                                      "Total_f_65_66" = "B01001_044",
                                                      "Total_f_67_69" = "B01001_045",
                                                      "Total_f_70_74" = "B01001_046",
                                                      "Total_f_75_79" = "B01001_047",
                                                      "Total_f_80_84" = "B01001_048",
                                                      "Total_f_84_up" = "B01001_049",
                                                      "White_m_65_74" = "B01001H_014",
                                                      "White_m_75_84" = "B01001H_015",
                                                      "White_m_85_up" = "B01001H_016",
                                                      "White_f_65_74" = "B01001H_029",
                                                      "White_f_75_84" = "B01001H_030",
                                                      "White_f_85_up" = "B01001H_031",
                                                      "Black_m_65_74" = "B01001B_014",
                                                      "Black_m_75_84" = "B01001B_015",
                                                      "Black_m_85_up" = "B01001B_016",
                                                      "Black_f_65_74" = "B01001B_029",
                                                      "Black_f_75_84" = "B01001B_030",
                                                      "Black_f_85_up" = "B01001B_031",
                                                      "Latin_m_65_74" = "B01001I_014",
                                                      "Latin_m_75_84" = "B01001I_015",
                                                      "Latin_m_85_up" = "B01001I_016",
                                                      "Latin_f_65_74" = "B01001I_029",
                                                      "Latin_f_75_84" = "B01001I_030",
                                                      "Latin_f_85_up" = "B01001I_031"),
                                        year = year,
                                        survey = "acs5",
                                        output = "tidy",
                                        state = NEMIA_States)

  ACS_data_county_race_65plus <- ACS_data_tract_race_65plus %>%
    mutate(variable = str_sub(variable, 1,5),
           State_FIPS = str_sub(GEOID, 1, 2),
           County_FIPS = str_sub(GEOID, 1, 5)) %>%
    group_by(State_FIPS, County_FIPS, variable) %>%
    summarise(Population = sum(estimate)) %>%
    ungroup() %>%
    mutate(year = year)
  return(ACS_data_county_race_65plus)

}

pop_counties_65plus_peryer <- lapply(seq.int(2009,2017), get_pop_counties_65plus)
pop_counties_65plus_peryer <- bind_rows(pop_counties_65plus_peryer)
#pop_counties_65plus_peryer <- pop_counties_65plus_peryer %>%
  #mutate(variable = if_else(variable == "Total", "Total Population", variable))

get_pop_tracts_65plus <- function(year){ ##this is redundant -- doing tracts to counties above, and redownloading tracts here. Should just download once and transform to counties.
  
  ACS_data_tract_race_65plus <- get_acs(geography = "tract", 
                                        variables = c("Total_m_65_66" = "B01001_020",
                                                      "Total_m_67_69" = "B01001_021",
                                                      "Total_m_70_74" = "B01001_022",
                                                      "Total_m_75_79" = "B01001_023",
                                                      "Total_m_80_84" = "B01001_024",
                                                      "Total_m_84_up" = "B01001_025",
                                                      "Total_f_65_66" = "B01001_044",
                                                      "Total_f_67_69" = "B01001_045",
                                                      "Total_f_70_74" = "B01001_046",
                                                      "Total_f_75_79" = "B01001_047",
                                                      "Total_f_80_84" = "B01001_048",
                                                      "Total_f_84_up" = "B01001_049",
                                                      "White_m_65_74" = "B01001H_014",
                                                      "White_m_75_84" = "B01001H_015",
                                                      "White_m_85_up" = "B01001H_016",
                                                      "White_f_65_74" = "B01001H_029",
                                                      "White_f_75_84" = "B01001H_030",
                                                      "White_f_85_up" = "B01001H_031",
                                                      "Black_m_65_74" = "B01001B_014",
                                                      "Black_m_75_84" = "B01001B_015",
                                                      "Black_m_85_up" = "B01001B_016",
                                                      "Black_f_65_74" = "B01001B_029",
                                                      "Black_f_75_84" = "B01001B_030",
                                                      "Black_f_85_up" = "B01001B_031",
                                                      "Latin_m_65_74" = "B01001I_014",
                                                      "Latin_m_75_84" = "B01001I_015",
                                                      "Latin_m_85_up" = "B01001I_016",
                                                      "Latin_f_65_74" = "B01001I_029",
                                                      "Latin_f_75_84" = "B01001I_030",
                                                      "Latin_f_85_up" = "B01001I_031"),
                                        year = year,
                                        survey = "acs5",
                                        output = "tidy",
                                        state = NEMIA_States)
  
  ACS_data_tract_race_65plus1 <- ACS_data_tract_race_65plus %>%
    mutate(variable = str_sub(variable, 1,5),
           State_FIPS = str_sub(GEOID, 1, 2),
           County_FIPS = str_sub(GEOID, 1, 5),
           year = year) %>%
    group_by(GEOID, year, State_FIPS, County_FIPS, variable) %>%
    summarise(Population = sum(estimate)) %>%
    ungroup()
  
  return(ACS_data_tract_race_65plus1)
  
}

pop_tracts_65plus_peryer <- lapply(seq.int(2009,2017), get_pop_tracts_65plus)
pop_tracts_65plus_peryer <- bind_rows(pop_tracts_65plus_peryer)

### SUMMARY of why the below cleaning/merging situation is currently so confusing. tidycensus is producing 2000 census tract geographies for a subset of tracts. Super weird. 
## temp models switch from 2000 CTract geographies to 2010 as they should, and FAQSD PM and O3 come in only 2010 geographies. Need to make a cleaner 
## Used a Census Tract relationship file to translate 2000 to 2010 geographies when appropriate. But need to create much more parsimonious code. For sure. Giving up on 2009 currently
## to do 2010-2017. 

# Merge tract temp and air pollution estimates with tract race/ethnicity and aggregate to counties 
XGBoost_and_Race_byCounty <- pop_tracts_65plus_peryer %>%
  filter(year>2009) %>%
  left_join(., Temperatures_XGBoost, by = c("GEOID", "year")) %>%
  left_join(., air_pollution_summaries, by = c("GEOID", "year")) %>%
  group_by(County_FIPS, variable, year) %>%
  summarise_at(vars(mean_temp_summer:nighttime.cdh_summer, mean_O3_summer, mean_PM_summer), ~ weighted.mean(., Population)) %>%
  arrange(year, County_FIPS) %>%
  mutate_at(vars(mean_temp_summer:nighttime.cdh_summer, mean_O3_summer, mean_PM_summer), ~ifelse(is.nan(.), NA, .))

NLDAS_and_Race_byCounty <- pop_tracts_65plus_peryer %>%
  filter(year>2009) %>%
  left_join(., Temperatures_NLDAS, by = c("GEOID", "year")) %>%
  left_join(., air_pollution_summaries, by = c("GEOID", "year")) %>%
  group_by(County_FIPS, variable, year) %>%
  summarise_at(vars(mean_temp_summer:nighttime.cdh_summer, mean_O3_summer, mean_PM_summer), ~ weighted.mean(., Population)) %>%
  arrange(year, County_FIPS) %>%
  mutate_at(vars(mean_temp_summer:nighttime.cdh_summer, mean_O3_summer, mean_PM_summer), ~ifelse(is.nan(.), NA, .))

#Merge CVD deaths and ACS County data
XGBoost_WONDER_CVD_Mortality_65plus_wCensus <- WONDER_CVD_Mortality_65plus %>%
  filter(year>2009) %>% #for now since Census geographies are messed up
  inner_join(., pop_counties_65plus_peryer, by = c("County_FIPS",
                                                   "Race" = "variable",
                                                   "year")) %>%
  mutate(Deaths = na_if(Deaths, "Suppressed"),
         deaths = as.numeric(Deaths),
         crude_cvd_mortality = (deaths/Population) * 100000) %>%
  dplyr::select(-Deaths) %>%
  left_join(., annual_pop_density, by = c("County_FIPS" = "GEOID", "year")) %>%
  mutate(pop_density = as.numeric(pop_density)) %>%
  left_join(., XGBoost_and_Race_byCounty, by = c("County_FIPS",
                                              "Race" = "variable", 
                                              "year")) %>%
  left_join(., SVI, by = "County_FIPS") %>%
  filter(SVI_quantile >= 0)

NLDAS_WONDER_CVD_Mortality_65plus_wCensus <- WONDER_CVD_Mortality_65plus %>%
  filter(year>2009) %>% #for now since Census geographies are messed up
  inner_join(., pop_counties_65plus_peryer, by = c("County_FIPS",
                                                   "Race" = "variable",
                                                   "year")) %>%
  mutate(Deaths = na_if(Deaths, "Suppressed"),
         deaths = as.numeric(Deaths),
         crude_cvd_mortality = (deaths/Population) * 100000) %>%
  dplyr::select(-Deaths) %>%
  left_join(., annual_pop_density, by = c("County_FIPS" = "GEOID", "year")) %>%
  mutate(pop_density = as.numeric(pop_density)) %>%
  left_join(., NLDAS_and_Race_byCounty, by = c("County_FIPS",
                                                 "Race" = "variable", 
                                                 "year")) %>%
  left_join(., SVI, by = "County_FIPS") %>%
  filter(SVI_quantile >= 0)

#plot some relationships for adjustments 
XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>%
  filter(Race == "Total") %>%
  ggplot(aes(x = log(pop_density), y = log(crude_cvd_mortality))) + 
  geom_point() +
  geom_smooth(method = "lm")

XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>%
  filter(Race == "Total") %>%
  ggplot(aes(x = mean_O3_summer, y = log(crude_cvd_mortality))) + 
  geom_point() +
  geom_smooth(method = "lm")

NLDAS_WONDER_CVD_Mortality_65plus_wCensus %>%
  filter(Race == "Total") %>%
  ggplot(aes(x = mean_PM_summer, y = log(crude_cvd_mortality))) + 
  geom_point() +
  geom_smooth(method = "lm")

XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>%
  filter(Race == "Total") %>%
  ggplot(aes(x = cdd_summer, y = log(crude_cvd_mortality))) + 
  geom_point() +
  geom_smooth(method = "lm")

XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>%
  filter(Race == "Total") %>%
  ggplot(aes(x = nighttime.cdh_summer, y = log(crude_cvd_mortality))) + 
  geom_point() +
  geom_smooth(method = "lm")

XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>%
  filter(Race == "Total") %>%
  ggplot(aes(x = mean_htindx_summer, y = log(crude_cvd_mortality))) + 
  geom_point() +
  geom_smooth(method = "lm")

XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>%
  filter(Race == "Total") %>%
  ggplot(aes(x = SVI_quantile, y = log(crude_cvd_mortality))) + 
  geom_point() +
  geom_smooth(method = "lm")

#censored proportions
XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>%
  group_by(Race) %>%
  summarise(censored = (sum(is.na(deaths))/n())*100)

#### Running Censored Poissons ####
clean_vglm_estimates <- function(model, temp_measure){
  
  model_name <- deparse(substitute(model))
  
  conf_int <- confintvglm(model, "get(temp_measure)", method = "profile")
  
  temp_estimates <- tibble(estimate = coef(model)[2],
            conf_low = conf_int[1],
            conf_high = conf_int[2],
            population = model_name)
  
  return(temp_estimates)
}

##Finish this function to run the models 
run_censored_poissons <- function(df, temp_measure= c("cdd_summer", "mean_temp_summer", "mean_htindx_summer", "nighttime.cdh_summer")){
  
  # df <- NLDAS_WONDER_CVD_Mortality_65plus_wCensus
  #df <- XGBoost_WONDER_CVD_Mortality_65plus_wCensus
  # temp_measure <- "mean_htindx_summer"
  
  df_for_censored_pois <- df %>%
    mutate(lcensored = if_else(is.na(deaths), TRUE, FALSE),
           status = if_else(lcensored==TRUE, 0, 1),
           cy = if_else(is.na(deaths), 9, deaths),
           # race_dummy = as.factor(Race))
           race_dummy = as.factor(Race))
  
  cens.pois.total <- vglm(SurvS4(cy, status, type = "left") ~ get(temp_measure) + mean_O3_summer + SVI_quantile + log(pop_density) + as.factor(year) + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                    data = subset(df_for_censored_pois, Race == "Total" & Population > 0), trace = F)

  cens.pois.black <- vglm(SurvS4(cy, status, type = "left") ~ get(temp_measure) + mean_O3_summer + SVI_quantile + log(pop_density) + as.factor(year) + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                          data = subset(df_for_censored_pois, Race == "Black" & Population >0), trace = F)
  
  cens.pois.white <- vglm(SurvS4(cy, status, type = "left") ~ get(temp_measure) + mean_O3_summer + SVI_quantile + log(pop_density) + as.factor(year) + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                          data = subset(df_for_censored_pois, Race == "White" & Population >0), trace = F)
  
  
  results_vglms <- bind_rows(clean_vglm_estimates(cens.pois.total, temp_measure),
            clean_vglm_estimates(cens.pois.black, temp_measure),
            clean_vglm_estimates(cens.pois.white, temp_measure)) %>%
    mutate(temp = temp_measure)
  
  return(results_vglms)
}

XGBoost_WONDER_CVD_Mortality_65plus_wCensus %>% filter(Race == "Total") %>% summarise(night_cdh_quant_25th = quantile(nighttime.cdh_summer, .25, na.rm = T),
                                                                                      night_cdh_quant_75th = quantile(nighttime.cdh_summer, .75, na.rm = T),
                                                                                      cdd_quant_25th = quantile(cdd_summer, .25, na.rm = T),
                                                                                      cdd_quant_75th = quantile(cdd_summer, .75, na.rm = T))

  
XGBoost_poisson_results <- bind_rows(run_censored_poissons(XGBoost_WONDER_CVD_Mortality_65plus_wCensus, "nighttime.cdh_summer"),
          run_censored_poissons(XGBoost_WONDER_CVD_Mortality_65plus_wCensus, "cdd_summer"),
          run_censored_poissons(XGBoost_WONDER_CVD_Mortality_65plus_wCensus, "mean_htindx_summer")) %>%
  mutate(model = "XGBoost")

NLDAS_poisson_results <- bind_rows(run_censored_poissons(NLDAS_WONDER_CVD_Mortality_65plus_wCensus, "nighttime.cdh_summer"),
          run_censored_poissons(NLDAS_WONDER_CVD_Mortality_65plus_wCensus, "cdd_summer"),
          run_censored_poissons(NLDAS_WONDER_CVD_Mortality_65plus_wCensus, "mean_htindx_summer")) %>%
  mutate(model = "NLDAS-2")

poisson_results <- bind_rows(XGBoost_poisson_results, NLDAS_poisson_results) %>%
  filter(temp != "mean_htindx_summer" & population != "cens.pois.total") %>% #did not converge
  mutate(Race = str_extract(population, "black|white|total"),
         estimate = if_else(temp=="nighttime.cdh_summer", exp(estimate*2679), exp(estimate*260)),#*3417,*335 #could have just done mutate_at
         conf_low = if_else(temp=="nighttime.cdh_summer", exp(conf_low*2679), exp(conf_low*260)),
         conf_high = if_else(temp=="nighttime.cdh_summer", exp(conf_high*2679), exp(conf_high*260)),
         temp = if_else(temp=="nighttime.cdh_summer", "Nighttime Cooling Degree Hours", "Cooling Degree Days"),
         Race = if_else(Race == "black", "Black", "White")) #iqr increases 


poisson_results %>%
ggplot(., aes(color = model)) + 
  geom_point(aes(x = Race, 
                 y = estimate, shape = model), size = 4, position = position_dodge(width = 1/2)) + 
  geom_linerange(aes(x = Race, 
                     ymin = conf_low,
                     ymax = conf_high), position = position_dodge(width = 1/2)) + 
  facet_grid(.~temp, scales = "free") +
  theme_light() +
  theme(text=element_text(size=18), axis.text.x = element_text(angle = 0, vjust = .75, hjust = .75), axis.title.x=element_blank(), axis.line.y = element_blank()) +
  labs(color="Temperature\nmodel", shape = "Temperature\nmodel") +
  ylab("Estimate (Rate Ratio)") +
  geom_hline(aes(yintercept = 1), linetype = 2)
  
  # ylab("Nighttime Cooling Degree Hours (C°)") + 
  # labs(color = "Segregation\nMeasure", shape = "Segregation\nMeasure")


  
  
  
  
  
  
  
#filter(!is.na(Population) & !is.nan(xgboost_avg_cdds)) ##double check this is okay because unsure why some are NA -- small subset though

# Non_merging_counties <- WONDER_CVD_Mortality_65plus_wCensus %>% filter(is.na(mean_temp_summer)) %>% distinct(County_FIPS)
# 
# corrected_broken_tract_GEOIDS <- pop_tracts_65plus_peryer %>%
#   ungroup() %>%
#   mutate(State_FIPS = str_sub(GEOID, 1,2),
#          County_FIPS = str_sub(GEOID, 1,5)) %>%
#   filter(County_FIPS %in% Non_merging_counties$County_FIPS & year != 2009) %>%
#   inner_join(., tract_2000_to_2010_links, by = c("GEOID" = "GEOID_00")) %>%
#   mutate(estimate1 = round(estimate * (PctPop_2000/100), 0)) %>%
#   select(GEOID_10, estimate1, variable, year, County_FIPS) %>%
#   rename(GEOID = "GEOID_10",
#          estimate = "estimate1")

# View(pop_tracts_65plus_peryer %>%
#   ungroup() %>%
#   mutate(State_FIPS = str_sub(GEOID, 1,2),
#          County_FIPS = str_sub(GEOID, 1,5)) %>%
#   filter(County_FIPS %in% Non_merging_counties$County_FIPS & year == 2009) %>%
#     inner_join(., Temperatures_re, by = c("GEOID", "year")) %>%
#     inner_join(., tract_2000_to_2010_links, by = c("GEOID" = "GEOID_00")))

## Okay -- kinda starting over until I figure out a better workflow. 

Temp_and_Race_byTract <- pop_tracts_65plus_peryer %>%
  ungroup() %>%
  mutate(State_FIPS = str_sub(GEOID, 1,2),
         County_FIPS = str_sub(GEOID, 1,5)) %>%
  anti_join(., Non_merging_counties, by = "County_FIPS") %>%
  bind_rows(., corrected_broken_tract_GEOIDS) %>%
  inner_join(., Temperatures_re, by = c("GEOID", "year")) %>%
  inner_join(., air_pollution_summaries, by = c("GEOID", "year")) %>%
  left_join(., EIA_regions, by = "State_FIPS") 

Temp_and_Race_byCounty <- Temp_and_Race_byTract %>%
  group_by(County_FIPS, variable, year) %>%
  summarise_at(vars(mean_temp_summer:mean_PM_summer), ~ weighted.mean(., estimate)) %>%
  arrange(year, County_FIPS) %>%
  mutate_at(vars(mean_temp_summer:mean_PM_summer), ~ifelse(is.nan(.), NA, .)) %>%
  ungroup() %>%
  group_by(year, County_FIPS) %>%
  tidyr::fill(mean_temp_summer:mean_PM_summer, .direction = "updown")

#Merge CVD deaths and ACS County data
WONDER_CVD_Mortality_65plus_wCensus <- WONDER_CVD_Mortality_65plus %>%
  inner_join(., pop_counties_65plus_peryer, by = c("County_FIPS",
                                                   "Race" = "variable",
                                                   "year")) %>%
  mutate(Deaths = na_if(Deaths, "Suppressed"),
         deaths = as.numeric(Deaths),
         crude_cvd_mortality = (deaths/Population) * 100000) %>%
  dplyr::select(-Deaths) %>%
  left_join(., annual_pop_density, by = c("County_FIPS" = "GEOID", "year")) %>%
  left_join(., Temp_and_Race_byCounty, by = c("County_FIPS",
                                              "Race" = "variable", 
                                              "year")) %>%
  filter(year>2009)



df_for_censored_pois %>%
  filter(Population!=0) %>%
  group_by(Race) %>%
  summarise(mean_cdd = median(nighttime.cdh_summer),
            mean_night_cdh = median(nighttime.cdh_summer))

# County_avgs <- ACS_data_tract_race_temp %>%
#   group_by(county, variable) %>%
#   summarise(avg_cdh = weighted.mean(w_mean_cdh, estimate))
View(df_for_censored_pois %>% filter(Race == "White" & cy == 9))

### NLDAS now ###

# Merge tract estimates with tract race/ethnicity
Temp_and_Race_byTract_NLDAS <- ACS_data_tract_race_65plus %>%
  inner_join(., NLDAS_Temps, by = "GEOID") %>%
  mutate(State_FIPS = str_sub(GEOID, 1,2),
         cooling_dds = w_mean_cdh_NLDAS/24,
         County_FIPS = str_sub(GEOID, 1,5)) %>%
  left_join(., EIA_regions, by = "State_FIPS") 

Temp_and_Race_byTract_NLDAS %>% ##need to modify this for cross race comparison, not against EIA
  group_by(Climate_Region, variable) %>%
  summarise(xgboost_avg_cdds = weighted.mean(cooling_dds, estimate)) %>%
  inner_join(., EIA_CDDs_a, by = "Climate_Region") %>%
  mutate(pct_diff = (xgboost_avg_cdds - EIA_estimate_cdds)/xgboost_avg_cdds)

### Mortality analysis

##County level temp data 
Temp_and_Race_byCounty_NLDAS <- Temp_and_Race_byTract_NLDAS %>%
  group_by(County_FIPS, variable) %>%
  summarise(nldas_avg_cdds = weighted.mean(cooling_dds, estimate))

#Merge CVD deaths and ACS County data
WONDER_CVD_Mortality_65plus_wCensus_nldas <- WONDER_CVD_Mortality_65plus %>%
  left_join(., ACS_data_county_race_65plus, by = c("County_FIPS",
                                                   "Race" = "variable")) %>%
  mutate(Deaths = na_if(Deaths, "Suppressed"),
         deaths = as.numeric(Deaths),
         crude_cvd_mortality = (deaths/Population) * 100000) %>%
  dplyr::select(-Deaths) %>%
  left_join(., County_PopDensity, by = c("County_FIPS" = "GEOID")) %>%
  left_join(., Temp_and_Race_byCounty_NLDAS, by = c("County_FIPS",
                                                    "Race" = "variable")) 
filter(!is.na(Population) & !is.nan(nldas_avg_cdds)) ##double check this is okay because unsure why some are NA -- small subset though

df_for_censored_pois_nldas <- WONDER_CVD_Mortality_65plus_wCensus_nldas %>%
  mutate(lcensored = if_else(is.na(deaths), TRUE, FALSE),
         status = if_else(lcensored==TRUE, 0, 1),
         cy = if_else(is.na(deaths), 9, deaths),
         # race_dummy = as.factor(Race))
         race_dummy = as.factor(if_else(Race=="Black or African American", 1, 0)))


##Compare NLDAS temps within counties with XGBoost
# compare_county_temps <- Temp_and_Race_byCounty %>%
#   left_join(., Temp_and_Race_byCounty_NLDAS, by = c("County_FIPS", "variable")) %>%
#   mutate(pct_diff = ((xgboost_avg_cdds/nldas_avg_cdds)*100) 
#pct_diff = ((xgboost_avg_cdds - nldas_avg_cdds)/nldas_avg_cdds)*100

compare_county_temps <- Temp_and_Race_byCounty %>%
  left_join(., Temp_and_Race_byCounty_NLDAS, by = c("County_FIPS", "variable")) %>%
  filter(variable != "Total Population") %>%
  pivot_wider(names_from = variable, values_from = c(xgboost_avg_cdds, nldas_avg_cdds)) %>%
  mutate(ratio_racial_diff_xgboost = (`xgboost_avg_cdds_Black or African American`/`xgboost_avg_cdds_White`)*100,
         ratio_racial_diff_nldas = (`nldas_avg_cdds_Black or African American`/nldas_avg_cdds_White)*100)

disp_comparisons <- compare_county_temps %>%
  ungroup() %>%
  summarise(median_xgboost = median(ratio_racial_diff_xgboost, na.rm = T),
            median_nldas = median(ratio_racial_diff_nldas, na.rm = T),
            IQR_low_xgboost = min(ratio_racial_diff_xgboost, .25, na.rm = T),
            IQR_low_nldas = min(ratio_racial_diff_nldas, .25, na.rm = T),
            IQR_high_xgboost = quantile(ratio_racial_diff_xgboost, .75, na.rm = T),
            IQR_high_nldas = max(ratio_racial_diff_nldas, .75, na.rm = T))

## THIS IS COUNTY
# Total_pops <- ACS_data_county_race_65plus %>%
#   filter(variable == "Total Population") %>%
#   rename(total_pop = Population) %>%
#   dplyr::select(County_FIPS, total_pop)
# 
# ACS_data_county_race_65plus_props <- ACS_data_county_race_65plus %>%
#   filter(variable == "Black or African American") %>%
#   left_join(., Total_pops, by = "County_FIPS") %>%
#   mutate(prop_black = Population/total_pop)

# prop_tracts_black <- pop_tracts_race %>%
#   mutate(prop_black = BlackE/Total_popE) %>%
#   select(GEOID, year, prop_black) %>%
#   filter(year>2009)

corrected_broken_tract_racecompare <- pop_tracts_race %>%
  inner_join(., tract_2000_to_2010_links, by = c("GEOID" = "GEOID_00")) %>%
  mutate(Total_popE = round(Total_popE * (PctPop_2000/100), 0),
         BlackE = round(BlackE * (PctPop_2000/100), 0),
         WhiteE = round(WhiteE * (PctPop_2000/100), 0)) %>%
  select(GEOID_10, year, Total_popE, BlackE, WhiteE, County_FIPS, State_FIPS) %>%
  rename(GEOID = "GEOID_10")


# Total_pops_tracts <- ACS_data_tract_race_65plus %>%
#   filter(variable == "Total Population") %>%
#   rename(total_pop = estimate) %>%
#   dplyr::select(GEOID, total_pop)

# ACS_data_tract_race_65plus_props <- ACS_data_tract_race_65plus %>%
#   filter(variable == "Black or African American") %>%
#   left_join(., Total_pops_tracts, by = "GEOID") %>%
#   mutate(prop_black = estimate/total_pop, 
#          County_FIPS = str_sub(GEOID, 1,5),
#          State_FIPS = str_sub(GEOID, 1,2))

# pop_65plus_in_region <- Total_pops_tracts %>%
#   ungroup() %>%
#   summarise(pop_in_NEMIA = sum(total_pop))
setdiff(pop_tracts_race$GEOID, Temperatures_re$GEOID)
setdiff(Temperatures_re$GEOID, pop_tracts_race$GEOID)





# with(df_for_censored_pois, table(print(SurvS4(cy, status, type = "left"))))

cens.pois.nldas <- vglm(SurvS4(cy, status, type = "left") ~ nldas_avg_cdds + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                        data = subset(df_for_censored_pois_nldas, Race == "Total Population"), trace = T)

# cens.pois.race.nldas <- vglm(SurvS4(cy, status, type = "left") ~ nldas_avg_cdds + race_dummy + nldas_avg_cdds*race_dummy + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
#                        data = subset(df_for_censored_pois_nldas, Race == "Black or African American"|Race == "White"), trace = T)

cens.pois.black.nldas <- vglm(SurvS4(cy, status, type = "left") ~ nldas_avg_cdds + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                              data = subset(df_for_censored_pois_nldas, Race == "Black or African American"), trace = T)

cens.pois.white.nldas <- vglm(SurvS4(cy, status, type = "left") ~ nldas_avg_cdds + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                              data = subset(df_for_censored_pois_nldas, Race == "White"), trace = T)

compare_total_poissons <- bind_cols(tibble(vars = cens.pois@misc$colnames.x),tibble(coefs_xgboost = coef(cens.pois)), tibble(coefs_nldas = coef(cens.pois.nldas)))

# compare_race_poissons <- bind_cols(tibble(vars = cens.pois.race@misc$colnames.x), tibble(coefs_xgboost = coef(cens.pois.race)), tibble(coefs_nldas = coef(cens.pois.race.nldas)))

compare_black_poissons <- bind_cols(tibble(vars = cens.pois.black@misc$colnames.x), tibble(coefs_xgboost = coef(cens.pois.black)), tibble(coefs_nldas = coef(cens.pois.black.nldas)))

View(compare_total_poissons %>% 
       mutate(coefs_nldas = exp(coefs_nldas*92), #times 10 days to scale the variables 
              coefs_xgboost = exp(coefs_xgboost*92)))
round(exp(confint(cens.pois)*92),2)
exp(confint(cens.pois.nldas)*92)
round(exp(confint(cens.pois.black.nldas)*92),2)
round(exp(confint(cens.pois.white)*92),2)
round(exp(confint(cens.pois.white.nldas)*92),2)
View(compare_black_poissons %>% 
       mutate(coefs_nldas = exp(coefs_nldas*92), #times 10 days to scale the variables 
              coefs_xgboost = exp(coefs_xgboost*92)))

round(exp(confint(cens.pois.black)*92),2)
round(exp(confint(cens.pois.black.nldas)*92),2)
cens.pois.black.nldas

df_for_censored_pois %>% filter(Race == "Total Population") %>% summarise(median(xgboost_avg_cdds, na.rm = T))
df_for_censored_pois %>% filter(Race == "Total Population") %>% summarise(quantile(xgboost_avg_cdds, .25, na.rm = T))
df_for_censored_pois %>% filter(Race == "Total Population") %>% summarise(quantile(xgboost_avg_cdds, .75, na.rm = T))
# CONUS <- read_delim(here("data", "StatesCONUS.Cooling.txt"), delim = "|", skip = 3)



# ACS_data_allUS_race <- ACS_data_tract_race_65plus %>%
#   mutate(Race = recode(variable, "white"
#                        , "White", "Black or African American")) %>%
#   group_by(GEOID, NAME, Race) %>%
#   summarise(population = sum(estimate))

# WONDER_Mortality_w_Census <- WONDER_CVD_Mortality_65plus %>%
#   inner_join(., ACS_data_tract_race_65plus, by = c("County Code" = "GEOID",
#                                        "Race" = "variable"))
#   dplyr::select(2:4,6,10) %>%
#   mutate(deaths = as.numeric(na_if(Deaths, "Suppressed")),
#     crude_mortality_rate = (deaths/population)*100000, Deaths)

# ACS_data_tract_race <- get_acs(geography = "tract",
#                           variables = c("total_population" = "B01003_001",
#                                         "white_population" = "B02001_002",
#                                         "black_population" = "B02001_003"),
#                           year = 2019,
#                           survey = "acs5", 
#                           output = "tidy",
#                           state = NEMIA_States)

#check exposures for white comparing NLDAS and XGBoost
xgboost_whites <- Temp_and_Race_byCounty %>%
  select(County_FIPS, xgboost_avg_cdds, variable) %>%
  filter(variable == "White") 

nldas_whites <- Temp_and_Race_byCounty_NLDAS %>%
  select(County_FIPS, nldas_avg_cdds, variable) %>%
  filter(variable == "White") 

combined_white_temps <- xgboost_whites %>%
  left_join(., nldas_whites, by = "County_FIPS")

ggplot(combined_white_temps) + geom_point(aes(x = xgboost_avg_cdds, y = nldas_avg_cdds))


## Try again with later df 
xgboost_whites1 <- df_for_censored_pois %>%
  select(County_FIPS, xgboost_avg_cdds, Race) %>%
  filter(Race == "White") 

nldas_whites1 <- df_for_censored_pois_nldas %>%
  select(County_FIPS, nldas_avg_cdds, Race) %>%
  filter(Race == "White") 

combined_white_temps1 <- xgboost_whites1 %>%
  left_join(., nldas_whites1, by = "County_FIPS")

ggplot(combined_white_temps1) + geom_point(aes(x = xgboost_avg_cdds, y = nldas_avg_cdds))





###
# ACS_data_tract_race_temp <- ACS_data_tract_race_65plus %>%
#   inner_join(., Temperatures, by = "GEOID") %>%
#   mutate(xgboost_cdd = w_mean_cdh/24, 
#     county = str_sub(GEOID, 1, 5))
# 





Epi_results <- bind_rows(tibble(Coefficient = exp(coef(summary(cens.pois.black))[2,1]*92), CI_low = exp(confint(cens.pois.black)*92)[2,1], CI_hi = exp(confint(cens.pois.black)*92)[2,2], Race = "Black", Model = "XIS"),
                         tibble(Coefficient = exp(coef(summary(cens.pois.black.nldas))[2,1]*92), CI_low = exp(confint(cens.pois.black.nldas)*92)[2,1], CI_hi = exp(confint(cens.pois.black.nldas)*92)[2,2], Race = "Black", Model = "NLDAS-2"),
                         tibble(Coefficient = exp(coef(summary(cens.pois.white))[2,1]*92), CI_low = exp(confint(cens.pois.white)*92)[2,1], CI_hi = exp(confint(cens.pois.white)*92)[2,2], Race = "White", Model = "XIS"),
                         tibble(Coefficient = exp(coef(summary(cens.pois.white.nldas))[2,1]*92), CI_low = exp(confint(cens.pois.white.nldas)*92)[2,1], CI_hi = exp(confint(cens.pois.white.nldas)*92)[2,2], Race = "White", Model = "NLDAS-2"))

ggplot(data=Epi_results)+
  geom_pointrange(aes(x = Model, y = Coefficient, ymin = CI_low, ymax = CI_hi, color = Race)) + 
  scale_color_manual(values = c("#4053d3", "#ddb310")) + 
  facet_wrap(Race ~ .) +
  ylab("Risk Ratio") + 
  theme(text = element_text(size = 20))

# ggplot() + 
#   geom_boxplot(data = Combined_temps, aes(x = cooling_dds, y = .01, group = variable)) +
#   facet_grid(model ~ .)

summary(cens.pois.white.nldas)
exp(.0004429*92)
exp(.0004512*92)


zcta_energy <- read_csv(here("data", "Utility_Energy_Registry_Monthly_ZIP_Code_Energy_Use__Beginning_2016.csv"))
zcta_energy %>% filter()

# 
# 
# 
# Temp_Disparity_XGBoost <- lmer(xgboost_cdds ~ prop_black + (prop_black || County_FIPS), 
#                                data = ACS_data_tract_race_65plus_props_XGBoost)
# confint(Temp_Disparity_XGBoost, method = "boot", nsim = 1000)
# 
# coefs_tempdisp_xgboost <- data.frame(coef(summary(Temp_Disparity_XGBoost)))
# coefs_tempdisp_xgboost
# 
# ACS_data_tract_race_65plus_props_NLDAS <- ACS_data_tract_race_65plus_props %>%
#   inner_join(., NLDAS_Temps, by = "GEOID") %>%
#   mutate(nldas_cdds = w_mean_cdh_NLDAS/24)
# 
# Temp_Disparity_NLDAS <- lmer(nldas_cdds ~ prop_black + (prop_black || County_FIPS), 
#                              data = ACS_data_tract_race_65plus_props_NLDAS)
# confint(Temp_Disparity_NLDAS, method = "boot", nsim = 1000)
# 
# coefs_tempdisp_nldas <- data.frame(coef(summary(Temp_Disparity_NLDAS)))
# coefs_tempdisp_nldas


# Temp_and_Race_byTract1 <- Temp_and_Race_byTract %>%
#   mutate(model = "XIS")
# 
# Temp_and_Race_byTract1 %>%
#   filter(variable != "Total Population") %>%
#   group_by(variable) %>%
#   summarise(median = matrixStats::weightedMedian(cooling_dds, estimate))
# 
# Temp_and_Race_byTract_NLDAS1 <- Temp_and_Race_byTract_NLDAS %>%
#   mutate(model = "NLDAS-2") 
# 
# Temp_and_Race_byTract_NLDAS1 %>%
#   filter(variable != "Total Population") %>%
#   group_by(variable) %>%
#   summarise(median = matrixStats::weightedMedian(cooling_dds, estimate))
# 
# Combined_temps <- bind_rows(Temp_and_Race_byTract1, Temp_and_Race_byTract_NLDAS1) %>%
#   filter(variable != "Total Population")
# 
# Combined_temps %>% #across all NEMIA -- exposure disparity already pretty stark
#   filter(variable != "Total Population") %>%
#   group_by(variable, model) %>%
#   summarise(weighted.mean(cooling_dds, estimate))
# 
# Combined_temps_uncounted <- Combined_temps %>% #across all NEMIA -- exposure disparity already pretty stark
#   filter(variable != "Total Population") %>%
#   dplyr::select(GEOID, variable, estimate, cooling_dds, model) %>%
#   ungroup() %>%
#   uncount(estimate)
# 
# sum(Combined_temps$estimate)
# dat_text <- data.frame(
#   label = c("558", "427", "524", "430"),
#   model = c("XIS", "XIS", "NLDAS-2", "NLDAS-2"),
#   x     = c(558, 427, 524, 430),
#   y     = c(.0065, .0027, .0071, .0027)
# )
# 
# ggplot(data = Combined_temps_uncounted, aes(x = variable, y = cooling_dds, color = variable)) + 
#   geom_boxplot(width = .5, position = position_dodge(width = .01)) +
#   theme(legend.position = "none") + 
#   facet_grid(model ~ .) + 
#   coord_flip() + 
#   theme_minimal()

#,plot.margin = unit(c(1, 1, 1, 1), "cm")

# Combined_temps %>%
#   ggplot(aes(cooling_dds)) +
# ggplot() + 
#   geom_density(data = Combined_temps, aes(x = cooling_dds, fill = variable, weights=estimate/sum(estimate)), alpha = .4) +
#   scale_fill_manual(values = c("#4053d3", "#ddb310")) + 
#   geom_text(data = dat_text, aes(label = label, x = x, y = y), size = 5) +
#   theme(text = element_text(size=20)) + 
#   labs(fill = "Race") + 
#   facet_grid(model ~ .) +
#   xlab("Cooling Degree Days (°C)") +
#   ylab("Density") 



# Temp_and_Race_byTract %>% ##need to modify this for cross race comparison, not against EIA
#   group_by(Climate_Region, variable) %>%
#   summarise(xgboost_avg_cdds = weighted.mean(cooling_dds, estimate)) %>%
#   inner_join(., EIA_CDDs_a, by = "Climate_Region") %>%
#   mutate(pct_diff = (xgboost_avg_cdds - EIA_estimate_cdds)/xgboost_avg_cdds)
# length(unique(Temp_and_Race_byTract$GEOID))
### Mortality analysis

##County level temp data 
# Temp_and_Race_byCounty <- Temp_and_Race_byTract %>%
#   group_by(County_FIPS, variable, year) %>%
#   summarise_at(vars(mean_temp_summer:mean_PM_summer), ~ weighted.mean(., estimate)) %>%
#   arrange(year, County_FIPS) %>%
#   mutate_at(vars(mean_temp_summer:mean_PM_summer), ~ifelse(is.nan(.), NA, .)) %>%
#   ungroup() %>%
#   group_by(year, County_FIPS) %>%
#   tidyr::fill(mean_temp_summer:mean_PM_summer, .direction = "updown")

#### hourly by race-county visualizations ####

#make a function that subsets the list of paths by year, and then binds them, and then does the averaging


# create_weighted_race_hours <- function(all_pred_filepaths, year){#by county
#   
#   year_i_pred_paths <- enframe(all_pred_filepaths) %>% filter(str_detect(value, year))
#   
#   year_i_preds <- year_i_pred_paths$value %>%
#     map_dfr(read_fst)
#   
#   summarized_day_hour <- year_i_preds %>%
#     mutate(ground.time.nominal = format.POSIXct(ground.time.nominal, tz = "America/New_York"),
#            year = year(ground.time.nominal),
#            census_year = if_else(year<=2009, 2000, 2010),
#            month = month(ground.time.nominal),
#            day = day(ground.time.nominal),
#            hour = hour(ground.time.nominal)) %>%
#     left_join(Tract_RaceEthn_Census, by = c("census_year", "GEOID")) %>%
#     dplyr::select(year, month, hour, day, County_FIPS, w_temp, Black, White) %>%
#     filter(month>=5 & month<=9) %>%
#     pivot_longer(cols = c("Black", "White"), names_to = "race", values_to = "estimate") %>%
#     group_by(month, year, hour, day, County_FIPS, race) %>%
#     summarise(county_monthhour_temp = weighted.mean(w_temp, estimate))
#   
#   return(summarized_day_hour)
# }

# race_hour_temps_2019 <- create_weighted_race_hours(all_pred_filepaths, "2019")

   
race_hour_temps_NEMIA_2003 <- race_hour_temps_NEMIA %>%
  filter(year==2003) %>%
  pivot_wider(names_from = race,
              names_sep = ".",
              values_from = temp_c) %>%
  mutate(Black_disparity = ((Black - White)/White) *100) 

pct_diff <- ggplot(race_hour_temps_NEMIA_2003, aes(date, hour, z = Black_disparity)) + #, breaks = c(0,12,18,24,30,36)
  geom_contour_fill(aes(fill = stat(level))) + #, breaks = c(-20,-10, 0, 10, 20, 30, 40) 
  geom_contour_tanaka() +
  scale_x_date(labels = function(x) format(x, "%b")) +
  scale_fill_divergent_discretised(low = "blue", mid = "grey", high = "red4", midpoint = 0, name = "% Difference", guide = guide_coloursteps(show.limits = T, barwidth = 12)) +
  scale_y_reverse(breaks = c(0,6,12,18,23), labels = c("12 AM", "6 AM", "12 PM", "6 PM", "11 PM")) + 
  #scale_y_continuous(breaks = c(0,6,12,18,23), labels = c("12 AM", "6 AM", "12 PM", "6 PM", "11 PM")) +
  theme_minimal(base_size = 14) + 
  xlab("Day of year, 2003") + 
  ylab("Hour of day") + 
  theme(legend.position = "bottom")

ggsave(pct_diff, file=here("plots", "contour_plot_2003diff.png"), width = 28, height = 10, units = "cm", dpi = 320)

# save(pct_diff, file = "pct_diff_for_rayshader.rdata")

# (race_2010_plot / pct_diff)  + plot_layout(ncol = 1) + xlab("2010") + 
#   ylab("Hour")



ggplot(race_hour_temps_NEMIA_2019_a) + 
  geom_contour_fill(aes(day_of_yr, hour, z = temp_c, fill = temp_c_cut)) +
  # scale_fill_discretised() +
  #guides(fill = guide_colorsteps()) +
  #geom_contour_tanaka()+
  facet_wrap(race~.) +  
  theme_minimal()
  scale_fill_divergent()



