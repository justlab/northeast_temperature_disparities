library(tidyverse)
library(tidycensus)
library(here)
library(fst)
library(VGAM)
library(sf)
library(lme4)
library(fst)
library(lubridate)
library(ggridges)

# WONDER_CVD_Mortality_18plus <- read_tsv(here("data","WONDER_Mortality_NEMIA_Summer2019.txt"))
WONDER_CVD_Mortality_65plus_total <- read_tsv(here("data","WONDER_CVD_deaths_65plus_total1.txt")) #to erase
WONDER_CVD_Mortality_65plus_total_re <- read_tsv(here("data","WONDER_CVD_deaths_65plus_total_2003_19.txt")) #replacement

WONDER_CVD_Mortality_65plus_byRace <- read_tsv(here("data","WONDER_CVD_deaths_65plus_race1.txt")) #to erase
WONDER_CVD_Mortality_65plus_byRace_re <- read_tsv(here("data","WONDER_CVD_deaths_65plus_byRace_2003_19.txt")) #replacement

Temperatures <- read_rds(here("/data-coco/NEMIA_temperature/cdh/cdh_tractsSF_2019_06-08.rds")) #to erase
Temperatures_re <- read_fst(here("data", "summarized_daily_temp_preds.fst")) #replacement

air_pollution <- read_fst(here("data", "faqsd_pm_and_O3_warmmths.fst"))

EIA_CDDs <- read_csv(here("data", "9c._US_Regional_Weather_Data.csv"), skip = 4) 
NEMIA_States <- c("09", "23", "25", "33", "44", "50", 
                    "34", "36", "42",
                    "10", "11", "24", "51", "54")
NLDAS_Temps <- read_fst("/data-coco/NEMIA_temperature/cdh/cdh_bytract_2019_06-08_NLDAS.fst")

tract_2000_to_2010_links <- read_csv(here("data", "us2010trf.txt"), col_names = F) %>%
  rename(state_fips_00 = "X1",
         GEOID_00 = "X4",
         GEOID_10 = "X13",
         PART10 = "X16",
         PctArea_of_2000 = "X21",
         Pop2010 = "X25",
         PctPop_2000 = "X26",
         PctPop_2010 = "X27") %>%
  filter(state_fips_00 %in% NEMIA_States) %>%
  select(GEOID_00, GEOID_10, PART10, PctArea_of_2000, Pop2010, PctPop_2000, PctPop_2010)

Convert_F_to_C <- function(temp_in_F) {
  
  temp_in_C = (temp_in_F - 32) * (5/9)
  return(temp_in_C)
  
}

##Temp cleaning
Temperatures_re <- Temperatures_re %>%
  filter(month(date)>=6 & month(date)<=9) %>%
  mutate(year = year(date)) %>%
  group_by(year, GEOID) %>%
  summarise(mean_temp_summer = mean(mean_temp_daily),
            mean_htindx_summer = mean(mean_htindx_daily),
            cdd_summer = sum(cdd),
            nighttime.cdh_summer = sum(nighttime.cdh))

air_pollution_summaries <- air_pollution %>%
  filter(month(date)>=6 & month(date)<=9) %>%
  mutate(year = year(date), 
         GEOID = FIPS) %>%
  group_by(year, GEOID) %>%
  summarise(mean_O3_summer = mean(ozone_pred),
            mean_PM_summer = mean(pm25_pred))

##Clean WONDER CVD Mortality 
WONDER_CVD_Mortality_65plus_byRace_a <- WONDER_CVD_Mortality_65plus_byRace_re %>%
  rename("year" = `Year Code`) %>%
  filter(Race == "Black or African American"| Race == "White") %>%
  mutate(Race = if_else(Race=="Black or African American", "Black", Race)) %>%
  dplyr::select(`County Code`, Race, Deaths, year)

WONDER_CVD_Mortality_65plus_total_a <- WONDER_CVD_Mortality_65plus_total_re %>%
  rename("year" = `Year Code`) %>%
  dplyr::select(`County Code`, Deaths, year) %>%
  mutate(Race = "Total Population")

WONDER_CVD_Mortality_65plus <- bind_rows(WONDER_CVD_Mortality_65plus_total_a, WONDER_CVD_Mortality_65plus_byRace_a) %>%
  rename("County_FIPS" = "County Code")

#Get Census Data
ACS_vars_2019 <- load_variables(2019, "acs5", cache = TRUE) 

View(ACS_vars_2019 %>%
  filter(concept == "POPULATION"))

View(ACS_vars_2019 %>%
       filter(str_detect(label, "65")))

calculate_annual_popdensity <- function(year){
  
County_PopDensity <- get_acs(geography = "county",
        variables = c("total_pop" = "B19037A_053"), #check to make 
        year = year,
        survey = "acs5",
        output = "tidy",
        geometry = T,
        state = NEMIA_States) %>%
  mutate(area = st_area(.),
    pop_density =  estimate/area,
    year = year) %>%
  st_drop_geometry() %>%
  dplyr::select(GEOID, pop_density, year)

# County_PopDensity %>% mutate(County_FIPS = str_sub(GEOID, 1,5)) %>% distinct(County_FIPS)

return(County_PopDensity)
}

annual_pop_density <- lapply(seq.int(2009, 2017), calculate_annual_popdensity)
annual_pop_density <- bind_rows(annual_pop_density)

get_pop_tracts_65plus <- function(year){
  
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
                                                    "White_m_65_74" = "B01001A_014",
                                                    "White_m_75_84" = "B01001A_015",
                                                    "White_m_85_up" = "B01001A_016",
                                                    "White_f_65_74" = "B01001A_029",
                                                    "White_f_75_84" = "B01001A_030",
                                                    "White_f_85_up" = "B01001A_031",
                                                    "Black_m_65_74" = "B01001B_014",
                                                    "Black_m_75_84" = "B01001B_015",
                                                    "Black_m_85_up" = "B01001B_016",
                                                    "Black_f_65_74" = "B01001B_029",
                                                    "Black_f_75_84" = "B01001B_030",
                                                    "Black_f_85_up" = "B01001B_031"),
                                      year = 2009,
                                      survey = "acs5", 
                                      output = "tidy",
                                      state = NEMIA_States)

ACS_data_tract_race_65plus_a <- ACS_data_tract_race_65plus %>%
  mutate(variable = str_sub(variable, 1,5)) %>%
  group_by(GEOID, variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(variable = recode(variable,
                       "Total" = "Total Population"),
         year = year)

return(ACS_data_tract_race_65plus_a)

}

find_vars <- load_variables(2009, "acs5")

get_total_race_byTracts <- function(year){
  
  ACS_data_tract_race <- get_acs(geography = "tract",
                                        variables = c("Black" = "B02001_003",
                                                      "White" = "B02001_002",
                                                      "Total_pop" = "B01001_001"),
                                        year = year,
                                        survey = "acs5", 
                                        output = "wide",
                                        state = NEMIA_States)

  ACS_data_tract_race1 <- ACS_data_tract_race %>%
    mutate(State_FIPS = str_sub(GEOID, 1, 2),
           County_FIPS = str_sub(GEOID, 1, 5)) %>%
    ungroup() %>%
    mutate(year = year)
  
return(ACS_data_tract_race1)

}

get_pop_counties_65plus <- function(year){
  
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
                                                      "White_m_65_74" = "B01001A_014",
                                                      "White_m_75_84" = "B01001A_015",
                                                      "White_m_85_up" = "B01001A_016",
                                                      "White_f_65_74" = "B01001A_029",
                                                      "White_f_75_84" = "B01001A_030",
                                                      "White_f_85_up" = "B01001A_031",
                                                      "Black_m_65_74" = "B01001B_014",
                                                      "Black_m_75_84" = "B01001B_015",
                                                      "Black_m_85_up" = "B01001B_016",
                                                      "Black_f_65_74" = "B01001B_029",
                                                      "Black_f_75_84" = "B01001B_030",
                                                      "Black_f_85_up" = "B01001B_031"),
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
pop_counties_65plus_peryer <- pop_counties_65plus_peryer %>%
  mutate(variable = if_else(variable == "Total", "Total Population", variable))
pop_tracts_65plus_peryer <- lapply(seq.int(2009,2017), get_pop_tracts_65plus)
pop_tracts_65plus_peryer <- bind_rows(pop_tracts_65plus_peryer)

pop_tracts_race <- lapply(seq.int(2009,2017), get_total_race_byTracts)
pop_tracts_race <- bind_rows(pop_tracts_race)

EIA_regions <- tibble(Climate_Region = c("New England","New England","New England","New England","New England","New England",
                                         "Middle Atlantic","Middle Atlantic","Middle Atlantic",
                                         "South Atlantic", "South Atlantic","South Atlantic", "South Atlantic","South Atlantic"), 
                      State_FIPS = c("09", "23", "25", "33", "44", "50", 
                                     "34", "36", "42",
                                     "10", "11", "24", "51", "54"))

### SUMMARY of why the below cleaning/merging situation is currently so confusing. tidycensus is producing 2000 census tract geographies for a subset of tracts. Super weird., 
## temp models switch from 2000 CTract geographies to 2010 as they should, and FAQSD PM and O3 come in only 2010 geographies. Need to make a cleaner 
## Used a Census Tract relationship file to translate 2000 to 2010 geographies when appropriate. But need to create much more parsimonious code. For sure. Giving up on 2009 currently
## to do 2010-2017. 

# Merge tract estimates with tract race/ethnicity
Temp_and_Race_byTract <- pop_tracts_65plus_peryer %>%
  inner_join(., Temperatures_re, by = c("GEOID", "year")) %>%
  inner_join(., air_pollution_summaries, by = c("GEOID", "year")) %>%
  mutate(State_FIPS = str_sub(GEOID, 1,2),
         County_FIPS = str_sub(GEOID, 1,5)) %>%
  left_join(., EIA_regions, by = "State_FIPS") 

# Temp_and_Race_byTract %>% ##need to modify this for cross race comparison, not against EIA
#   group_by(Climate_Region, variable) %>%
#   summarise(xgboost_avg_cdds = weighted.mean(cooling_dds, estimate)) %>%
#   inner_join(., EIA_CDDs_a, by = "Climate_Region") %>%
#   mutate(pct_diff = (xgboost_avg_cdds - EIA_estimate_cdds)/xgboost_avg_cdds)
# length(unique(Temp_and_Race_byTract$GEOID))
### Mortality analysis

##County level temp data 
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
  filter(year>2009) %>% #for now since Census geographies are messed up
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
                                              "year")) 
  #filter(!is.na(Population) & !is.nan(xgboost_avg_cdds)) ##double check this is okay because unsure why some are NA -- small subset though

Non_merging_counties <- WONDER_CVD_Mortality_65plus_wCensus %>% filter(is.na(mean_temp_summer)) %>% distinct(County_FIPS)

corrected_broken_tract_GEOIDS <- pop_tracts_65plus_peryer %>%
  ungroup() %>%
  mutate(State_FIPS = str_sub(GEOID, 1,2),
         County_FIPS = str_sub(GEOID, 1,5)) %>%
  filter(County_FIPS %in% Non_merging_counties$County_FIPS & year != 2009) %>%
  inner_join(., tract_2000_to_2010_links, by = c("GEOID" = "GEOID_00")) %>%
  mutate(estimate1 = round(estimate * (PctPop_2000/100), 0)) %>%
  select(GEOID_10, estimate1, variable, year, County_FIPS) %>%
  rename(GEOID = "GEOID_10",
         estimate = "estimate1")

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

df_for_censored_pois <- WONDER_CVD_Mortality_65plus_wCensus %>%
  mutate(lcensored = if_else(is.na(deaths), TRUE, FALSE),
         status = if_else(lcensored==TRUE, 0, 1),
         cy = if_else(is.na(deaths), 9, deaths),
         # race_dummy = as.factor(Race))
         race_dummy = as.factor(if_else(Race=="Black", 1, 0)))

#### Running Censored Poissons ####

##Finish this function to run the models 
function(df, heat_measure){
cens.pois <- vglm(SurvS4(cy, status, type = "left") ~ scale(nighttime.cdh_summer, center = T) + pop_density + as.factor(State_FIPS) + as.factor(year) + mean_O3_summer + offset(log(Population)), cens.poisson,
     data = subset(df_for_censored_pois, Race == "Total Population"), trace = T)

cens.pois.black <- vglm(SurvS4(cy, status, type = "left") ~ scale(mean_htindx_summer, center = F) + pop_density + as.factor(State_FIPS) + as.factor(year) + offset(log(Population)), cens.poisson,
                              data = subset(df_for_censored_pois, Race == "Black" & Population >0), trace = T)

cens.pois.white <- vglm(SurvS4(cy, status, type = "left") ~ scale(mean_htindx_summer, center = F) + pop_density + as.factor(State_FIPS) + as.factor(year) + offset(log(Population)), cens.poisson,
                        data = subset(df_for_censored_pois, Race == "White"), trace = T)

tibble(coef = exp(coef(cens.pois)[2]),
       bind_rows(exp(confintvglm(cens.pois, "scale(nighttime.cdh_summer, center = T)", method = "profile"))))
summary(cens.pois)
iqr_increase <- IQR(df_for_censored_pois$nighttime.cdh_summer)
exp(coef(cens.pois)[2]*iqr_increase)
summary(cens.pois)

summary(cens.pois.black)
exp(coef(cens.pois.black)[2]*iqr_increase)
confint(cens.pois, parm = "scale(nighttime.cdh_summer, center = T)", method = "boot", nsim= 1000)
summary(cens.pois.white)
exp(coef(cens.pois.white)[2]*iqr_increase)

}

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


ACS_data_tract_race_temps <- pop_tracts_race %>%
  anti_join(., corrected_broken_tract_racecompare, by = "GEOID") %>%
  bind_rows(corrected_broken_tract_racecompare) %>%
  inner_join(., Temperatures_re, by = c("GEOID", "year")) %>%
  filter(Total_popE > 0 & year>2009) %>%
  select(GEOID, year, BlackE, WhiteE, cdd_summer, nighttime.cdh_summer)

temps_for_ggplot_density <- ACS_data_tract_race_temps %>%
  pivot_longer(cols = c("BlackE", "WhiteE"), names_to = "race", values_to = "estimate") 

temps_for_ggplot_density %>%
  ggplot(.) +
  geom_density(aes(x = cdd_summer, fill = race, weights=estimate/sum(estimate)), alpha = .4) +
  scale_fill_manual(values = c("#4053d3", "#ddb310")) + 
  facet_grid(year~.)




Temp_Disparity_XGBoost <- lmer(xgboost_cdds ~ prop_black + (prop_black || County_FIPS), 
                               data = ACS_data_tract_race_65plus_props_XGBoost)
confint(Temp_Disparity_XGBoost, method = "boot", nsim = 1000)

coefs_tempdisp_xgboost <- data.frame(coef(summary(Temp_Disparity_XGBoost)))
coefs_tempdisp_xgboost

ACS_data_tract_race_65plus_props_NLDAS <- ACS_data_tract_race_65plus_props %>%
  inner_join(., NLDAS_Temps, by = "GEOID") %>%
  mutate(nldas_cdds = w_mean_cdh_NLDAS/24)

Temp_Disparity_NLDAS <- lmer(nldas_cdds ~ prop_black + (prop_black || County_FIPS), 
                               data = ACS_data_tract_race_65plus_props_NLDAS)
confint(Temp_Disparity_NLDAS, method = "boot", nsim = 1000)

coefs_tempdisp_nldas <- data.frame(coef(summary(Temp_Disparity_NLDAS)))
coefs_tempdisp_nldas




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


Temp_and_Race_byTract1 <- Temp_and_Race_byTract %>%
  mutate(model = "XIS")

Temp_and_Race_byTract1 %>%
  filter(variable != "Total Population") %>%
  group_by(variable) %>%
  summarise(median = matrixStats::weightedMedian(cooling_dds, estimate))

Temp_and_Race_byTract_NLDAS1 <- Temp_and_Race_byTract_NLDAS %>%
  mutate(model = "NLDAS-2") 

Temp_and_Race_byTract_NLDAS1 %>%
  filter(variable != "Total Population") %>%
  group_by(variable) %>%
  summarise(median = matrixStats::weightedMedian(cooling_dds, estimate))

Combined_temps <- bind_rows(Temp_and_Race_byTract1, Temp_and_Race_byTract_NLDAS1) %>%
  filter(variable != "Total Population")

Combined_temps %>% #across all NEMIA -- exposure disparity already pretty stark
  filter(variable != "Total Population") %>%
  group_by(variable, model) %>%
  summarise(weighted.mean(cooling_dds, estimate))

Combined_temps_uncounted <- Combined_temps %>% #across all NEMIA -- exposure disparity already pretty stark
  filter(variable != "Total Population") %>%
  dplyr::select(GEOID, variable, estimate, cooling_dds, model) %>%
  ungroup() %>%
  uncount(estimate)
  
sum(Combined_temps$estimate)
dat_text <- data.frame(
  label = c("558", "427", "524", "430"),
  model = c("XIS", "XIS", "NLDAS-2", "NLDAS-2"),
  x     = c(558, 427, 524, 430),
  y     = c(.0065, .0027, .0071, .0027)
)

ggplot(data = Combined_temps_uncounted, aes(x = variable, y = cooling_dds, color = variable)) + 
  geom_boxplot(width = .5, position = position_dodge(width = .01)) +
  theme(legend.position = "none") + 
  facet_grid(model ~ .) + 
  coord_flip() + 
  theme_minimal()

#,plot.margin = unit(c(1, 1, 1, 1), "cm")

# Combined_temps %>%
#   ggplot(aes(cooling_dds)) +
ggplot() + 
  geom_density(data = Combined_temps, aes(x = cooling_dds, fill = variable, weights=estimate/sum(estimate)), alpha = .4) +
  scale_fill_manual(values = c("#4053d3", "#ddb310")) + 
  geom_text(data = dat_text, aes(label = label, x = x, y = y), size = 5) +
  theme(text = element_text(size=20)) + 
  labs(fill = "Race") + 
  facet_grid(model ~ .) +
  xlab("Cooling Degree Days (°C)") +
  ylab("Density") 


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