library(data.table)
library(fst)
library(future.apply)
library(lubridate)
# library(tidyverse)
# library(dtplyr)

t00weights = read_fst('/data-coco/NEMIA_temperature/weights/nemia_tracts_2000_popdens.fst', as.data.table = TRUE)
t10weights = read_fst('/data-coco/NEMIA_temperature/weights/nemia_tracts_2010_popdens.fst', as.data.table = TRUE)

sample_half_month <- read_fst('/data-coco/NEMIA_temperature/saved-predictions/all_monthly/2003_05_h1.fst', as.data.table = T)

start_date = as.Date('2003-05-01')
end_date = as.Date('2019-09-30')
all_dates = seq.Date(start_date, end_date, by = 1)
warm_month_dates = subset(all_dates, format.Date(all_dates, "%m") %in% c("05","06","07","08","09"))
warm_months = unique(format(warm_month_dates, '%Y_%m'))
all_years = unique(format(warm_month_dates, '%Y'))
# tz = "America/New York"

all_files = apply(expand.grid(warm_months, c('h1.fst', 'h2.fst')), MARGIN = 1, 
                  FUN = paste0, collapse = '_')
all_files_2000CTs = sort(all_files)[1:70]
all_files_2010CTs = sort(all_files)[71:170]

path_preds = '/data-coco/NEMIA_temperature/saved-predictions/all_monthly/'
save_hourly_CT_to = "/home/carrid08/northeast_temperature_disparities/data/hourly_tract_preds/"

temperatureexcess <- function(temperature, threshold = 18.333){
  pmax(temperature, threshold) - threshold
}

create_tract_hourly_summaries <- function(fst_file, tract_weights){
  
  temp =  read_fst(file.path(path_preds, fst_file), as.data.table = T)
  
    joined = tract_weights[temp, on = 'gid', nomatch = 0, allow.cartesian = TRUE]
    weighted_hourly_bytract = joined[, .(w_temp = sum(pred.temp.K * popdens * coverage_area, na.rm = T)/
                                          sum(popdens * coverage_area, na.rm = T),
                                        w_temp_heatindex = sum(pred.heat.index.K * popdens * coverage_area, na.rm = T)/
                                          sum(popdens * coverage_area, na.rm = T)),
                                    by = .(GEOID, ground.time.nominal)]
    write_fst(weighted_hourly_bytract, file.path(save_hourly_CT_to, fst_file))
}


start <- proc.time()
# create_temp_measures(all_files_2000CTs[3], t00weights)
create_tract_hourly_summaries(all_files_2000CTs[3], t00weights)
proc.time() - start

setDTthreads(threads = 15)
lapply(all_files_2000CTs, FUN = create_tract_hourly_summaries, t00weights)
lapply(all_files_2010CTs, FUN = create_tract_hourly_summaries, t10weights)

###
read_files <- function(fst_file){
  read_fst(file.path(save_hourly_CT_to, fst_file), as.data.table = T)
}

produce_daily_summaries <- function(year){ 
  
  files_per_year <- all_files[all_files %like% as.expression(year)] 
  all_temps_per_summer <- lapply(files_per_year, read_files)
  all_temps_per_summer <- rbindlist(all_temps_per_summer)

  all_temps_per_summer[, ground.time.nominal := format.POSIXct(ground.time.nominal, tz = "America/New_York")]
  all_temps_per_summer[, date := as.Date(ground.time.nominal)]

  daily_temps_and_cdds <- all_temps_per_summer[, .(mean_temp_daily = mean(w_temp),
                                                 mean_htindx_daily = mean(w_temp_heatindex),
                                                 noaa_mean = ((max(w_temp) - min(w_temp))/2)+min(w_temp)), 
                                             by = .(GEOID, date)]

  daily_temps_and_cdds[, cdd := temperatureexcess(mean_temp_daily - 273.15), by = c("GEOID", "date")]

  evening_hours = all_temps_per_summer[hour(ground.time.nominal)<=6 | hour(ground.time.nominal)>=18]
  evening_hours[, evening_cdh := temperatureexcess(w_temp - 273.15), by = c("GEOID", "ground.time.nominal")]

  daily_cdhs = evening_hours[, .(nighttime.cdh = sum(evening_cdh, na.rm = T)),
                                by = .(GEOID, date)]

  all_exposures <- daily_temps_and_cdds[daily_cdhs, on = c("GEOID", "date"), nomatch = 0, allow.cartesian = TRUE]

return(all_exposures)
}

# produce_daily_summaries(2003)

NEMIA_daily_summaries_all_years <- lapply(all_years, produce_daily_summaries)
NEMIA_daily_summaries_all_years <- rbindlist(NEMIA_daily_summaries_all_years)
write.fst(NEMIA_daily_summaries_all_years, "/home/carrid08/northeast_temperature_disparities/data/summarized_daily_temp_preds.fst")





library(tidyverse)
library(tidycensus)
library(here)
library(fst)
library(VGAM)
library(sf)
library(lme4)

# WONDER_CVD_Mortality_18plus <- read_tsv(here("data","WONDER_Mortality_NEMIA_Summer2019.txt"))
WONDER_CVD_Mortality_65plus_total <- read_tsv(here("data","WONDER_CVD_deaths_65plus_total1.txt"))
WONDER_CVD_Mortality_65plus_byRace <- read_tsv(here("data","WONDER_CVD_deaths_65plus_race1.txt"))
Temperatures <- read_rds(here("/data-coco/NEMIA_temperature/cdh/cdh_tractsSF_2019_06-08.rds"))
EIA_CDDs <- read_csv(here("data", "9c._US_Regional_Weather_Data.csv"), skip = 4) 
NEMIA_States <- c("09", "23", "25", "33", "44", "50", 
                    "34", "36", "42",
                    "10", "11", "24", "51", "54")
NLDAS_Temps <- read_fst("/data-coco/NEMIA_temperature/cdh/cdh_bytract_2019_06-08_NLDAS.fst")

Convert_F_to_C <- function(temp_in_F) {
  
  temp_in_C = (temp_in_F - 32) * (5/9)
  return(temp_in_C)
  
}

##Clean WONDER CVD Mortality 
WONDER_CVD_Mortality_65plus_byRace_a <- WONDER_CVD_Mortality_65plus_byRace %>%
  rename("Race" = `Single Race 6`) %>%
  filter(Race == "Black or African American"| Race == "White") %>%
  dplyr::select(`County Code`, Race, Deaths)

WONDER_CVD_Mortality_65plus_total_a <- WONDER_CVD_Mortality_65plus_total %>%
  dplyr::select(`County Code`, Deaths) %>%
  mutate(Race = "Total Population")

WONDER_CVD_Mortality_65plus <- bind_rows(WONDER_CVD_Mortality_65plus_total_a, WONDER_CVD_Mortality_65plus_byRace_a) %>%
  rename("County_FIPS" = "County Code")

#Get Census Data
ACS_vars_2019 <- load_variables(2019, "acs5", cache = TRUE) 

View(ACS_vars_2019 %>%
  filter(concept == "POPULATION"))

View(ACS_vars_2019 %>%
       filter(str_detect(label, "65")))

County_PopDensity <- get_acs(geography = "county",
        variables = c("total_pop" = "B19037A_053"), #check to make 
        year = 2019,
        survey = "acs5",
        output = "tidy",
        geometry = T,
        state = NEMIA_States) %>%
  mutate(area = st_area(.),
    pop_density =  estimate/area) %>%
  st_drop_geometry() %>%
  dplyr::select(GEOID, pop_density)

County_PopDensity %>% mutate(County_FIPS = str_sub(GEOID, 1,5)) %>% distinct(County_FIPS)

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
                                      year = 2019,
                                      survey = "acs5", 
                                      output = "tidy",
                                      state = NEMIA_States)

ACS_data_tract_race_65plus <- ACS_data_tract_race_65plus %>%
  mutate(variable = str_sub(variable, 1,5)) %>%
  group_by(GEOID, variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(variable = recode(variable,
                       "Black" = "Black or African American",
                       "Total" = "Total Population"))

ACS_data_county_race_65plus <- ACS_data_tract_race_65plus %>%
  mutate(State_FIPS = str_sub(GEOID, 1,2),
         County_FIPS = str_sub(GEOID, 1,5)) %>%
  group_by(State_FIPS, County_FIPS, variable) %>%
  summarise(Population = sum(estimate)) %>%
  ungroup()

#Clean EIA data 

EIA_CDDs_a <- EIA_CDDs %>%
  filter(!is.na(`Jun-19`) & !str_detect(X2, "10-Year Average") & str_detect(X2, "Cooling Degree days")) %>%
  mutate(EIA_estimate_cdds = `Jun-19` + `Jul-19` + `Aug-19`,
         EIA_estimate_cdds = Convert_F_to_C(EIA_estimate_cdds)) %>%
  rename("Climate_Region" = remove) %>%
  dplyr::select(1,10)

EIA_regions <- tibble(Climate_Region = c("New England","New England","New England","New England","New England","New England",
                                         "Middle Atlantic","Middle Atlantic","Middle Atlantic",
                                         "South Atlantic", "South Atlantic","South Atlantic", "South Atlantic","South Atlantic"), 
                      State_FIPS = c("09", "23", "25", "33", "44", "50", 
                                     "34", "36", "42",
                                     "10", "11", "24", "51", "54"))



###First analysis -- compare EIA totals with our estimates

# Merge tract estimates with tract race/ethnicity
Temp_and_Race_byTract <- ACS_data_tract_race_65plus %>%
  inner_join(., Temperatures, by = "GEOID") %>%
  mutate(State_FIPS = str_sub(GEOID, 1,2),
         cooling_dds = w_mean_cdh/24,
         County_FIPS = str_sub(GEOID, 1,5)) %>%
  left_join(., EIA_regions, by = "State_FIPS") 

Temp_and_Race_byTract %>% ##need to modify this for cross race comparison, not against EIA
  group_by(Climate_Region, variable) %>%
  summarise(xgboost_avg_cdds = weighted.mean(cooling_dds, estimate)) %>%
  inner_join(., EIA_CDDs_a, by = "Climate_Region") %>%
  mutate(pct_diff = (xgboost_avg_cdds - EIA_estimate_cdds)/xgboost_avg_cdds)
length(unique(Temp_and_Race_byTract$GEOID))
### Mortality analysis

##County level temp data 
Temp_and_Race_byCounty <- Temp_and_Race_byTract %>%
  group_by(County_FIPS, variable) %>%
  summarise(xgboost_avg_cdds = weighted.mean(cooling_dds, estimate))
length(unique(Temp_and_Race_byCounty$County_FIPS))

#Merge CVD deaths and ACS County data
WONDER_CVD_Mortality_65plus_wCensus <- WONDER_CVD_Mortality_65plus %>%
  left_join(., ACS_data_county_race_65plus, by = c("County_FIPS",
                                                   "Race" = "variable")) %>%
  mutate(Deaths = na_if(Deaths, "Suppressed"),
         deaths = as.numeric(Deaths),
         crude_cvd_mortality = (deaths/Population) * 100000) %>%
  dplyr::select(-Deaths) %>%
  left_join(., County_PopDensity, by = c("County_FIPS" = "GEOID")) %>%
  left_join(., Temp_and_Race_byCounty, by = c("County_FIPS",
                                              "Race" = "variable")) %>%
  filter(!is.na(Population) & !is.nan(xgboost_avg_cdds)) ##double check this is okay because unsure why some are NA -- small subset though

df_for_censored_pois <- WONDER_CVD_Mortality_65plus_wCensus %>%
  mutate(lcensored = if_else(is.na(deaths), TRUE, FALSE),
         status = if_else(lcensored==TRUE, 0, 1),
         cy = if_else(is.na(deaths), 9, deaths),
         # race_dummy = as.factor(Race))
         race_dummy = as.factor(if_else(Race=="Black or African American", 1, 0)))

# with(df_for_censored_pois, table(print(SurvS4(cy, status, type = "left"))))

cens.pois <- vglm(SurvS4(cy, status, type = "left") ~ xgboost_avg_cdds + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
     data = subset(df_for_censored_pois, Race == "Total Population"), trace = T)

# cens.pois.race <- vglm(SurvS4(cy, status, type = "left") ~ xgboost_avg_cdds + race_dummy + xgboost_avg_cdds*race_dummy + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
#                   data = subset(df_for_censored_pois, Race == "Black or African American"|Race == "White"), trace = F)

cens.pois.black <- vglm(SurvS4(cy, status, type = "left") ~ xgboost_avg_cdds + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                              data = subset(df_for_censored_pois, Race == "Black or African American"), trace = T)

cens.pois.white <- vglm(SurvS4(cy, status, type = "left") ~ xgboost_avg_cdds + pop_density + as.factor(State_FIPS) + offset(log(Population)), cens.poisson,
                        data = subset(df_for_censored_pois, Race == "White"), trace = T)

summary(cens.pois.white)
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
                                              "Race" = "variable")) %>%
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


Total_pops_tracts <- ACS_data_tract_race_65plus %>%
  filter(variable == "Total Population") %>%
  rename(total_pop = estimate) %>%
  dplyr::select(GEOID, total_pop)

ACS_data_tract_race_65plus_props <- ACS_data_tract_race_65plus %>%
  filter(variable == "Black or African American") %>%
  left_join(., Total_pops_tracts, by = "GEOID") %>%
  mutate(prop_black = estimate/total_pop, 
         County_FIPS = str_sub(GEOID, 1,5),
         State_FIPS = str_sub(GEOID, 1,2))

pop_65plus_in_region <- Total_pops_tracts %>%
  ungroup() %>%
  summarise(pop_in_NEMIA = sum(total_pop))

ACS_data_tract_race_65plus_props_XGBoost <- ACS_data_tract_race_65plus_props %>%
  inner_join(., Temperatures, by = "GEOID") %>%
  mutate(xgboost_cdds = w_mean_cdh/24,
         total_pop_norm = total_pop/sum(total_pop))

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
