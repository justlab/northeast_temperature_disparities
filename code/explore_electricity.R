library(tidyverse)
library(lubridate)
library(readxl)
library(Just.universal)
library(sf)
library(here)
library(fst)
library(tidycensus) # remember to set your api key
library(lme4)
library(tabulizer)
library(sjPlot)
#library(splines)
#library(nortest)
devtools::install('/home/rushj03/dev/lqmm_parallel')
library(lqmm)

#### FUNCTIONS ####

# Download a file, update metadata records, and load it with function `f`
# File metadata is stored in a sqlite file, by default in data/downloads/meta.sqlite
download = function(url, to, f, ...){
  f(download.update.meta(url, file.path(data.root, "downloads"), to),
    ...)
}

data.root <- here("data")

# acs5_2017vars <- load_variables("acs5", year = 2017)
# acs5_2011vars <- load_variables("acs5", year = 2011)
# sf1_2010vars <- load_variables("sf1", year = 2010)

# grab the Zip code energy data as a csv
# get the Zip monthly electric dataset from #https://data.ny.gov/Energy-Environment/Utility-Energy-Registry-Monthly-ZIP-Code-Energy-Us/tzb9-c2c6
get.electric <- function() download(
  "https://data.ny.gov/api/views/tzb9-c2c6/rows.csv?accessType=DOWNLOAD&sorting=true",
  to = "electric.csv",
  function(p) read_csv(p)
)

get.zip.to.zcta <- function() download(
  "https://udsmapper.org/wp-content/uploads/2020/09/Zip_to_zcta_crosswalk_2020.xlsx",
  to = "zip_to_zcta_crosswalk.xlsx",
  function(p) read_xlsx(p)
)

get.ny.counties.fips <- function() download(
  "https://data.ny.gov/api/views/79vr-2kdi/rows.csv?accessType=DOWNLOAD&sorting=true",
  to = "NY_couty_fips.csv",
  function(p) read_csv(p)
)

acs.zcta <- function(year) {
  
  ACS_Data <- get_acs(geography = "zcta",
                      state = NULL,
                      geometry = FALSE,
                      variables = c(
                        total_pop1 = "B01003_001",
                        medincome = "B19013_001",
                        fpl_100 = "B06012_002", 
                        fpl_100to150 = "B06012_003",
                        median_rent = "B25031_001",
                        total_hholds1 = "B22003_001",
                        hholds_snap = "B22003_002",
                        unemployed = "B23025_005",
                        hisplat_raceethnic = "B03002_012",
                        nonhispLat_white_raceethnic = "B03002_003",
                        nonhispLat_black_raceethnic = "B03002_004",
                        nonhispLat_amerindian_raceethnic = "B03002_005",
                        nonhispLat_asian_raceethnic = "B03002_006",
                        age65_plus  = "B08101_008",
                        median_rooms = "B25018_001",
                        median_yr_built = "B25035_001",
                        owner_occupied = "B25003_002",
                        renter_occupied = "B25003_003",
                        per_capita_income = "B19301_001"),
                      year = year,
                      output = "wide",
                      survey = "acs5")
  
    ACS_Data <- ACS_Data %>% #only pull out the estimates and cleaning variable names
      mutate(GEOID = stringr::str_sub(GEOID, -5, -1)) %>%
      select(-NAME)  %>%
      select(GEOID, total_pop1M, !ends_with("M")) %>%
      rename_at(vars(ends_with("E")), .funs = list(~stringr::str_sub(., end = -2))) %>%
      mutate(year = year)
  
  return(ACS_Data)
}

get.ny.zcta.temps <- function(){

  zcta_temp_preds <- read_fst(here("data", "summarized_daily_zcta_temp_preds.fst")) %>%
  filter(str_starts(GEOID, "36") & year(date)>=2016)
  
  zcta_temp_preds1 <- zcta_temp_preds %>% filter(month(date)>=5 & month(date)<=9) %>%
    mutate(month = month(date),
           year = year(date))%>%
    group_by(year, month, GEOID) %>%
    summarise(mean_temp = mean(mean_temp_daily),
              mean_htindx = mean(mean_htindx_daily),
              noaa_mean = mean(noaa_mean),
              cdd_total = sum(cdd),
              nighttime_cdh_total = sum(nighttime.cdh)) %>%
    ungroup() %>%
    mutate(ZCTA = str_sub(GEOID, -5))
  
  return(zcta_temp_preds1)

}

get.building.centroids <- function() { #replace w/ Johnathan's code to retrieve and modify from website #"https://usbuildingdata.blob.core.windows.net/usbuildings-v2/NewYork.geojson.zip"
  nyb <- st_read('/data-coco/bldg_footprints/NewYork.shp')
  nyb <- st_transform(nyb, crs = 4269)
  
  ny_buildings_valid <- st_make_valid(nyb) #now make sure all buildings have valid geometries
  ny_buildings_valid1 <- ny_buildings_valid %>% slice(-c(2432950, 2999573, 3488056, 4698742)) #4 of them remain invalid -- deleting them 
  ny_building_valid_centroids <- st_centroid(ny_buildings_valid1) 
  
  return(ny_building_valid_centroids)
}

get.pluto.nyc <- function() {
  download.file("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_19v2_csv.zip", here("data", "pluto_nyc"))
  unzip(here("data", "pluto_nyc"), exdir = here("data", "pluto_files"))
  pluto <- read_csv(here("data","pluto_files", "pluto_19v2.csv"))
  return(pluto)
}

get.energy.grades <- function() {
  pdf <- "https://www1.nyc.gov/assets/buildings/pdf/ll33_Data_Disclosure_2019-CBL.pdf"
  tab <- extract_tables(pdf)
# drop the header on each page and coerce from matrix
  tablist <- lapply(tab, function(x) data.table::data.table(x[-1,]))
  building_grades <- data.table::rbindlist(tablist, idcol = T)
  data.table::setnames(building_grades, c("page", "BBL", "Street_Number", "Street_Name", "Sq_Footage", "Energy_Score", "Energy_Grade"))
# clean up
  rm(pdf, tab, tablist)
  return(building_grades)

}

get.address.dir <- function(){
  download.file("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/pad21c.zip", here("data", "pad_nyc"))
  unzip(here("data", "pad_nyc"), exdir = here("data", "pad_files"))
  pad_file <- read_csv(here("data", "pad_files", "bobaadr.txt")) %>%
    mutate(BBL = paste0(boro, block, lot))
  return(pad_file)
}

get.countyshp.nys <- function() {
  download.file("https://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip", here("data", "counties"))
  unzip(here("data", "counties"), exdir = here("data", "county_shp"))
  county_shp <- st_read(here("data","county_shp", "Counties_Shoreline.shp"))
  county_shp <- st_transform(county_shp, crs = 4269)
  county_shp <- st_make_valid(county_shp)
  return(county_shp)
}

get.zcta.shp <- function(){
  options(timeout = max(300, getOption("timeout")))
  download.file("https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/tl_2019_us_zcta510.zip", here("data", "zctas_us"))
  unzip(here("data", "zctas_us"), exdir = here("data", "zctas_shp"))
  zctas_shp <- st_read(here("data","zctas_shp", "tl_2019_us_zcta510.shp"))
  # zctas_shp <- st_transform(county_shp, crs = 4269)
  zctas_shp <- st_make_valid(zctas_shp)
  
  return(zctas_shp)
}

# get.nycha.ids <- function() { #NYCHA is not in the energy grades dataset 
#   
#   pdf <- "https://www1.nyc.gov/assets/nycha/downloads/pdf/Address-Guide-04-07-2021.pdf"
#   tab <- extract_tables(pdf)
#   tablist <- lapply(tab, function(x) data.table::data.table(x[-1,]))
#   nycha_ids <- data.table::rbindlist(tablist, idcol = T, fill = T)
#   nycha_ids1 <- nycha_ids %>% filter(!is.na(V3)) %>% select(-V10, -V11) #find out the names of the variables 
#   data.table::setnames(nycha_ids1, c("page", "Street_Number", "Street_Name", "Zip_Code", "Development", "Managed_by", "CD", "Block", "Lot", "BIN"))
#   
#   rm(pdf, tab, tablist, nycha_ids)  
#   return(nycha_ids1)
# 
# }

#### DATASETS ####

electric <- get.electric()
zip_to_zcta <- get.zip.to.zcta()
month_temps_zcta <- get.ny.zcta.temps()
ny_buildings <- get.building.centroids()
pluto_nyc <- get.pluto.nyc() 
energy_grades <- get.energy.grades()
bbl_to_zip <- get.address.dir()
nys_counties_shp <- get.countyshp.nys()
zctas_shp <- get.zcta.shp()
# nycha_ids <- get.nycha.ids()
ny_prisons <- read_csv(here("data", "NY_prisons.csv"), col_types = c("ccc"))

##

find_zctas_in_NY_bbox <- function(zctas_shp){
#st_bbox(nys_counties_shp)  
  ny_box <- c(xmin = -80, ymin = 40, xmax = -71, ymax = 46) #rounding/being more inclusive
  zctas_in_NY_bbox1 <- st_crop(zctas_shp, ny_box)

  return(zctas_in_NY_bbox1)
}
zctas_in_NY_bbox <- find_zctas_in_NY_bbox(zctas_shp)


remove_bldgs_without_population <- function(ny_buildings, nys_counties_shp){

  NY_County_FIPS <- get.ny.counties.fips() %>%
    distinct(`County Name`) %>%
    filter(!str_detect(`County Name`, "City")) %>%
    mutate(`County Name` = if_else(`County Name`=="St Lawrence", "St. Lawrence", `County Name`))
  
  Census_blocks <- get_decennial(geography = "block", 
                                 state = "NY", 
                                 county = NY_County_FIPS$`County Name`, #need county
                                 variables = "H001001",
                                 year = 2010, 
                                 geometry = T) 
  
  Census_blocks_withHHolds <- Census_blocks %>% 
    filter(value>0) %>% #only keep blocks with >=1 household
    dplyr::select(GEOID, geometry) 
  
  buildings_in_blocksWhholds <- ny_buildings %>%
    st_join(., Census_blocks_withHHolds, st_intersects) %>% 
    filter(!is.na(GEOID)) %>%
    select(GEOID, geometry)
  
  return(buildings_in_blocksWhholds)
}
buildings_in_blocksWhholds <- remove_bldgs_without_population(ny_buildings, nys_counties_shp)


keep_zctas_touching_NYS <- function(nys_counties_shp, zctas_in_NY_bbox){

  zctas_in_NY_intersection <- st_intersection(nys_counties_shp, zctas_in_NY_bbox)
  return(zctas_in_NY_intersection)
  
}
zctas_in_NY <- keep_zctas_touching_NYS(zctas_shp, nys_counties_shp) #takes a long time

assign_zcta_to_county <- function(zctas_in_NY){
  
  zctas_assigned_to_counties <- zctas_in_NY %>%
  st_transform(., crs = 26918) %>% 
  mutate(area = st_area(.)) %>%
  group_by(ZCTA5CE10) %>%
  slice_max(area) %>%
  select(ZCTA5CE10, NYSP_ZONE, FIPS_CODE, NAME) %>%
  rename("ZCTA" = "ZCTA5CE10") %>%
  st_drop_geometry(.) 

return(zctas_assigned_to_counties)

}
zctas_to_counties <- assign_zcta_to_county(zctas_in_NY)

# zctas_in_NY1 <- zctas_in_NY_bbox %>% 
#   filter(ZCTA5CE10 %in% zctas_to_counties$ZCTA) %>% 
#   st_sf %>%
#   st_cast

assign_building_to_zcta <- function(buildings_in_blocksWhholds, zctas_in_NY_bbox) {
  
  buildings_in_NY_zctas <- st_join(buildings_in_blocksWhholds, zctas_in_NY_bbox, st_intersects) 
  buildings_in_NY_zctas1 <- st_drop_geometry(buildings_in_NY_zctas) %>%
    mutate(building = 1) %>%
    group_by(ZCTA5CE10) %>%
    summarise(buildings_per_zcta = sum(building))
  
  return(buildings_in_NY_zctas1)
  
}
buildings_in_NY_zctas <- assign_building_to_zcta(buildings_in_blocksWhholds, zctas_in_NY_bbox)

rm(buildings_in_NY_zctas, buildings_in_blocksWhholds, ny_buildings, ny_buildings_valid, ny_buildings_valid1, ny_building_valid_centroids)


create_df_for_analysis <- function(electric, month_temps_zcta, zip_to_zcta, zctas_to_counties, month_temps_zcta, buildings_in_NY_zctas, ny_prisons){

  electric_residential <- electric %>% 
    filter(data_class == "electricity" & 
           data_field_display_name == "Residential Consumption (R)") %>%
    mutate(ZIP_CODE = as.character(zip_code))

# zctas_in_NY <- zip_to_zcta %>%
#   filter(STATE =="NY") %>%
#   select(ZCTA) %>%
#   distinct()

  zctas_in_NYC <- zip_to_zcta %>% 
    left_join(., zctas_to_counties, by = "ZCTA") %>%
    filter(STATE =="NY" & (NAME =="Bronx"|NAME =="New York"|NAME =="Queens"|NAME =="Kings"|NAME =="Richmond")) %>%
    select(ZCTA) %>%
    distinct() %>%
    mutate(NYC = "Yes") 
  
  acs_zcta_2016_2019 <- pmap_dfr(.l = list(year=c(2016:2019)), .f = acs.zcta) %>%
    filter(GEOID %in% zctas_in_NY$ZCTA)
  
  electric_res_acs_temp <- electric_res_acs %>% #need to do the ACS quantiling separately
    filter(month>=5 & month<=9) %>%
    left_join(., month_temps_zcta, by = c("year", "month", "ZCTA")) %>%
    left_join(., zctas_in_NYC, by = "ZCTA") %>%
    left_join(., zctas_to_counties, by = "ZCTA") %>%
    left_join(., buildings_in_NY_zctas1, by = c("ZCTA" = "ZCTA5CE10")) %>%
    anti_join(., ny_prisons, by = c("ZCTA" = "Zip")) %>% 
    mutate(NYC = ifelse(is.na(NYC),"No", NYC),
           Region = if_else(NYC == "Yes", "NYC", NYSP_ZONE)) %>%
    filter(value > 0 & !is.na(medincome)) %>%
    mutate(scale_cdd = scale(cdd_total, center = T),
      scale_ntime_cdh = scale(nighttime_cdh_total, center = T),
      ppl_per_hhold = total_pop1/total_hholds1,
           kWh_per_acct = (value/accounts_n)*1000,
           year_month = paste0(year, "_", month),
           prop_black = nonhispLat_black_raceethnic/total_pop1,
           prop_latin = hisplat_raceethnic/total_pop1, 
           prop_black_latin = (nonhispLat_black_raceethnic + hisplat_raceethnic)/total_pop1,
           ppl_per_bldg = total_pop1/buildings_per_zcta,
      ICE_black = (nonhispLat_black_raceethnic - nonhispLat_white_raceethnic)/(total_pop1),
      ICE_POC = ((total_pop1 - nonhispLat_white_raceethnic) - nonhispLat_white_raceethnic)/total_pop1,
      ICE_latino = (hisplat_raceethnic - nonhispLat_white_raceethnic)/total_pop1,
      prop_rental = renter_occupied/total_hholds1,
      median_yr_built_fixed = if_else(median_yr_built == 0, median(median_yr_built), median_yr_built),
      accts_greater_than_pop = if_else(accounts_n>(total_pop1+total_pop1M), 1, 0)) %>%
    filter(accts_greater_than_pop==0)
  
  electric_res_acs_temp1 <- electric_res_acs_temp %>% 
    mutate(ppl_per_bldg_quantile = round(ecdf(electric_res_acs_temp[["ppl_per_bldg"]])(electric_res_acs_temp[["ppl_per_bldg"]]) * 10, 0),
           medincome_quantile = ecdf(electric_res_acs_temp[["medincome"]])(electric_res_acs_temp[["medincome"]]) * 10,
           per_capita_income_quantile = round(ecdf(electric_res_acs_temp[["per_capita_income"]])(electric_res_acs_temp[["per_capita_income"]]) * 10, 1))
  
  median_accounts <- electric_res_acs_temp1 %>%
    group_by(ZCTA) %>%
    summarise(median_accts_n = median(accounts_n)) %>%
    right_join(., electric_res_acs_temp, by = "ZCTA") %>%
    mutate(pct_diff_accts = ((accounts_n - median_accts_n)/(accounts_n + median_accts_n)/2)*100) %>%
    filter(pct_diff_accts < -10)
  
  electric_res_acs_temp2 <- electric_res_acs_temp %>%
    anti_join(., median_accounts, by = c("ZCTA", "year_month"))

  return(electric_res_acs_temp2)
  
}
electric_res_acs_temp <- create_df_for_analysis(electric, month_temps_zcta, zip_to_zcta, zctas_to_counties, month_temps_zcta, buildings_in_NY_zctas, ny_prisons)
### plots of data before analysis 

plot_zctas_in_dataset <- function(electric, zip_to_zcta, zctas_in_NY1){
  
  residential <- electric %>%
    filter(data_class == "electricity" & 
             data_field_display_name == "Residential Consumption (R)") %>%
    mutate(ZIP_CODE = as.character(zip_code))
  
  zips_in_df <- residential %>%
    left_join(., zip_to_zcta, by = "ZIP_CODE") %>%
    distinct(ZIP_CODE, .keep_all = T) %>%
    inner_join(., zctas_in_NY, by = c("ZIP_CODE" = "ZCTA5CE10")) %>%
    st_sf
  
  plot <- ggplot() + geom_sf(data = st_centroid(zctas_in_NY1), color = "grey") + geom_sf(data = st_centroid(zips_in_df), color = "red")
  
  return(plot)
}
plot_zctas_in_dataset(electric, zip_to_zcta)


electric_res_acs_temp %>%
  mutate(zip.year = paste0(ZCTA, ".", year)) %>%
  filter(value>0 & total_pop1>0) %>%
  ggplot(aes(cdd_total, value/total_pop1)) + #cdd_total
  geom_line(aes(group = zip.year), alpha = 0.1) + #utility_display_name
  #geom_boxplot(aes(group = month), alpha = 0.15) + 
  #scale_y_log10() +
  #coord_cartesian(0,1) +
  # ylim(0,1) +
  ylab("MWh/person (log scale)") +
  xlab("Cooling degree days (May-September)") +
  #scale_x_discrete(limits = month.abb) + 
  theme_bw(base_size = 18)

electric_res_acs_temp %>%
  mutate(zip.year = paste0(ZCTA, ".", year)) %>%
  filter(value>0 & total_pop1>0 & year == 2017) %>%
  group_by(year, ZCTA, total_pop1) %>%
  summarise(cdd_total = sum(cdd_total), value = sum(value)) %>%
  #filter(cdd_total>200) %>%
  ggplot(aes(cdd_total, value/total_pop1)) + #cdd_total
  geom_point(alpha = 0.7) + #utility_display_name
  geom_smooth(method = "lm", color = "blue") +
  #scale_x_log10() +
  #coord_cartesian(0,1) +
  # ylim(0,1) +
  ylab("MWh/person") +
  xlab("Cooling degree days (May-September)")+
  #scale_x_discrete(limits = month.abb) + 
  theme_bw(base_size = 18)

electric_res_acs_temp1 %>% 
  filter(value>0 & total_pop1>0) %>%
  ggplot(aes(x = median_grade, y = kWh_per_capita)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(month ~ year)

electric_res_acs_temp1 %>%
  filter(value>0) %>%
  mutate(MwH_per_capita = value/total_pop1) %>%
  ggplot(aes(x = medincome, y = MwH_per_capita)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(month ~ year)

electric_res_acs_temp1 %>%
  distinct(ZCTA, .keep_all = T) %>%
  mutate(MwH_per_capita = value/total_pop1) %>%
  ggplot(aes(x = medincome, y = mean_grade)) +
  geom_point() +
  geom_smooth(method = "loess") 
  facet_wrap(month ~ year)

 electric_res_acs_temp %>%
   filter(value>0) %>% #& month >5 & month<9
    ggplot(aes(cdd_total, kWh_per_capita)) + #cdd_total
    geom_point(aes(color = medincome_tert)) + 
    geom_smooth(method = "lm", color = "grey") + 
    ylab("kWh/person") +
    facet_wrap(vars(month, year), nrow = 5, ncol = 4, scales = "free_x") +
   theme(strip.placement = "outside") +
    theme_minimal()
 
 electric_res_acs_temp %>%
   filter(value>0 & Region == "Long Island") %>% #& month >5 & month<9
   ggplot(aes(cdd_total, log(kWh_per_capita))) + #cdd_total
   geom_point(aes(color = black_latin_tert)) + 
   geom_smooth(method = "lm", color = "grey") + 
   ylab("kWh/person") +
   facet_wrap(vars(month, year), nrow = 5, ncol = 4, scales = "free_x") +
   theme(strip.placement = "outside") +
   theme_minimal()
 
electric_res_acs_temp %>%
   filter(value>0 & Region == "West") %>% #& month >5 & month<9
   ggplot(aes(cdd_total, value/accounts_n)) + #cdd_total
   geom_point(aes(color = black_latin_tert)) + 
   geom_smooth(method = "lm", color = "grey") + 
   ylab("kWh/person") +
   facet_wrap(vars(month, year), nrow = 5, ncol = 4, scales = "free_x") +
   theme(strip.placement = "outside") +
   theme_minimal()

 View(electric_res_acs_temp %>% group_by(month, year) %>% mutate(quant_ten = ifelse(quantile(nighttime_cdh_total, .1), 1, 0)) %>% filter(quant_ten==1))
 
#### Now conducting analysis ####

##Quantile Reg

# library(lqmm)
# oldwd = getwd()
# setwd('/home/rushj03/dev')
# install.packages('lqmm_parallel')
# setwd(oldwd)
# library(lqmm)


#per_capita_income_quantile
#median_rooms

#+ median_rooms + ppl_per_bldg_quantile + per_capita_income_quantile

View(electric_res_acs_temp %>% 
  group_by(ZCTA) %>%
  summarise(length(unique(year_month))))

#Start modeling with lqmm 

lqmm_controls <- lqmmControl(method = "df", LP_tol_ll = 1e-01, LP_tol_theta = 1e-02,
                             check_theta = FALSE, LP_step = NULL, beta = 0.5, gamma = 1,
                             reset_step = FALSE, LP_max_iter = 100000, UP_tol = 1e-01,
                             UP_max_iter = 100000, startQR = T, verbose = F)

lqmm_all <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total + ppl_per_hhold + NAME, tau = 0.5, 
                        random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_income <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*medincome_quantile + ppl_per_hhold + NAME, tau = 0.5, 
                 random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_income1 <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*medincome + ppl_per_hhold + NAME, tau = 0.5, 
                        random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_prop_black <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*prop_black + ppl_per_hhold + NAME, tau = 0.5, 
                        random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

# didnt converge
# lqmm_all_prop_latin <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*prop_latin + ppl_per_hhold + NAME, tau = 0.5, 
#                             random = ~1, group = year_month, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_ICE_black <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*ICE_black + ppl_per_hhold + NAME, tau = 0.5, 
                            random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_ICE_latin <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*ICE_latino + ppl_per_hhold + NAME, tau = 0.5, 
                           random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_ICE_POC <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*ICE_POC + ppl_per_hhold + NAME, tau = 0.5, 
                           random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_income_quadratic <- lqmm(fixed = scale(kWh_per_acct) ~   1 + cdd_total + I(cdd_total^2) + medincome_quantile + medincome_quantile:cdd_total + medincome_quantile:I(cdd_total^2) + NAME, tau = 0.5, 
                        random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_prop_black_quadratic <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total + I(cdd_total^2) + prop_black + prop_black:cdd_total + prop_black:I(cdd_total^2) + NAME, tau = 0.5, 
                            random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_ICE_black_quadratic <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total + I(cdd_total^2) + ICE_black + prop_black:ICE_black + prop_black:I(cdd_total^2) + NAME, tau = 0.5, 
                                      random = ~1, group = year_month, data = electric_res_acs_temp %>% filter(Region != "Long Island"), control = lqmm_controls)


# kWh/acct ~ 1 + CDD + I(CDD^2) + ICE + ICE:CDD + ICE:I(CDD^2) + (1 + CDD + I(CDD^2) | zip)

lqmm_all_summary <- summary(boot.lqmm(lqmm_all, cores = 10))
lqmm_all_income_summary <- summary(boot.lqmm(lqmm_all_income, cores = 10))
lqmm_all_income1_summary <- summary(boot.lqmm(lqmm_all_income1, cores = 10))
lqmm_all_propblack_summary <- summary(boot.lqmm(lqmm_all_prop_black, cores = 10))
# lqmm_all_proplatin_summary <- summary(boot.lqmm(lqmm_all_prop_latin, cores = 8))
lqmm_all_ICEblack_summary <- summary(boot.lqmm(lqmm_all_ICE_black, cores = 10))
lqmm_all_ICElatin_summary <- summary(boot.lqmm(lqmm_all_ICE_latin, cores = 10))
lqmm_all_ICEpoc_summary <- summary(boot.lqmm(lqmm_all_ICE_POC, cores = 10))
lqmm_all_income_quadratic_summary <- summary(boot.lqmm(lqmm_all_income_quadratic, cores = 10))
lqmm_all_prop_black_quadratic_summary <- summary(boot.lqmm(lqmm_all_prop_black_quadratic, cores = 10))
lqmm_all_ICE_black_quadratic_summary <- summary(boot.lqmm(lqmm_all_ICE_black_quadratic, cores = 10))

as_tibble(lqmm_all_income1_summary, rownames = "var") %>% filter(!str_detect(var, "NAME") & !str_detect(var, "scale"))

#interact_plot(lqmm_cdd_percapincomeincome_quantile, pred = scale_cdd, modx = medincome_quantile, modx.values = c(25,75), interval = T, int.type = "confidence", data = electric_res_acs_temp)




