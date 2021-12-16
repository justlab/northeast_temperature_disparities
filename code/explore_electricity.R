library(data.table)
library(Just.universal)
library(ggplot2)
library(sf)
library(mapview)
library(here)
library(tidycensus) # remember to set your api key

# Download a file, update metadata records, and load it with function `f`
# File metadata is stored in a sqlite file, by default in data/downloads/meta.sqlite
download = function(url, to, f, ...){
    f(download.update.meta(url, file.path(data.root, "downloads"), to),
        ...)
}

data.root <- here("data")

# grab the Zip code energy data as a csv
# get the Zip monthly electric dataset from #https://data.ny.gov/Energy-Environment/Utility-Energy-Registry-Monthly-ZIP-Code-Energy-Us/tzb9-c2c6
get.electric <- function() download(
    "https://data.ny.gov/api/views/tzb9-c2c6/rows.csv?accessType=DOWNLOAD&sorting=true",
    to = "electric.csv",
    function(p) fread(p)
)
electric <- get.electric()
electric[, rowid := .I]

# how many ZIPs with how many months with non-zero residential data by year?
electric[data_class == "electricity" & 
      data_field == "1_nat_consumption" & 
      value > 0, .N, 
    by = .(zip_code,year)][, table(N, year)]

# who are the data sources?
electric[data_class == "electricity" & 
      data_field == "1_nat_consumption" & 
      value > 0, table(utility_display_name, year)]

# use the Georeference field as Well Known Text to create an sf object for mapping
electricsf <- st_as_sf(electric, wkt = "Georeference", crs = 4326)

# interactive map for the 2016 data -- takes a moment
mapview(electricsf[electricsf$data_class == "electricity" & electricsf$data_field == "1_nat_consumption" & 
      electricsf$value > 0 & electricsf$year == "2016",])

# grab utility service areas: https://data.ny.gov/Energy-Environment/NYS-Electric-Utility-Service-Territories/q5m9-rahr
get.utilities <- function() download(
    "https://data.ny.gov/api/geospatial/q5m9-rahr?method=export&format=Shapefile",
    to = "NYS_electric_utilities.zip",
    function(p) read_sf(paste0("/vsizip/", p))
)
utilities_sf <- get.utilities()

# map with utility service areas on top
mapview(electricsf[electricsf$data_class == "electricity" & electricsf$data_field == "1_nat_consumption" & 
      electricsf$value > 0 & electricsf$year == "2016",]) + mapview(utilities_sf, zcol = "comp_short")

# plot of monthly usage for NYC (ConEd)
# exclude a zip with only 1 account
electric[, zip.year := paste(zip_code, year, sep = ".")]
ggplot(electric[zip_code != 11439 & data_class == "electricity" & data_field == "1_nat_consumption" & 
+       value > 0 & utility_display_name == "Consolidated Edison",], aes(month, value/number_of_accounts)) + 
  geom_line(aes(group = zip.year), alpha = 0.05) + 
  geom_boxplot(aes(group = month), alpha = 0.15) + 
  ylab("MWh/account") + 
  scale_x_discrete(limits = month.abb) + 
  theme_bw()

# distribution of the number of accounts
ggplot(electric[data_class == "electricity" & data_field == "1_nat_consumption" & 
       value > 0,], aes(number_of_accounts)) + geom_histogram() + 
  scale_x_log10()
electric[data_class == "electricity" & data_field == "1_nat_consumption" & 
       value > 0, uniqueN(number_of_accounts), by = zip_code][, table(V1)]

# a campus zip with a single account
electric[data_class == "electricity" & data_field == "1_nat_consumption" & 
           value > 0 & utility_display_name == "Consolidated Edison" & value/number_of_accounts > 5,]

# function to pull 2018 ACS data 
acs.main <- function(admin_unit = c("zcta", "tract"), state_unit = c(NULL, "NY"), sf_shapes = c(TRUE, FALSE)) {
     ACS_Data <- get_acs(geography = admin_unit,
                         state = state_unit,
                         geometry = sf_shapes,
                         variables = c(
                                       total_pop1 = "B01003_001",
                                       medincome = "B19013_001",
                                       fpl_100 = "B06012_002", 
                                       fpl_100to150 = "B06012_003",
                                       median_rent = "B25031_001",
                                       total_hholds1 = "B22003_001",
                                       hholds_snap = "B22003_002",
                                       unemployed = "B23025_005",
                                       under19_noinsurance = "B27010_017",
                                       age19_34_noinsurance = "B27010_033",
                                       age35_64_noinsurance = "B27010_050",
                                       age65plus_noinsurance = "B27010_066",
                                       hisplat_raceethnic = "B03002_012",
                                       nonhispLat_white_raceethnic = "B03002_003",
                                       nonhispLat_black_raceethnic = "B03002_004",
                                       nonhispLat_amerindian_raceethnic = "B03002_005",
                                       nonhispLat_asian_raceethnic = "B03002_006",
                                       age65_plus  = "B08101_008"),
                         year = 2018,
                         output = "wide",
                         survey = "acs5")
     
     if(admin_unit=="zcta"){
       ACS_Data <- ACS_Data %>% #only pull out the estimates and cleaning variable names
         dplyr::mutate(GEOID = stringr::str_sub(GEOID, -5, -1)) %>%
         # filter(GEOID %in% ZCTAs_in_NYC) %>%
         dplyr::select(-NAME)  %>%
         dplyr::select(GEOID, !ends_with("M")) %>%
         dplyr::rename_at(vars(ends_with("E")), .funs = list(~stringr::str_sub(., end = -2)))
     }
     
     if(admin_unit=="tract"){
       ACS_Data <- ACS_Data %>% #only pull out the estimates and cleaning variable names
         # filter(substr(GEOID,1,5) %in% NYC_boro_county_match$fips) %>% # Tracts in NYC counties
         dplyr::select(-NAME)  %>%
         dplyr::select(GEOID, !ends_with("M")) %>%
         dplyr::rename_at(vars(ends_with("E")), .funs = list(~stringr::str_sub(., end = -2)))
     }
     
     return(ACS_Data)
}
acs.dt <- acs.main(admin_unit = "zcta", state_unit = "NY", sf_shapes = F)
setDT(acs.dt)
acs.dt[, zip_code := as.numeric(GEOID),]

# merge the total population into the electricity data
setkey(electric, "zip_code")
setkey(acs.dt, "zip_code")
electric[acs.dt, total_pop1 := total_pop1]

# looks like we will need a population for 11249 
acs.dt[zip_code == 11249, ]
electric[zip_code == 11249 & data_class == "electricity" & 
           data_field == "1_nat_consumption" & value > 0, 
         .(year, month, zip_code, value, number_of_accounts, total_pop1)]

ggplot(electric[zip_code != 11439 & data_class == "electricity" & data_field == "1_nat_consumption" & 
+       value > 0 & utility_display_name == "Consolidated Edison",], aes(month, value/total_pop1)) + 
  geom_line(aes(group = zip.year), alpha = 0.05) + 
  geom_boxplot(aes(group = month), alpha = 0.15) + 
  ylab("MWh per person") + 
  scale_x_discrete(limits = month.abb) + 
  theme_bw()

# end of ACJ file

# beginning of DC's tidyverse version

library(tidyverse)
library(lubridate)
library(readxl)
library(Just.universal)
library(ggplot2)
library(sf)
#library(raster)
#library(rgdal)
#library(mapview)
library(here)
library(fst)
library(tidycensus) # remember to set your api key
library(lme4)
library(furrr)
library(tabulizer)
library(sjPlot)
library(splines)
library(nortest)
library(qrNLMM)

#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#### FUNCTIONS ####

# Download a file, update metadata records, and load it with function `f`
# File metadata is stored in a sqlite file, by default in data/downloads/meta.sqlite
download = function(url, to, f, ...){
  f(download.update.meta(url, file.path(data.root, "downloads"), to),
    ...)
}

data.root <- here("data")

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

NY_County_FIPS <- get.ny.counties.fips() %>%
  distinct(`County Name`) %>%
  filter(!str_detect(`County Name`, "City")) %>%
  mutate(`County Name` = if_else(`County Name`=="St Lawrence", "St. Lawrence", `County Name`))

Census_blocks <- get_decennial(geography = "block", 
                               state = "NY", 
                               county = NY_County_FIPS$`County Name`, #need county
                               variables = "H001001",
                               year = 2010, 
                               geometry = T) #%>%
  #st_transform(., crs = 4326)


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

get.building.data <- function() { #replace w/ Johnathan's code to retrieve and modify from website #"https://usbuildingdata.blob.core.windows.net/usbuildings-v2/NewYork.geojson.zip"
  nyb <- st_read('/data-coco/bldg_footprints/NewYork.shp')
  nyb <- st_transform(nyb, crs = 4269)
  return(nyb)
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

##trying to use the national land use dataset -- CRS is messed up 
# library(mapview)
# 
# for (i in seq_along(list.files(here("data", "NLUD"), full.names = T))){ #import all rasters
# 
#   file_dir <- list.files(here("data", "NLUD"),full.names = T)[i]
#   tmp <- raster(file_dir)
#   raster_name <- paste0("raster_", i)
#   assign(raster_name, tmp)
#   rm(raster_name)
# }

#### DATASETS ####

electric <- get.electric()
zip_to_zcta <- get.zip.to.zcta()
month_temps_zcta <- get.ny.zcta.temps()
ny_buildings <- get.building.data()
pluto_nyc <- get.pluto.nyc() 
energy_grades <- get.energy.grades()
bbl_to_zip <- get.address.dir()
nys_counties_shp <- get.countyshp.nys()
# zcta_shp <- get.zcta.centroid()
zctas_shp <- get.zcta.shp()
# nycha_ids <- get.nycha.ids()
ny_prisons <- read_csv(here("data", "NY_prisons.csv"), col_types = c("ccc"))

##households per zcta 

#get 2010 decenniel census data
#keep only blocks with households 
#intersect with buildings and only keep those that intersect

#1) intersect buildings with blocks

Census_blocks_withHHolds <- Census_blocks %>% 
  filter(value>0) %>% #only keep blocks with >=1 household
  dplyr::select(GEOID, geometry) 

ny_buildings_valid <- st_make_valid(ny_buildings) #now make sure all buildings have valid geometries
 ny_buildings_valid1 <- ny_buildings_valid %>% slice(-c(2432950, 2999573, 3488056, 4698742)) #4 of them remain invalid -- deleting them for now
 ny_building_valid_centroids <- st_centroid(ny_buildings_valid1) #%>% #getting their centroids for computational ease

buildings_in_blocksWhholds <- ny_building_valid_centroids %>%
  st_join(., Census_blocks_withHHolds, st_intersects) %>% #Census_blocks_withHHolds
  filter(!is.na(GEOID)) %>%
  select(GEOID, geometry)

#1) add up all households per zcta

#2) remove all buildings in zero household zctas
st_bbox(nys_counties_shp) #adding more 
ny_box <- c(xmin = -80, ymin = 40, xmax = -71, ymax = 46)
zctas_in_NY_bbox <- st_crop(zctas_shp, ny_box)
zctas_in_NY <- st_intersection(nys_counties_shp, zctas_in_NY_bbox)
# zctas_in_NY <- st_intersection(zctas_in_NY_bbox, nys_counties_shp)
# View(zctas_in_NY)

zctas_to_counties <- zctas_in_NY %>%
  # filter(!is.na(NAME)) %>%
  st_transform(., crs = 26918) %>% 
  mutate(area = st_area(.)) %>%
  group_by(ZCTA5CE10) %>%
  slice_max(area) %>%
  select(ZCTA5CE10, NYSP_ZONE, FIPS_CODE, NAME) %>%
  rename("ZCTA" = "ZCTA5CE10") %>%
  st_drop_geometry(.) 
  # ungroup() %>%
  # bind_rows(., tibble(ZCTA = "10307", NYSP_ZONE = "Long Island", FIPS_CODE = "36085", NAME = "Richmond")) #for some reason, not there

zctas_in_NY1 <- zctas_in_NY_bbox %>% 
  filter(ZCTA5CE10 %in% zctas_to_counties$ZCTA) %>% 
  st_sf %>%
  st_cast

zips_in_df <- electric_residential %>%
  left_join(., zip_to_zcta, by = "ZIP_CODE") %>%
  distinct(ZIP_CODE, .keep_all = T) %>%
  inner_join(., zctas_in_NY1, by = c("ZIP_CODE" = "ZCTA5CE10")) %>%
  st_sf
  # st_geometry(geometry)

ggplot() + geom_sf(data = zctas_in_NY1, fill = "grey") + geom_sf(data = zips_in_df, fill = "red")

zctas_in_NY1 %>% st_centroid() %>% ggplot() + geom_sf()

names(zctas_to_counties)
class(zctas_to_counties$ZCTA)

buildings_in_NY_zctas <- st_join(buildings_in_blocksWhholds, zctas_in_NY_bbox, st_intersects) 
buildings_in_NY_zctas1 <- st_drop_geometry(buildings_in_NY_zctas) %>%
  mutate(building = 1) %>%
  group_by(ZCTA5CE10) %>%
  summarise(buildings_per_zcta = sum(building))

rm(buildings_in_NY_zctas, buildings_in_blocksWhholds, ny_buildings, ny_buildings_valid, ny_buildings_valid1, ny_building_valid_centroids)
#next -- use zcta to county to region geographies for the modeling 

## then make sure the crossing is happening properly

### consider something to account for electricity provider 

electric_residential <- electric %>% 
  filter(data_class == "electricity" & 
           data_field == "1_nat_consumption") %>%
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

electric_res_acs <- electric_residential %>%
  left_join(., zip_to_zcta, by = "ZIP_CODE") %>%
  mutate(value = na_if(value, -999.000)) %>%
  group_by(year, month, ZCTA, utility_display_name, PO_NAME) %>%
  summarise(value = sum(value, na.rm = T),
            accounts_n = sum(number_of_accounts)) %>%
  ungroup() %>%
  inner_join(., acs_zcta_2016_2019, by = c("year", "ZCTA" = "GEOID")) 

electric_res_acs %>%
  mutate(zip.year = paste0(ZCTA, ".", year)) %>%
  filter(value>0 & total_pop1>0) %>%
  ggplot(aes(month, value/accounts_n)) + 
  geom_line(aes(group = zip.year), alpha = 0.05) + 
  geom_boxplot(aes(group = month), alpha = 0.15, color = "blue", width = .5) + 
  scale_y_log10() +
  ylab("MWh/account") +
  xlab("Month") +
  scale_x_discrete(limits = month.abb) + 
  theme_bw(base_size = 18)

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
    kWh_per_capita = (value/total_pop1)*1000,
    ppl_per_hhold = total_pop1/total_hholds1,
         kWh_per_acct = (value/accounts_n)*1000,
         year_month = paste0(year, "_", month),
         prop_black = nonhispLat_black_raceethnic/total_pop1,
         prop_latin = hisplat_raceethnic/total_pop1, 
         prop_black_latin = (nonhispLat_black_raceethnic + hisplat_raceethnic)/total_pop1,
         ppl_per_bldg = total_pop1/buildings_per_zcta,
         medincome_tert = ifelse(medincome < quantile(medincome, .333), "lowest_tertile", 
                                 ifelse(medincome > quantile(medincome, .666), "highest_tertile", "middle_tertile")),
         medincome_tert = factor(medincome_tert, levels = c("highest_tertile", "middle_tertile", "lowest_tertile")),
         medincome_quart = ifelse(medincome < quantile(medincome, .25), "first_quartile", 
                                  ifelse(medincome >= quantile(medincome, .25) & medincome < quantile(medincome, .5), "second_quartile",
                                         ifelse(medincome >= quantile(medincome, .5) & medincome < quantile(medincome, .75),"third_quartile", "fourth_quartile"))),
         medincome_quart = factor(medincome_quart, levels = c("fourth_quartile", "third_quartile", "second_quartile", "first_quartile")),
         black_latin_tert = ifelse(prop_black_latin < quantile(prop_black_latin, .333), "lowest_tertile", 
                                   ifelse(prop_black_latin > quantile(prop_black_latin, .666), "highest_tertile", "middle_tertile")),
         black_latin_tert = factor(black_latin_tert, levels = c("lowest_tertile", "middle_tertile", "highest_tertile")),
         black_latin_categ = ifelse(prop_black_latin < .25, "less than 25% Black & Latin", 
                                   ifelse(prop_black_latin >=.25 & prop_black_latin <.5, "25-50% Black & Latin",
                                          ifelse(prop_black_latin >=.5 & prop_black_latin <.75, "50-75% Black & Latin","75%-100% Black & Latin"))),
         black_latin_categ = factor(black_latin_categ, levels = c("less than 25% Black & Latin", "25-50% Black & Latin", "50-75% Black & Latin", "75%-100% Black & Latin")),
         black_latin_decile = as.numeric(cut(prop_black_latin, quantile(prop_black_latin, probs = seq(0,1,.1), type = 4))),
    prop_rental = renter_occupied/total_hholds1,
    median_yr_built_fixed = if_else(median_yr_built == 0, median(median_yr_built), median_yr_built),
    kWh_per_bldg = (value*1000/buildings_per_zcta),
    accts_greater_than_pop = if_else(accounts_n>(total_pop1+total_pop1M), 1, 0)) %>%
  filter(accts_greater_than_pop==0)

electric_res_acs_temp <- electric_res_acs_temp %>% 
  mutate(ppl_per_bldg_quantile = round(ecdf(electric_res_acs_temp[["ppl_per_bldg"]])(electric_res_acs_temp[["ppl_per_bldg"]]) * 10, 0),
         medincome_quantile = ecdf(electric_res_acs_temp[["medincome"]])(electric_res_acs_temp[["medincome"]]) * 10,
         per_capita_income_quantile = round(ecdf(electric_res_acs_temp[["per_capita_income"]])(electric_res_acs_temp[["per_capita_income"]]) * 10, 1))

median_accounts <- electric_res_acs_temp %>%
  group_by(ZCTA) %>%
  summarise(median_accts_n = median(accounts_n)) %>%
  right_join(., electric_res_acs_temp, by = "ZCTA") %>%
  mutate(pct_diff_accts = ((accounts_n - median_accts_n)/(accounts_n + median_accts_n)/2)*100) %>%
  filter(pct_diff_accts < -10)

electric_res_acs_temp <- electric_res_acs_temp %>%
  anti_join(., median_accounts, by = c("ZCTA", "year_month"))

ggplot(median_accounts, aes(x = year_month, y = pct_diff_accts)) + geom_point() + geom_line(aes(color = Region, group = ZCTA)) 
ggplot(electric_res_acs_temp, aes(x = ))

# electric_res_acs_temp <- electric_res_acs_temp %>%
#   group_by(ZCTA) %>%
#   summarise(obs = n()) %>%
#   right_join(., electric_res_acs_temp, by = "ZCTA") %>%
#   filter(obs == 20)
# group_by(Region) %>%
  # mutate(black_latin_tert = ifelse(prop_black_latin < quantile(prop_black_latin, .333), "lowest_tertile", 
  #                                ifelse(prop_black_latin > quantile(prop_black_latin, .666), "highest_tertile", "middle_tertile"))) %>%
  # ungroup()

electric_res_acs_temp %>%
  mutate(zip.year = paste0(ZCTA, ".", year)) %>% #is this still right? 
  #filter(value>0 & total_pop1>0 & NYC =="Yes") %>%
  ggplot(aes(cdd_total, kWh_per_capita)) + #cdd_total
  geom_line(aes(color = NYC, group = zip.year), alpha = 0.1) + 
  #geom_boxplot(aes(group = month), alpha = 0.15) + 
  scale_y_log10() +
  #ylim(0,1) +
  ylab("kWh/person") +
  #scale_x_discrete(limits = month.abb) + 
  theme_bw()

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

# electric_res_acs_temp1 <- electric_res_acs_temp %>%
#   filter(NYC == "Yes") %>%
#   left_join(., energy_grades2, by = "ZCTA") %>%
#   filter(value > 0) %>%
#   mutate(kWh_per_capita = (value/total_pop1)*1000,
#          medincome_tert = ifelse(medincome < quantile(medincome, .333), "lowest_tertile", 
#                                  ifelse(medincome > quantile(medincome, .666), "highest_tertile", "middle_tertile")))

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
 
#univariate relationships 
electric_res_acs_temp %>%
  group_by(year_month) %>%
  summarise(n())

ggplot(data = electric_res_acs_temp, aes(x = as.factor(month), y = nighttime_cdh_total)) + geom_boxplot() + xlab("Month (all years, 2016-2019)")
ggplot(data = electric_res_acs_temp, aes(x = as.factor(month), y = cdd_total)) + geom_boxplot() + xlab("Month (all years, 2016-2019)")

ggplot(data = electric_res_acs_temp, aes(x = nighttime_cdh_total, y = kWh_per_capita)) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month, scales = "free_x")+ xlab("Nighttime Cooling Degree Hours")
ggplot(data = electric_res_acs_temp, aes(x = nighttime_cdh_total, y = kWh_per_acct)) + geom_point() + geom_smooth(method = "loess") + facet_grid(Region~month, scales = "free")+ xlab("Nighttime Cooling Degree Hours") 


ggplot(data = electric_res_acs_temp, aes(x = log(ppl_per_bldg), y = log(kWh_per_capita), fill = as.factor(month))) + geom_point() + geom_smooth(method = "loess") + xlab("People per building (using Microsoft buildings)")
ggplot(data = electric_res_acs_temp, aes(x = log(ppl_per_bldg), y = log(kWh_per_acct), fill = as.factor(month))) + geom_point() + geom_smooth(method = "loess") + xlab("People per building (using Microsoft buildings)")
ggplot(data = electric_res_acs_temp, aes(x = ppl_per_bldg_quantile, y = log(kWh_per_acct), fill = as.factor(month))) + geom_point() + geom_smooth(method = "loess") + xlab("People per building (using Microsoft buildings)")

ggplot(data = electric_res_acs_temp, aes(x = log(ppl_per_hhold), y = log(kWh_per_acct), fill = as.factor(month))) + geom_point() + geom_smooth(method = "loess") + xlab("People per household (Census)")

ggplot(data = electric_res_acs_temp, aes(x = median_rooms, y = log(kWh_per_acct), fill = as.factor(month))) + geom_point() + geom_smooth(method = "loess") + xlab("Median rooms per household (Census)")

ggplot(data = electric_res_acs_temp, aes(x = as.factor(utility_display_name), y = log(kWh_per_capita))) + geom_boxplot() + xlab("Utility company")
ggplot(data = electric_res_acs_temp, aes(x = as.factor(utility_display_name), y = log(kWh_per_acct))) + geom_boxplot() + xlab("Utility company")

ggplot(data = electric_res_acs_temp, aes(x = as.factor(month), y = log(kWh_per_capita))) + geom_boxplot() + facet_grid(~Region) + xlab("Month (all years)")
ggplot(data = electric_res_acs_temp, aes(x = as.factor(month), y = log(kWh_per_acct))) + geom_boxplot() + facet_grid(~Region) + xlab("Month (all years)")

ggplot(data = electric_res_acs_temp, aes(x = prop_black_latin, y = log(kWh_per_capita))) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month) + xlab("Proportion Black & Latino")
ggplot(data = electric_res_acs_temp, aes(x = prop_black_latin, y = log(kWh_per_acct))) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month) + xlab("Proportion Black & Latino")
ggplot(data = electric_res_acs_temp, aes(x = black_latin_decile, y = log(kWh_per_acct))) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month) + xlab("Proportion Black & Latino")

ggplot(data = electric_res_acs_temp, aes(x = medincome_quantile, y = log(kWh_per_capita))) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month) + xlab("Median Income (quantile transform)")
ggplot(data = electric_res_acs_temp, aes(x = medincome_quantile, y = log(kWh_per_acct))) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month) + xlab("Median Income (quantile transform)")
ggplot(data = electric_res_acs_temp, aes(x = per_capita_income_quantile, y = log(kWh_per_acct))) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month) + xlab("Per Capita Income (quantile transform)")

ggplot(data = electric_res_acs_temp %>% filter(median_yr_built>0), aes(x = median_yr_built, y = log(kWh_per_acct))) + geom_point() + geom_smooth(method = "loess") + facet_grid(~month) + xlab("Median year structure built")

ggplot(data = electric_res_acs_temp, aes(x = prop_rental, y = log(kWh_per_acct))) + geom_point() + geom_smooth(method = "lm") + facet_grid(~month) + xlab("Proportion households that are rentals")

View(electric_res_acs_temp %>% select(ZCTA, total_pop1, total_hholds1, owner_occupied, renter_occupied))

#all NYS using Census pops to normalize
lmer_energy_cdds_census <- lmer(log(kWh_per_capita) ~ 1 + scale(nighttime_cdh_total, center = T) + utility_display_name + ppl_per_bldg + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp) # %>% filter(month >5 & month<9)
summary(lmer_energy_cdds_census)
plot(lmer_energy_cdds_census)
hist(residuals(lmer_energy_cdds_census))
qqnorm(residuals(lmer_energy_cdds_census))
##looks BADDDDD

#Now using all NYS using account # to normalize #nighttime_cdh_total -- RANDOM INTERCEPTS ONLY

lmer_energy_cdhs_accounts <- lmer(log(kWh_per_acct) ~ 1 + scale_ntime_cdh + utility_display_name + median_rooms + ppl_per_bldg_quantile + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp)
lmer_energy_cdds_accounts <- lmer(log(kWh_per_acct) ~ 1 + scale_cdd + as.factor(utility_display_name) +  ns(ppl_per_bldg_quantile) + ns(median_yr_built) + (1|NAME) + (1 |year), data = electric_res_acs_temp) # %>% filter(month>5 & month<9)
#median_rooms 
# + ppl_per_bldg_quantile
#median_rooms + median_yr_built_fixed


# lmer_energy_cdds_accounts <- lmer(log(kWh_per_acct) ~ 1 + scale_cdd + utility_display_name + ns(median_rooms) + as.factor(ppl_per_bldg_quantile) + bs(ppl_per_hhold) + per_capita_income_quantile + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp) # %>% filter(month>5 & month<9)
cor.test(electric_res_acs_temp$scale_cdd, electric_res_acs_temp$scale_ntime_cdh)
fixed_eff_lm <- lm(log(kWh_per_acct) ~ scale_cdd + utility_display_name + ns(median_rooms) + as.factor(ppl_per_bldg_quantile) + bs(ppl_per_hhold) + per_capita_income_quantile + as.factor(NAME) + as.factor(year), data = electric_res_acs_temp)
qqnorm(residuals(fixed_eff_lm));qqline(residuals(fixed_eff_lm))

summary(lmer_energy_cdds_accounts)
plot(lmer_energy_cdds_accounts)
hist(residuals(lmer_energy_cdds_accounts))
qqnorm(residuals(lmer_energy_cdds_accounts));qqline(residuals(lmer_energy_cdds_accounts))
plot_model(lmer_energy_cdds_accounts, type = "diag")
plot_model(lmer_energy_cdhs_accounts, type = "diag")
tab_model(lmer_energy_cdds_accounts)

resids <- as.tibble(x = residuals(lmer_energy_cdds_accounts)) %>% rename(residuals = value)
residuals_to_explore <- bind_cols(electric_res_acs_temp, resids) 
residuals_to_explore <- residuals_to_explore %>%
  group_by(ZCTA) %>%
  summarise(observations = n()) %>%
  right_join(., residuals_to_explore, by = "ZCTA")

residuals_to_explore %>% ggplot() + geom_boxplot(aes(as.factor(year_month), residuals)) + geom_hline(yintercept = c(.5, -.5), linetype = 2)
residuals_to_explore %>% ggplot() + geom_boxplot(aes(as.factor(Region), residuals)) + geom_hline(yintercept = c(.25, -.75), linetype = 2)
residuals_to_explore %>% ggplot() + geom_boxplot(aes(as.factor(NAME), residuals)) + geom_hline(yintercept = c(.25, -.75), linetype = 2) + facet_wrap(~Region, scales = "free_x")
residuals_to_explore %>% ggplot(aes(ppl_per_bldg_quantile, residuals)) + geom_point() + geom_smooth(method = "loess") + geom_hline(yintercept = c(.5, -.5), linetype = 2)
residuals_to_explore %>% ggplot(aes(observations, residuals)) + geom_point() + geom_smooth(method = "lm") + geom_hline(yintercept = c(.5, -.5), linetype = 2)
residuals_to_explore %>% ggplot(aes(medincome_quantile, residuals)) + geom_point() + geom_smooth(method = "loess") + geom_hline(yintercept = c(.5, -.5), linetype = 2)
residuals_to_explore %>% ggplot(aes(prop_black_latin, residuals)) + geom_point() + geom_smooth(method = "loess") + geom_hline(yintercept = c(.5, -.5), linetype = 2)
residuals_to_explore %>% ggplot(aes(log(kWh_per_acct), residuals)) + geom_point() + geom_smooth(method = "lm") + geom_hline(yintercept = c(.5, -.5), linetype = 2)
residuals_to_explore %>% ggplot(aes(total_pop1, residuals)) + geom_point() + geom_smooth(method = "lm") + geom_hline(yintercept = c(.5, -.5), linetype = 2) 

ad.test(residuals(lmer_energy_cdds_accounts))


resids_for_ggplot <- residuals_to_explore %>% 
  filter(residuals>.25 | residuals< -.75) %>% select(ZCTA, Region, NAME, scale_cdd, year, month, year_month, residuals) %>% 
  group_by(ZCTA) %>% 
  summarise(times = n())
View(resids_for_ggplot %>% group_by(ZCTA) %>% summarise(times = n()))

resids_for_ggplot1 <- zctas_in_NY %>% left_join(., resids_for_ggplot, by = c("ZCTA5CE10" = "ZCTA")) %>% 
  mutate(times = ifelse(is.na(times), 0, times),
         times_w_large_resids = ifelse(times==0, "0",
                                       ifelse(times>0 & times<=5, ">0 & <=5",
                                              ifelse(times>5 & times<=10, ">5 & <=10",
                                                     ifelse(times>10 & times<=15, ">10 & <=15", ">15")))),
         times_w_large_resids = factor(times_w_large_resids, levels = c("0", ">0 & <=5", ">5 & <=10", ">10 & <=15",">15"), ordered = T))

ggplot() + geom_sf(data = resids_for_ggplot1, aes(fill = as.factor(times_w_large_resids)), lwd = 0) + scale_fill_manual(values = c("white", "blue", "green", "orange", "red")) + geom_sf(data = nys_counties_shp, fill = NA) + theme_minimal()

smaller_pop_zctas <- electric_res_acs_temp %>% filter(total_pop1 < 15000) %>% distinct(ZCTA)
ggplot() + geom_sf(data = zctas_shp) + 
  geom_sf(data = zctas_shp %>% filter(ZCTA5CE10 %in% smaller_pop_zctas$ZCTA5CE10), color = "red")
 
tab_model(lmer_energy_cdhs_accounts)


# library(robustlmm)
# robust_cdh_lmer <- rlmer(log(kWh_per_acct) ~ 1 + nighttime_cdh_total + utility_display_name + ppl_per_bldg_quantile + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp)
# plot(robust_cdh_lmer)
# ad.test(residuals(robust_cdh_lmer))
# hist(residuals(robust_cdh_lmer))


##Quantile Reg
library(lqmm)

lqmm_controls <- lqmmControl(method = "gs", LP_tol_ll = 1e-07, LP_tol_theta = 1e-07,
            check_theta = FALSE, LP_step = NULL, beta = 0.5, gamma = 1,
            reset_step = FALSE, LP_max_iter = 100000, UP_tol = 1e-07,
            UP_max_iter = 100000, startQR = T, verbose = F)

lqmm_cdd <- lqmm(fixed = log(kWh_per_acct) ~ 1 + scale_cdd + as.factor(utility_display_name) + log(ppl_per_hhold), tau = 0.5, 
     random = ~1, group = NAME, data = electric_res_acs_temp, control = lqmm_controls)
summary(lqmm_cdd)

lqmm_cdd_medincome_tert <- lqmm(fixed = log(kWh_per_acct) ~ 1 + scale_cdd*medincome_tert + as.factor(utility_display_name) + log(ppl_per_hhold), tau = 0.5, 
                 random = ~1, group = NAME, data = electric_res_acs_temp, control = lqmm_controls)
summary(lqmm_cdd_medincome_tert)

lqmm_cdd_medincome_quart <- lqmm(fixed = log(kWh_per_acct) ~ 1 + scale_cdd*medincome_quart + as.factor(utility_display_name) + log(ppl_per_hhold), tau = 0.5, 
                                random = ~1, group = NAME, data = electric_res_acs_temp, control = lqmm_controls)
summary(lqmm_cdd_medincome_quart)

lqmm_cdd_medincome_quantile <- lqmm(fixed = log(kWh_per_acct) ~ 1 + scale_cdd*medincome_quantile + as.factor(utility_display_name) + log(ppl_per_hhold), tau = 0.5, 
                                 random = ~1, group = NAME, data = electric_res_acs_temp, control = lqmm_controls)
summary(lqmm_cdd_medincome_quantile)

lqmm_cdd_percapincomeincome_quantile <- lqmm(fixed = log(kWh_per_acct) ~ 1 + scale_cdd * per_capita_income_quantile + 
                                               as.factor(utility_display_name) + log(ppl_per_hhold), random = ~1, 
                                             group = NAME, tau = 0.5, data = electric_res_acs_temp, control = lqmm_controls)
summary(lqmm_cdd_percapincome_quantile)

interact_plot(lqmm_cdd_percapincomeincome_quantile, pred = scale_cdd, modx = medincome_quantile, modx.values = c(25,75), interval = T, int.type = "confidence", data = electric_res_acs_temp)

#per_capita_income_quantile
#median_rooms

#+ median_rooms + ppl_per_bldg_quantile + per_capita_income_quantile
coef(lqmm_cdd)
ranef(lqmm_cdd)
summary(lqmm_cdd)

#Now using all NYS using account # to normalize #nighttime_cdh_total -- RANDOM INTERCEPTS & SLOPES
lmer_energy_cdds_accounts1 <- lmer(log(kWh_per_acct) ~ 1 + scale_ntime_cdh + utility_display_name + ppl_per_bldg + (scale(cdd_total, center = T)|Region/NAME) + (scale(cdd_total, center = T)|year_month), data = electric_res_acs_temp) # %>% filter(month >5 & month<9)
summary(lmer_energy_cdds_accounts1)
plot(lmer_energy_cdds_accounts1)
hist(residuals(lmer_energy_cdds_accounts1))
qqnorm(residuals(lmer_energy_cdds_accounts1))
plot_model(lmer_energy_cdds_accounts1, type = "diag")
tab_model(lmer_energy_cdds_accounts1)
##Doesnt look good! 


#Now try an interaction w/income #black_latin_tert #prop_black_latin #as.factor(medincome) #scale(medincome) #scale(nighttime_cdh_total, center = T)
lmer_energy_cdds_accounts_interact_income <- lmer(log(kWh_per_acct) ~ scale_ntime_cdh*medincome_tert +  utility_display_name + as.factor(median_rooms) + ns(ppl_per_bldg_quantile) + ns(ppl_per_hhold) + prop_rental +(1|Region) + (1|year_month), data = electric_res_acs_temp) # %>% filter(month >5 & month<9)
lmer_energy_cdds_accounts_interact_income <- lmer(log(kWh_per_acct) ~ scale_cdd*medincome_tert + as.factor(utility_display_name) + log(ppl_per_hhold) + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp) 
summary(lmer_energy_cdds_accounts_interact_income)
plot(lmer_energy_cdds_accounts_interact_income)
hist(residuals(lmer_energy_cdds_accounts_interact_income))
qqnorm(residuals(lmer_energy_cdds_accounts_interact_income));qqline(residuals(lmer_energy_cdds_accounts_interact_income))
plot_model(lmer_energy_cdds_accounts_interact_income, type = "diag")
tab_model(lmer_energy_cdds_accounts_interact_income)
interact_plot(lmer_energy_cdds_accounts_interact_income, pred = scale_ntime_cdh, modx = medincome_tert)
interact_plot(lmer_energy_cdds_accounts_interact_income, pred = scale_cdd, modx = medincome_tert, interval = T, int.type = "confidence", data = electric_res_acs_temp)

lmer_energy_cdds_accounts_interact_income1 <- lmer(log(kWh_per_acct) ~ scale_cdd*medincome_quart + as.factor(utility_display_name)  + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp) # %>% filter(month >5 & month<9)
tab_model(lmer_energy_cdds_accounts_interact_income1)
interact_plot(lmer_energy_cdds_accounts_interact_income1, pred = scale_ntime_cdh, modx = medincome_quart)
interact_plot(lmer_energy_cdds_accounts_interact_income1, pred = scale_cdd, modx = medincome_quart, interval = T, int.type = "confidence", data = electric_res_acs_temp)

lmer_energy_cdds_accounts_interact_income2 <- lmer(log(kWh_per_acct) ~ 1 + scale_cdd*medincome_quantile + as.factor(utility_display_name) + log(ppl_per_hhold) + (1|Region) + (1|year_month), data = electric_res_acs_temp) # %>% filter(month >5 & month<9)
qqnorm(residuals(lmer_energy_cdds_accounts_interact_income2));qqline(residuals(lmer_energy_cdds_accounts_interact_income2))
plot_model(lmer_energy_cdds_accounts_interact_income2, type = "diag")
tab_model(lmer_energy_cdds_accounts_interact_income2)

interact_plot(lmer_energy_cdds_accounts_interact_income2, pred = scale_cdd, modx = medincome_quantile, modx.values = c(25,75), interval = T, int.type = "confidence", sec.axis = cdd_total, data = electric_res_acs_temp) +
  theme(text = element_text(size = 25)) + xlab("Cooling degree days (scaled)") + ylab("kWh per account (natural log)") 
scale_y_continuous(sec.axis = electric_res_acs_temp$cdd_total)

##black latin
lmer_energy_cdds_accounts_interact_raceethnic <- lmer(log(kWh_per_acct) ~ 1 + scale_ntime_cdh*black_latin_tert + utility_display_name + ppl_per_bldg + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp) # %>% filter(month >5 & month<9)
tab_model(lmer_energy_cdds_accounts_interact_raceethnic)
interact_plot(lmer_energy_cdds_accounts_interact_raceethnic, pred = scale_ntime_cdh, modx = black_latin_tert)

lmer_energy_cdds_accounts_interact_raceethnic1 <- lmer(log(kWh_per_acct) ~ 1 + scale_ntime_cdh*black_latin_categ + utility_display_name + ppl_per_bldg + (1|Region/NAME) + (1|year_month), data = electric_res_acs_temp) # %>% filter(month >5 & month<9)
tab_model(lmer_energy_cdds_accounts_interact_raceethnic1)
interact_plot(lmer_energy_cdds_accounts_interact_raceethnic1, pred = scale_ntime_cdh, modx = black_latin_categ)

install.packages("interactions")
library(interactions)


View(electric_res_acs_temp %>% 
  group_by(ZCTA) %>%
  summarise(length(unique(year_month))))

# median(electric_res_acs_temp$nighttime_cdh_total)
# median(electric_res_acs_temp$ppl_per_bldg, na.rm = T)
# 
# medians_for_preds <- electric_res_acs_temp %>% 
#   group_by(Region, NAME, year) %>% 
#   summarise(nighttime_cdh_total = median(nighttime_cdh_total, na.rm = T),
#             ppl_per_bldg = median(ppl_per_bldg)) 
# 
# prediction_df_low_POC <- electric_res_acs_temp %>% 
#   mutate(prop_black_latin = 0,
#          POC = "Low") %>%
#   select(ZCTA, utility_display_name, Region, NAME, year_month, year, month, POC) %>%
#   left_join(., medians_for_preds, by = c("Region", "NAME", "year"))
# 
# prediction_df_hi_POC <- electric_res_acs_temp %>% 
#   mutate(prop_black_latin = .9,
#          POC = "High") %>%
#   select(ZCTA, utility_display_name, Region, NAME, year_month, year, month, POC) %>%
#   left_join(., medians_for_preds, by = c("Region", "NAME", "year")) 
# 
# prediction_df_low_POC1 <- bind_cols(prediction_df_low_POC, mWh_per_acct = predict(lmer_energy_cdds_accounts_interact, prediction_df_low_POC))
# prediction_df_hi_POC1 <- bind_cols(prediction_df_hi_POC, mWh_per_acct = predict(lmer_energy_cdds_accounts_interact, prediction_df_hi_POC))
# prediction_df <- bind_rows(prediction_df_hi_POC1, prediction_df_low_POC1)
# 
#  prediction_df %>%
#   filter(month == 7, year == 2018) %>%
#   ggplot() + geom_line(aes(nighttime_cdh_total, mWh_per_acct, color = POC)) %>%
#   facet_grid(NAME)

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
##NYC specific focus
lmer_energy_cdds_NYC <- lmer(log(value/accounts_n) ~ 1 + scale(nighttime_cdh_total, center = F)*prop_black_latin + ppl_per_bldg + (1|NAME) + (1|year_month), data = electric_res_acs_temp %>% filter(Region == "NYC")) # %>% filter(month >5 & month<9)
summary(lmer_energy_cdds_NYC)
plot(lmer_energy_cdds_NYC)
hist(residuals(lmer_energy_cdds_NYC))
qqnorm(residuals(lmer_energy_cdds_NYC))
tab_model(lmer_energy_cdds_NYC)
plot_model(lmer_energy_cdds_NYC, type = "diag")


#### Exploring energy grades ####

energy_grades1 <- energy_grades %>%
  # left_join(., bbl_to_zip, by = "BBL")
  left_join(., pluto_nyc %>% select(bbl, zipcode) %>% mutate(BBL = as.character(bbl)), by = "BBL") %>% #60 out of 20333 zips missing 
  left_join(., zip_to_zcta %>% mutate(zipcode = as.numeric(ZIP_CODE)), by = "zipcode") 

#approx how many grades do I have per zcta?

energy_grades2 <- energy_grades1 %>% 
  mutate(energy_score = as.numeric(Energy_Score)) %>% #NAs by coercion is what I want
  filter(!is.na(energy_score)) %>% 
  group_by(ZCTA) %>% 
  summarise(grades_per_zcta = length(unique(BBL)),
            mean_grade = mean(energy_score),
            median_grade = median(energy_score))




#year/admin
acs5_2017vars <- load_variables("acs5", year = 2017)
acs5_2011vars <- load_variables("acs5", year = 2011)
sf1_2010vars <- load_variables("sf1", year = 2010)


library(spatialreg)
library(spdep)
library(MuMIn)
# electric_res_acs_temp1 <- zctas_in_NY %>% 
#   rename("ZCTA" = "ZCTA5CE10") %>%
#   select(ZCTA, NAME) %>%
#   full_join(., electric_res_acs_temp, by = "ZCTA")

mean_august_logkwh <- electric_res_acs_temp %>%
  filter(month == 8) %>%
  group_by(ZCTA) %>%
  summarise(mean_aug_electric = mean(log(kWh_per_acct))) %>%
  inner_join(., electric_res_acs_temp %>% filter(month==8) %>% distinct(ZCTA, .keep_all = T), by = "ZCTA")

aug_zctas <- electric_res_acs_temp %>%
  filter(ZCTA %in% mean_august_logkwh$ZCTA)

# lmer_energy_cdds_accounts_aug <- lmer(log(kWh_per_acct) ~ 1 + scale_cdd + as.factor(utility_display_name) + median_rooms + ppl_per_bldg_quantile + (1|NAME) + (1|year), data = aug_zctas) # %>% filter(month>5 & month<9)
# plot(lmer_energy_cdds_accounts_aug)
# qqnorm(residuals(lmer_energy_cdds_accounts_aug));qqline(residuals(lmer_energy_cdds_accounts_aug))

electric_res_acs_temp1 <- zctas_in_NY1 %>% 
  distinct(ZCTA5CE10, .keep_all = T) %>% 
  st_centroid(.) %>%
  rename("ZCTA" = "ZCTA5CE10") %>%
  select(ZCTA, geometry) %>%
  inner_join(., mean_august_logkwh, by = "ZCTA") #only zctas we have data for 

    # full_join(., electric_res_acs_temp, by = "ZCTA")

# electric_res_acs_temp1 <- electric_res_acs_temp1 %>%
#    st_sf %>%
#    st_cast 
# 
# st_geometry(electric_res_acs_temp1) <- st_collection_extract(x = st_geometry(electric_res_acs_temp1), 
#                                            type = "POLYGON")
electric_res_acs_temp1 <- as(electric_res_acs_temp1, "Spatial")
# spdat.sens <- as_Spatial(zctas_in_NY1$geometry)
ny.nb6 <- knearneigh(electric_res_acs_temp1, k=6) #sp::coordinates(electric_res_acs_temp1)
ny.nb6 <- knn2nb(ny.nb6)
ny.nb6 <- make.sym.nb(ny.nb6)
ny.wt6 <- nb2listw(ny.nb6, style="W", zero.policy = T)
plot(ny.nb6, coordinates(electric_res_acs_temp1))

fit.lm.ny.sens <- lm(mean_aug_electric ~ scale_cdd + utility_display_name + ppl_per_bldg_quantile, electric_res_acs_temp1)
lm.morantest(fit.lm.ny.sens, listw = ny.wt6, zero.policy = T)
# me.fit.sens <- spatialreg::ME(log(kWh_per_acct) ~ scale_cdd + as_factor(year_month), 
#                               spdat.sens@data, family=negative.binomial(fit.lmer.ny.sens$theta), listw = ny.wt6, verbose=T, alpha=.1, nsim = 999)
me.fit.sens <- spatialreg::SpatialFiltering(mean_aug_electric ~ scale_cdd + utility_display_name + ppl_per_bldg_quantile, 
                              data = electric_res_acs_temp1@data, nb = ny.nb6, zero.policy = T, ExactEV = F, na.action = , verbose = T, alpha = .05)
spatial_vectors <- bind_cols(electric_res_acs_temp1@data %>% select(ZCTA), as_tibble(me.fit.sens$dataset))
electric_res_acs_temp2 <- electric_res_acs_temp %>% inner_join(., spatial_vectors, by = "ZCTA") %>%
  mutate(ICE_black = (nonhispLat_black_raceethnic - nonhispLat_white_raceethnic)/(nonhispLat_black_raceethnic + nonhispLat_white_raceethnic),
         ICE_POC = ((hisplat_raceethnic+nonhispLat_black_raceethnic) - nonhispLat_white_raceethnic)/(nonhispLat_black_raceethnic + nonhispLat_white_raceethnic + hisplat_raceethnic),
         ICE_latino = (hisplat_raceethnic - nonhispLat_white_raceethnic)/(hisplat_raceethnic + nonhispLat_white_raceethnic + hisplat_raceethnic),
         latin_tert = if_else(prop_latin<=.333, "first_tertile", 
                              if_else(prop_latin>.333 & prop_latin<=.666, "second_tertile", "third_tertile")),
         latin_tert = factor(latin_tert, levels = c("first_tertile", "second_tertile", "third_tertile")))

lmer_cdd_per_acct_spfilter <- lmer(kWh_per_acct ~ 1 + scale_cdd + as.factor(utility_display_name) +  ppl_per_bldg_quantile + median_yr_built + as.factor(ZCTA) +
                                       + (1 |Region) + (1|year), na.action = "na.fail", data = electric_res_acs_temp2)



lmer_cdd_per_acct_spfilter_west <- lmer(kWh_per_acct ~ 1 + scale_cdd +  ppl_per_hhold + median_yr_built + (1|NAME) +
                                      + (1|year_month), na.action = "na.fail", data = electric_res_acs_temp2 %>% filter(Region=="West"|Region=="Central"))

lmer_cdd_per_acct_spfilter_nyc <- lmer(kWh_per_acct ~ 1 + scale_cdd +  ppl_per_bldg_quantile + median_yr_built + 
                                      + (1|year), na.action = "na.fail", data = electric_res_acs_temp2 %>% filter(Region=="NYC"))

lmer_cdd_per_acct_spfilter_north <- lmer(kWh_per_acct ~ 1 + scale_cdd  +  ppl_per_bldg_quantile + median_yr_built +
                                      + (1|year), na.action = "na.fail", data = electric_res_acs_temp2 %>% filter(Region=="East"))



broom::tidy(lm(kWh_per_acct ~ scale_cdd, data = electric_res_acs_temp2), conf.int = T)
summary(lmer_cdd_per_acct_spfilter)
#scale_ntime_cdh +
#+ ppl_per_hhold +
#vec3 + vec20 + vec2 + vec15 + vec31 + vec17 + vec12 + vec23 + vec11 + vec9 + vec37 + vec44 + vec4 + vec56 + vec84 + vec1 + 
#vec5 + vec40 + vec6 + vec38 + vec28 + vec47 + vec66 + vec33
#+ vec51 + vec49 + vec21
#vec72 + vec48 + vec13 + vec18 + 
# vec98 + vec94 + vec35 + vec30 + vec97 + vec95 + vec104 + vec76 ++ vec19

sub_vars <- electric_res_acs_temp2 %>% dplyr::select(kWh_per_acct, scale_cdd, scale_ntime_cdh, ppl_per_hhold, buildings_per_zcta, median_rooms, median_yr_built, ppl_per_bldg_quantile, vec3:vec76, Region, year)
all_vars <- do.call(c, lapply(seq_along(sub_vars), combn, x = sub_vars, simplify = FALSE)) 

# lmer_basemodel <- lmer(log(kWh_per_acct) ~ ., data = sub_vars)
lmer_dredge <- dredge(lmer_cdd_per_acct_spfilter, subset = xor(scale_cdd, scale_ntime_cdh), extra = c(resid_ad_stat = function(x) ad.test(residuals(x))$statistic[[1]]))
hist(electric_res_acs_temp2$kWh_per_acct)
?expand.grid
plot(lmer_cdd_per_acct_spfilter)
qqnorm(residuals(lmer_cdd_per_acct_spfilter));qqline(residuals(lmer_cdd_per_acct_spfilter))
ad.test(residuals(lmer_cdd_per_acct_spfilter))$statistic[[1]]
#moran.test(residuals(lmer_cdd_per_acct_spfilter), listw = ny.wt6)



# from example(glmmTMB)
# m2 <- glmmTMB(count ~ spp + mined + (1|site), family=nbinom2, data=Salamanders)
# models <- get.models(dredge(m2), TRUE)
# models <- dredge(m2)

##########
lqmm_controls <- lqmmControl(method = "df", LP_tol_ll = 1e-01, LP_tol_theta = 1e-02,
                             check_theta = FALSE, LP_step = NULL, beta = 0.5, gamma = 1,
                             reset_step = FALSE, LP_max_iter = 100000, UP_tol = 1e-01,
                             UP_max_iter = 100000, startQR = T, verbose = F)

lqmm_west <- lqmm(fixed = scale(kWh_per_acct) ~ 1 + cdd_total + ppl_per_hhold + ppl_per_bldg_quantile + NAME, tau = 0.5, 
                 random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region=="Central"|Region=="West"), control = lqmm_controls)
summary(lqmm_west)
residuals(lqmm_west)

lqmm_nyc <- lqmm(fixed = scale(kWh_per_acct) ~ 1 + cdd_total* + ppl_per_hhold + ppl_per_bldg_quantile + NAME, tau = 0.5, 
                                random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region=="NYC"|Region=="Long Island"), control = lqmm_controls)
summary(lqmm_nyc)

lqmm_north <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total + ppl_per_hhold + ppl_per_bldg_quantile + NAME, tau = 0.5, 
                                 random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region=="East"), control = lqmm_controls)
summary(lqmm_north)

lqmm_all <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total + ppl_per_hhold + NAME, tau = 0.5, 
                   random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)

lqmm_all_income <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*medincome_quantile + ppl_per_hhold + NAME, tau = 0.5, 
                 random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_income <- summary(lqmm_all_income)

lqmm_all_prop_black <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*prop_black + ppl_per_hhold + NAME, tau = 0.5, 
                        random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_prop_black <- summary(lqmm_all_prop_black)

lqmm_all_prop_black1 <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*prop_black + ppl_per_bldg_quantile + NAME, tau = 0.5, 
                            random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_prop_black1 <- summary(lqmm_all_prop_black)

lqmm_all_ICE_black <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*ICE_black + ppl_per_hhold + NAME, tau = 0.5, 
                            random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_ICE_black <- summary(lqmm_all_ICE_black)

lqmm_all_ICE_black1 <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*ICE_black + ppl_per_bldg_quantile + NAME, tau = 0.5, 
                           random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_ICE_black1 <- summary(lqmm_all_ICE_black)
# bootstrap_lqmm_black1 <- extractBoot(lqmm_all_ICE_black1, which = "fixed")

lqmm_all_ICE_lat <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*ICE_latino + ppl_per_hhold + NAME, tau = 0.5, #not converging
                         random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_ICE_lat <- summary(lqmm_all_ICE_lat)

lqmm_all_ICE_POC <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*ICE_POC + ppl_per_hhold + NAME, tau = 0.5, 
                           random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_ICE_POC <- summary(lqmm_all_ICE_POC)

lqmm_all_tert_lat <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total*latin_tert + ppl_per_bldg_quantile + NAME, tau = 0.5, 
                         random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)
summary_lqmm_all_tert_lat <- summary(lqmm_all_tert_lat)

# lqmm_all <- lqmm(fixed = scale(kWh_per_acct) ~  1 + cdd_total + ppl_per_bldg_quantile + Region, tau = 0.5, 
#                  random = ~1, group = year, data = electric_res_acs_temp2 %>% filter(Region != "Long Island"), control = lqmm_controls)

residuals_lqmm <- bind_rows(
  bind_cols(electric_res_acs_temp2 %>% filter(Region=="Central"|Region=="West") %>% select(ZCTA, year, month, year_month, kWh_per_capita, kWh_per_acct, accounts_n, value), 
            tibble(resid = residuals(lqmm_west))),
  bind_cols(electric_res_acs_temp2 %>% filter(Region=="NYC"|Region=="Long Island") %>% select(ZCTA, year, month, year_month, kWh_per_capita, kWh_per_acct, accounts_n, value), 
            tibble(resid = residuals(lqmm_nyc))),
  bind_cols(electric_res_acs_temp2 %>% filter(Region=="East") %>% select(ZCTA, year, month, year_month, kWh_per_capita, kWh_per_acct, accounts_n, value),
            tibble(resid = residuals(lqmm_north))))

View(residuals_lqmm %>%
  group_by(ZCTA) %>%
  summarise(mean_resids = max(resid)))





##try lmer model dataset with lqmm

fm1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
fm2 <- lqmm(Reaction ~ 1 + Days, random = ~1, group = Subject, data = sleepstudy)
QRLMM(y = sleepstudy$Reaction, x = sleepstudy$Days, z =  groups = sleepstudy$Subject)
summary(lmer(extra~group + (1|ID), data = sleep))
summary(lqmm(extra~ 1 + group, random = ~1, group = ID, data = sleep))


data("Orthodont")
attach(Orthodont)
y = distance
x = cbind(1,c(rep(0,64), rep(1,44)), age)
z = cbind(1, age)
groups = Subject
QRLMM(y,x,z,groups,MaxIter=200)


View(cross(electric_res_acs_temp %>% names(), electric_res_acs_temp %>% names()))

all_interactions <- electric_res_acs_temp2 %>%
  
  expand.grid(names(electric_res_acs_temp2)[-1], 
                                 (names(electric_res_acs_temp2)[-1])) %>%
  filter(Var1 != Var2) %>% 
  mutate(interact = paste0("(", Var1, " * ", Var2, ")")) %>% 
  pull(interact) %>% 
  paste0(sep = " + ") %>% 
  paste0(collapse = "") %>% 
  substr(1, nchar(.)-3)

electric_res_acs_temp2 %>% 
  dplyr::select(buildings_per_zcta, Region, ppl_per_bldg_quantile, vec3:vec76) %>%  # exclude outcome, leave only predictors 
  map(~lm(Fair$nbaffairs ~ .x + (1 |Region) + (1|year) + data = Fair)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  dplyr::arrange(desc(x)) %>% 
  rename(r.squared = x)

View(electric_res_acs_temp2 %>% slice(8864, 3533, 10871, 10637, 6389, 4063))

View(electric_res_acs_temp2 %>%
  filter(ZCTA=="13045"|ZCTA=="13692"|ZCTA=="14464"|ZCTA=="12841"|ZCTA=="13353"|ZCTA=="12993"))

View(electric_res_acs_temp2 %>%
       filter(ZCTA=="13666"|ZCTA=="13141"|ZCTA=="14783"))

resid_plot <- electric_res_acs_temp2 %>%
  mutate(high_resid = ifelse(ZCTA=="13045"|ZCTA=="13692"|ZCTA=="14464"|ZCTA=="12841", "high_resid", "normal")) %>%
  select(ZCTA, total_pop1, accounts_n, high_resid)
ggplot(data = resid_plot, aes(x = total_pop1, y = accounts_n, color = high_resid)) + geom_point() + geom_smooth(method = "lm")
ggplot() + geom_sf(data = zctas_in_NY1) + geom_sf(data = zctas_in_NY1 %>% filter(ZCTA5CE10==13141), color = "red")

avg_region_obs <- electric_res_acs_temp2 %>% group_by(Region, month) %>% summarise(mean_kWh_per_acct = mean(kWh_per_acct),
                                                                      median_kWh_per_acct = median(kWh_per_acct))

compare_kwh_per_cap_to_avg <- electric_res_acs_temp2 %>% 
  select(ZCTA, Region, year_month, year, month, accounts_n, total_pop1, total_hholds1, value, kWh_per_acct) %>%
  left_join(., avg_region_obs, by = c("Region", "month")) %>%
  mutate(pct_diff_region_mean = (kWh_per_acct - mean_kWh_per_acct)/((kWh_per_acct + mean_kWh_per_acct)/2)*100,
         pct_diff_region_median = (kWh_per_acct - median_kWh_per_acct)/((kWh_per_acct + median_kWh_per_acct)/2)*100) 
  
  
#finding ways to identify outliers 
plotly::ggplotly(ggplot(electric_res_acs_temp2,aes(x = total_pop1, y = accounts_n)) + geom_point() + geom_smooth(method = "lm"))

#+ warning=FALSE
# Step 2c: Pull out these fits and visualize the autocorrelation
fits.sens <- data.frame(fitted(me.fit.sens))
spdat.sens$me16 <- fits.sens$vec16
spdat.sens$me23 <- fits.sens$vec23
spplot(spdat.sens, "me16", at=quantile(spdat.sens$me16, p=seq(0,1,length.out = 7)))
spplot(spdat.sens, "me23", at=quantile(spdat.sens$me23, p=seq(0,1,length.out = 7)))

# suggest_crs(raster_10)
# 
# combined_raster <- raster::merge(raster_10, raster_11)
# # combined_raster_4269 <- projectRaster(combined_raster, crs = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
# # raster11_4269 <- projectRaster(raster_11, crs = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
# # mapView(combined_raster)
# 
# states <- read_sf(here("data", "states", "tl_2017_us_state.shp")) %>% filter(DIVISION != "0" & (STATEFP != "02"&STATEFP != "15"))
# 
# states_raster1 <- states %>%
#   st_transform(., crs = st_crs(raster_1)) %>%
#   as_Spatial(.) 
# 
# mapView(states_raster1)
# 
# plot(combined_raster)
# plot(states_raster1)
# 
# raster1_df <- as.data.frame(raster_1, xy = T)
# 
# ny_state_shp <- states %>%
#   filter(STATE == "NY") %>%
#   st_transform(., crs = st_crs(combined_raster))
# 
# # combined_raster_df <- as.data.frame(combined_raster, xy = T)
# 
# mapView(ny_state_shp$geometry)
# mapView(combined_raster)
# plot(combined_raster, add = T)
# mapView(raster_1)
# 
# st_crs(ny_state_shp)
# 
# ggplot() +
#   geom_raster(data = combined_raster, aes())


# get.land.use.data <- function()
#  download.file("https://landscape.blm.gov/CHD_2012_layerpackages/CHD_DV_C_luc2010.lpk", 
#                destfile = here("data", "downloads", "CHD_DV_C_luc2010.lpk"))
# unzip(zipfile = here("data", "downloads", "CHD_DV_C_luc2010.lpk"), exdir = here("data", "downloads"))

# land_use <- system.file(here("data", "land_use", "CHD_DV_C_luc2010.rrd"), package="rgdal")
# land_use_raster <- raster(land_use)
# land_use_raster <- readGDAL(here("data", "land_use", "CHD_DV_C_luc2010.rrd"),)
# land_use_raster <- readGDAL(system.file(here("data", "CHD_DV_C_luc2010", "commondata", "raster_data", "CHD_DV_C_luc2010.img"), package = "rgdal"))
# land_use_raster <- readGDAL(system.file("/home/carrid08/northeast_temperature_disparities/data/CHD_DV_C_luc2010/v10/0000CHD_DV_C_luc2010_img.lyr", package = "rgdal")[1])
#1188

# r = raster(filename)
# crs(r) = "+init=epsg:4326" # fix broken CRS
# r = projectRaster(r, crs="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# rm(ny_buildings_valid)
#
# buildings_in_blocksWhholds <- ny_building_valid_centroids %>%
#   st_intersects(., Census_blocks_withHHolds, sparse = T) %>% #Census_blocks_withHHolds
#   tibble(census_block = .) %>%
#   mutate(id_num = as.integer(census_block)) %>%
#   filter(!is.na(census_block))

### now buildings to zctas 

#   tibble(census_block = .) %>%
#   mutate(id_num = as.integer(census_block)) %>%
#   filter(!is.na(census_block))
# buildings_in_blocksWhholds %>% filter(is.na(Value))  
# 
# buildings_in_blocks1 <- buildings_in_blocks %>%
#   mutate(building = 1) %>%
#   filter(!is.na(census_block)) %>%
#   group_by(id_num) %>%
#   summarise(bldgs_in_blocks = sum(building)) 
#   
# Census_blocks_numbldgs <- Census_blocks_withHHolds %>%
#   left_join(., buildings_in_blocks1, by = "id_num") %>%
#   filter(is.na(bldgs_in_blocks))



# ggplot() +
#   geom_sf(data = Census_blocks_withHHolds, color = "grey") +
#   geom_sf(data = ny_buildings_in_resblocks, color = "red")



#library(raster)
# img = raster(here("data", "CHD_DV_C_luc2010", "commondata", "raster_data", "CHD_DV_C_luc2010.img"))
# table(img[]) # will show many more values for 128 than 1
# NAvalue(img) <- 128
# table(img[]) # now shows only 1 values
# # the default plot(img) is a terrible tiny shape, but you can get a reduced-resolution preview with:
# library(mapview)
# mapview(img)