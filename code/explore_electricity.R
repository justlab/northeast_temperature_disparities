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


#### Load Data ####
data.root <- here("data")

# grab the Zip code energy data as a csv
# get the Zip monthly electric dataset from #https://data.ny.gov/Energy-Environment/Utility-Energy-Registry-Monthly-ZIP-Code-Energy-Us/tzb9-c2c6
get.electric.zip <- function() download(
    "https://data.ny.gov/api/views/tzb9-c2c6/rows.csv?accessType=DOWNLOAD&sorting=true",
    to = "electric_zip.csv",
    function(p) {
      dat <- fread(p)
      dat <- dat[data_class == "electricity" & 
      data_field == "1_nat_consumption" & 
      value > 0]
    })
electric.zip <- get.electric.zip()

# grab the city/town/village energy data as a csv
# get the monthly town/county electric dataset from #https://data.ny.gov/Energy-Environment/Utility-Energy-Registry-Monthly-ZIP-Code-Energy-Us/tzb9-c2c6
get.electric.town <- function() download(
    "https://data.ny.gov/api/views/m3xm-q3dw/rows.csv?accessType=DOWNLOAD&sorting=true",
    to = "electric_town.csv",
    function(p) {
      dat <- fread(p)
      dat <- dat[data_class == "electricity" & 
      data_field == "1_nat_consumption" & 
      value > 0]
    })
electric.town <- get.electric.town()

# grab utility service areas: https://data.ny.gov/Energy-Environment/NYS-Electric-Utility-Service-Territories/q5m9-rahr
get.utilities <- function() download(
    "https://data.ny.gov/api/geospatial/q5m9-rahr?method=export&format=Shapefile",
    to = "NYS_electric_utilities.zip",
    function(p) read_sf(paste0("/vsizip/", p))
)
utilities.sf <- get.utilities()

# grab civil boundaries for cities/towns and villages
# http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927
get.civil.boundaries <- function() download(
    "http://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip",
    to = "NYS_civil_boundaries.shp.zip",
    function(p) {
      unzip(p, exdir = file.path(data.root, "downloads"))
    })
civil.boundaries.sf <- get.civil.boundaries()
villages.sf <- read_sf(file.path(data.root, "downloads", "Villages.shp"))
towns.sf <- read_sf(file.path(data.root, "downloads", "Cities_Towns.shp"))


#### data exploration - ZIPs ####

# how many ZIPs with how many months with non-zero residential data by year?
electric.zip[, .N, 
    by = .(zip_code,year)][, table(N, year)]

# who are the data sources?
electric.zip[, table(utility_display_name, year)]

# use the Georeference field as Well Known Text to create an sf object for mapping
electric.zip.sf <- st_as_sf(electric.zip, wkt = "Georeference", crs = 4326)

# interactive map for the 2016 data -- takes a moment
mapview(electric.zip.sf[electric.zip.sf$year == "2016",])


# map with utility service areas on top
mapview(electric.zip.sf[electric.zip.sf$year == "2016",], zcol = "utility_display_name") + mapview(utilities.sf, zcol = "comp_short")

# plot of monthly usage for NYC (ConEd)
# exclude a zip with only 1 account
electric.zip[, zip.year := paste(zip_code, year, sep = ".")]
ggplot(electric.zip[zip_code != 11439 & utility_display_name == "Consolidated Edison",], aes(month, value/number_of_accounts)) + 
  geom_line(aes(group = zip.year), alpha = 0.05) + 
  geom_boxplot(aes(group = month), alpha = 0.15) + 
  ylab("MWh/account") + 
  scale_x_discrete(limits = month.abb) + 
  theme_bw()

# distribution of the number of accounts
ggplot(electric.zip, aes(number_of_accounts)) + geom_histogram(bins=50, aes(fill = utility_display_name)) + 
  scale_x_log10()

# a campus zip with a single account
electric.zip[utility_display_name == "Consolidated Edison" & value/number_of_accounts > 5,]

# among zips with incomplete records
# which months are present/missing before 2020? is there a pattern in the month(s) of suppression?
electric.zip[year < 2020, uniqueN(month), by = zip.year][V1 < 12][,table(V1)]

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
setkey(electric.zip, "zip_code")
setkey(acs.dt, "zip_code")
electric.zip[acs.dt, total_pop1 := total_pop1]

# looks like we will need a population for 11249 
acs.dt[zip_code == 11249, ]
electric.zip[zip_code == 11249 & data_class == "electricity" & 
           data_field == "1_nat_consumption" & value > 0, 
         .(year, month, zip_code, value, number_of_accounts, total_pop1)]

ggplot(electric.zip[zip_code != 11439 & utility_display_name == "Consolidated Edison",], aes(month, value/total_pop1)) + 
  geom_line(aes(group = zip.year), alpha = 0.05) + 
  geom_boxplot(aes(group = month), alpha = 0.15) + 
  ylab("MWh per person") + 
  scale_x_discrete(limits = month.abb) + 
  theme_bw()

#### data exploration - towns ####

# First electric record
electric.town.first <- electric.town[J(unique(full_fips)), mult = "first", on = "full_fips"]
# how many of each type of civil unit?
electric.town.first[, table(com_type)]

# how many unique geographic entities?
electric.town[, uniqueN(com_name)]# 1136
# but note that some have more than one unique fips
electric.town[, uniqueN(full_fips)]# 1334
electric.town[, .(uniqueN(full_fips)), by = com_name][V1 > 1][, table(V1)]
# distribution of length of full_fips
electric.town[, table(nchar(unique(full_fips)))]

# use the Georeference field as Well Known Text to create an sf object for mapping
# note that 1 Town (com_name == "Ava") doesn't have a Georeference
electric.town.sf <- st_as_sf(electric.town[com_name != "Ava", ], wkt = "Georeference", crs = 4326)
mapview(electric.town.sf[electric.town.sf$year == 2016,], col.regions = "red") + mapview(electric.zip.sf[electric.zip.sf$year == "2016",])

ggplot(electric.town, aes(number_of_accounts)) + geom_histogram(bins=50, aes(fill = utility_display_name)) + 
  scale_x_log10(labels = scales::comma)

# end of file