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

# end of file