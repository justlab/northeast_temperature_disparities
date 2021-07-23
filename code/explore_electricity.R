library(data.table)
library(Just.universal)
library(ggplot2)
library(sf)
library(mapview)
library(here)

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

# plot with utility service areas on top
mapview(electricsf[electricsf$data_class == "electricity" & electricsf$data_field == "1_nat_consumption" & 
      electricsf$value > 0 & electricsf$year == "2016",]) + mapview(utilities_sf, zcol = "comp_short")

# end of file