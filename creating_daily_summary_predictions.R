library(data.table)
library(fst)
library(future.apply)
library(lubridate)

# Weighting tables
t00weights = read_fst('/data-coco/NEMIA_temperature/weights/nemia_tracts_2000_popdens.fst', as.data.table = TRUE)
t10weights = read_fst('/data-coco/NEMIA_temperature/weights/nemia_tracts_2010_popdens.fst', as.data.table = TRUE)
t00_nldas = read_fst('/data-coco/NEMIA_temperature/weights/nemia_NLDAS_tracts_2000_popdens.fst', as.data.table = TRUE)
t10_nldas = read_fst('/data-coco/NEMIA_temperature/weights/nemia_NLDAS_tracts_2010_popdens.fst', as.data.table = TRUE)


sample_half_month <- read_fst('/data-coco/NEMIA_temperature/saved-predictions/all_monthly/2003_05_h1.fst', as.data.table = T)

start_date = as.Date('2003-05-01')
end_date = as.Date('2019-09-30')
all_dates = seq.Date(start_date, end_date, by = 1)
warm_month_dates = subset(all_dates, format.Date(all_dates, "%m") %in% c("05","06","07","08","09"))
warm_months = unique(format(warm_month_dates, '%Y_%m'))
all_years = unique(format(warm_month_dates, '%Y'))
# tz = "America/New York"

temperatureexcess <- function(temperature, threshold = 18.333){
  pmax(temperature, threshold) - threshold
}
setDTthreads(threads = 15)

# MODIS-based model predictions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

all_files = apply(expand.grid(warm_months, c('h1.fst', 'h2.fst')), MARGIN = 1, 
                  FUN = paste0, collapse = '_')
all_files_2000CTs = sort(all_files)[1:70]
all_files_2010CTs = sort(all_files)[71:170]

path_preds = '/data-coco/NEMIA_temperature/saved-predictions/all_monthly/'
save_hourly_CT_to = "/home/carrid08/northeast_temperature_disparities/data/hourly_tract_preds/"


create_tract_hourly_summaries <- function(fst_file, tract_weights){
  
  temp =  read_fst(file.path(path_preds, fst_file), as.data.table = T)
  
  joined = tract_weights[temp, on = 'gid', nomatch = 0, allow.cartesian = TRUE]
  weighted_hourly_bytract = joined[, .(w_temp = sum(pred.temp.K * popdens * coverage_area, na.rm = T)/
                                         sum(popdens * coverage_area, na.rm = T),
                                       w_temp_heatindex = sum(pred.heat.index.K * popdens * coverage_area, na.rm = T)/
                                         sum(popdens * coverage_area, na.rm = T)),
                                   by = .(GEOID, ground.time.nominal)]
  write_fst(weighted_hourly_bytract, file.path(save_hourly_CT_to, fst_file), compress = 100)
}

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

NEMIA_daily_summaries_all_years <- lapply(all_years, produce_daily_summaries)
NEMIA_daily_summaries_all_years <- rbindlist(NEMIA_daily_summaries_all_years)
write.fst(NEMIA_daily_summaries_all_years, "/home/carrid08/northeast_temperature_disparities/data/summarized_daily_temp_preds.fst", compress = 100)

# NLDAS reanalysis data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

