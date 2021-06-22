# Test loading of EPA FAQSD predictions by tract from CONUS cache
library(data.table)
library(lubridate)
library(stringr)

faqsd_path = '/data-coco/CONUS/downloads/faqsd'
load_year = 2005
alt_format = load_year %in% 2015:2016

# PM:
pm_cols_in = (if (alt_format) 
  c("Date", "Loc_Label1", "Prediction") else
    c("Date", "FIPS", "pm25_daily_average(ug/m3)"))
pm_cols_out = c("date", "FIPS", "pm25_pred")

pm_faqsd = fread(file.path(faqsd_path, paste0('output_pm25_', load_year, '.csv.gz')),
                 select = pm_cols_in, 
                 colClasses = list(character = pm_cols_in[1:2], numeric = pm_cols_in[3]))
setnames(pm_faqsd, pm_cols_in, pm_cols_out)
# convert character dates to date type and pad FIPS to 11 characters
pm_faqsd[, date := (if (alt_format) lubridate::mdy else lubridate::ymd)(date)]
pm_faqsd[, FIPS := str_pad(FIPS, 11, side = 'left', pad = '0')]


# ozone:
oz_cols_in = (if (alt_format) 
  c("Date", "Loc_Label1", "Prediction") else
    c("Date", "FIPS", "ozone_daily_8hour_maximum(ppb)")) 
oz_cols_out = c("date", "FIPS", "ozone_pred")

oz_faqsd = fread(file.path(faqsd_path, paste0(load_year, '_ozone_daily_8hour_maximum.csv.gz')),
                 select = oz_cols_in,
                 colClasses = list(character = oz_cols_in[1:2], numeric = oz_cols_in[3]))
setnames(oz_faqsd, oz_cols_in, oz_cols_out)
# convert character dates to date type and pad FIPS to 11 characters
oz_faqsd[, date := (if (alt_format) lubridate::mdy else lubridate::ymd)(date)]
oz_faqsd[, FIPS := str_pad(FIPS, 11, side = 'left', pad = '0')]