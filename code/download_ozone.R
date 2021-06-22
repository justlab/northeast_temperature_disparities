# Download EPA FAQSD ozone to the same cache directory used to download the PM 2.5
library(Just.universal)

data.root = "/data-coco/CONUS"
dpath = function(...) file.path(data.root, ...)
download.dir = dpath('downloads')
download = function(from, to, ...){
  download.update.meta(from, download.dir, to, ...)}

url_root = "https://ofmpub.epa.gov/rsig/rsigserver?data/FAQSD/outputs/"

for(this_year in 2002:2017){
  download(paste0(url_root, this_year, "_ozone_daily_8hour_maximum.txt.gz"),
           paste0("faqsd/", this_year, "_ozone_daily_8hour_maximum.csv.gz"))
}
