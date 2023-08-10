# get ERA5 reanalysis data for SOM weather typing
# adapted from testECMWF.R
# MAC 04/23/23

library(ecmwfr)

# set a key to the keychain
source("./keychain.R")

##### set up request for data

# which days
days<-format(seq(as.Date("2000/1/1"), as.Date("2000/1/31"), "days"),"%d")
# which years
years<-format(seq(as.Date("2000/1/1"), as.Date("2022/1/31"), "years"),"%Y")
# which months
months<-c("03","04","05","06","07")
# which hours
hours<-c("00:00","06:00","12:00","18:00")
# 

request <- list(
  "dataset_short_name" = "reanalysis-era5-pressure-levels",
  "product_type" = "reanalysis",
  "variable" = "geopotential",
  "pressure_level" = "500",
  "year" = years,
  "month" = months,
  "day" = days,
  "time" = hours,
  "area" = "40/-120/27/-99",
  "format" = "netcdf",
  "target" = "reanalysis-era5_500mb_2000_2022_SWUS_Mar-Jul.nc"
)

# If you have stored your user login information
# in the keyring by calling cds_set_key you can
# call:
file <- wf_request(
  user     = "192992",   # user ID (for authentification)
  request  = request,  # the request
  transfer = TRUE,     # download the file
  path     = "./data/ecmwf"       # store data in current working directory
)

# load downloaded data
library(raster)
# set rasteroptions
rasterOptions(progress = 'text')
# load data
era5<-stack("./data/ecmwf/reanalysis-era5_500mb_2000_2022_SWUS_Mar-Jul.nc")
# convert to geopotential height
era5<-era5/9.80665

# get layer names
layers<-as.data.frame(names(era5))
layers$date<-as.Date(layers$`names(era5)`,format="X%Y.%m.%d.%H.%M.%S")
daysNames<-unique(layers$date)
# get daily average
era5dailyAvg<- stackApply(era5, layers$date, fun = mean)
# add daily names
names(era5dailyAvg)<-daysNames

# save daily average
writeRaster(era5dailyAvg, filename = "./data/ecmwf/dailyAvg_reanalysis-era5_500mb_2000_2022_SWUS_Mar-Jul.grd", overwrite=TRUE)

# test raster
test<-stack("./data/ecmwf/dailyAvg_reanalysis-era5_500mb_2000_2022_SWUS_Mar-Jul.grd")


