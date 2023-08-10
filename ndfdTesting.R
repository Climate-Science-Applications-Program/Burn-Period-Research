# testing NDFD download
# using https://www.ncei.noaa.gov/thredds/ncss/model-ndfd-file/202306/20230606/YRUZ98_KWBN_202306062353/dataset.html
# MAC 6/17/23

library(raster)
library(ncdf4)

ncfname<-"./data/ndfd/20230606_YRUZ98_KWBN_202306062353.nc"

# open a netCDF file https://pjbartlein.github.io/REarthSysSci/netCDF.html
ncin <- nc_open(ncfname)
print(ncin)

time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")

test<-stack("./data/ndfd/20230606_YRUZ98_KWBN_202306062353.nc")



library(rNOMADS)

gribMeta<-GribInfo("./data/ndfd/rtma2p5_ru.t0000z.2dvaranl_ndfd.grb2", file.type = "grib2")
   levels<-c("2 m above ground")
   #variables<-c("TMP","DPT")
   variables<-c("DPT")
data<-ReadGrib("./data/ndfd/rtma2p5_ru.t0000z.2dvaranl_ndfd.grb2",levels,variables)

library(raster) 
xyz<-cbind.data.frame(data$lon, data$lat,data$value)
# c(-115,-102.8,31,37.1)
xyz<-subset(xyz, `data$lon`<=(-102) & `data$lon`>=(-115) & `data$lat`>=31 & `data$lat`<=37.1)
  colnames(xyz)<-c('x','y','z')
rst <- rasterFromXYZ(xyz)

   
# directly read grb2 with raster
# look https://nomads.ncep.noaa.gov/pub/data/nccf/com/rtma/prod/
# 
grbData<-stack("./data/ndfd/rtma2p5_ru.t0000z.2dvaranl_ndfd.grb2")
tempDpt<-stack(grbData[[4:3]])
   tempDpt <- projectRaster(tempDpt, crs="+init=epsg:4326")
   tempDpt<-crop(tempDpt, extent(-115,-102.8,31,37.1))
# calculate rel hum
   rh <- overlay(tempDpt, fun=function(x,y) weathermetrics::dewpoint.to.humidity(x,y,temperature.metric = "celsius"))

   
#### THIS WORKS for calculating burn hours from URMA data 06/18/23      
# try downloading URMA data and processing RH
# https://nomads.ncep.noaa.gov/pub/data/nccf/com/urma/prod/urma2p5.20230617/
   url <- "https://nomads.ncep.noaa.gov/pub/data/nccf/com/urma/prod/urma2p5.20230617/"
   filenames = RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
   filenames<-as.data.frame(XML::getHTMLLinks(filenames))
      colnames(filenames)<-'name'
   filenames<-as.data.frame(filenames[which(stringr::str_detect(filenames$name, '2dvaranl')==TRUE),])
      colnames(filenames)<-'name'
      filenames$name<-as.character(filenames$name)
      
   rhStack<-stack() 
   ptm <- proc.time()
   for(i in 1:nrow(filenames)){
      download.file(paste0(url,filenames$name[i]), "./data/ndfd/temp.grb2", mode = 
                       "wb")
      grbData<-stack("./data/ndfd/temp.grb2")
      tempDpt<-stack(grbData[[4:3]])
      tempDpt <- projectRaster(tempDpt, crs="+init=epsg:4326")
      tempDpt<-crop(tempDpt, extent(-115,-102.8,31,37.1))
      # calculate rel hum
      rhStack <-stack(rhStack,overlay(tempDpt, fun=function(x,y) weathermetrics::dewpoint.to.humidity(x,y,temperature.metric = "celsius")))
      print(i)
   }   
   proc.time() - ptm

   burnHours<-sum(rhStack<=20)

