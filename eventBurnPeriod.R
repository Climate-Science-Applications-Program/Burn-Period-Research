# Event burn period analysis
# MAC 04/30/2023

library(raster)
library(dplyr)

#####
# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")
# work with  FOD dataframe only
fc<-fc@data
# remove factors
fc[,] <- lapply(fc, function(x) type.convert(as.character(x), as.is = TRUE))
# adjust dates on fc DF  
fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,"%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,"%m/%d/%Y")
#####

##### load MODIS FRP data -- produced by frpAnalysis.R
load("~/RProjects/BurnPeriodResearch/data/M-C61_FRP_MODIS_SW.RData")
#####

# extract MTBS data - only do once
# dirs<-list.dirs(path = "./data/mtbs/az", full.names = TRUE, recursive = TRUE)
# for(i in 2:length(dirs)){
#   zipF <- list.files(path = dirs[i], pattern = "*.zip", full.names = TRUE)
#   # unzip all your files
#   plyr::ldply(.data = zipF, .fun = unzip, exdir = dirs[i])
# }

# look for shapefile
i<-9
dirs<-list.dirs(path = "./data/mtbs/az", full.names = TRUE, recursive = TRUE)
shpF <- list.files(path = dirs[i], pattern = "*bndy.shp", full.names = TRUE)
shpF2 <- list.files(path = dirs[i], pattern = "*bndy.shp", full.names = FALSE)

# load in shapefile
perim <- rgdal::readOGR(dsn = dirs[i], layer = tools::file_path_sans_ext(shpF2[1]))
perim<-spTransform(perim, CRS("+proj=longlat +datum=WGS84"))
perim$year<-as.numeric(format(as.Date(perim$Ig_Date, format="%Y/%m/%d"),"%Y"))

# get discovery and containment dates
discDate<-min(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name & 
                       fc$FIRE_YEAR==perim@data$year),"DISCOVERY_DATE"])
contDate<-max(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name &
                       fc$FIRE_YEAR==perim@data$year),"CONT_DATE"])
#contDate<-as.Date("2002-07-01")

# subset modis FRP in event dates
modisFRP<-subset(modiscrop,ACQ_DATE>=discDate & ACQ_DATE<=contDate )
colnames(modisFRP)[1:2]<-c("Latitude","Longitude")

# convert frp to spatial points
coordinates(modisFRP)<- ~ Longitude + Latitude
proj4string(modisFRP) <- proj4string(perim)
# subset points to perim
temp<-sp::over(modisFRP, perim)
modisFRP<-modisFRP[-(which(is.na(temp$Event_ID)==TRUE)),]
# create FRP time stamps
modisFRP$DateTimeUTC<-as.POSIXct(paste0(modisFRP$ACQ_DATE," ",modisFRP$ACQ_TIME), format="%Y-%m-%d %H%M")
modisFRP$DateTimeAZ7=as.POSIXct(format(modisFRP$DateTimeUTC,tz="America/Phoenix"))
modisFRP$DateAZ<-as.Date(format(modisFRP$DateTimeAZ7, "%Y-%m-%d"))
#plot(perim)
#plot(modisFRP, add=TRUE)

# get daily summed FRP
dailyFRP<-modisFRP@data %>% 
  group_by(DateAZ) %>%
  summarize(sumFRP=sum(FRP),
            maxFRP=max(FRP),
            countFRP=n(),
            nightSumFRP=sum(FRP[DAYNIGHT=="N"]),
            nightMaxFRP=max(FRP[DAYNIGHT=="N"]),
            nightCountFRP=sum(DAYNIGHT=="N"))

library(ggplot2)
ggplot(modisFRP@data, aes(DateTimeAZ7,FRP))+
  geom_point()

ggplot(dailyFRP, aes(DateAZ,maxFRP))+
  geom_point()

# get burn period stats
# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# get station list from burnList
stations<-do.call(rbind, lapply(burnList,function(x) x[1,c("STA_NAME","LATITUDE","LONGITUDE")]))
# get lat/lon for look up
geoStns<-stations[,c("LONGITUDE","LATITUDE")]
# get center of fire polyong
fireCenter<-rgeos::gCentroid(perim[1],byid=TRUE)@coords
# lookup closet RAWS
distances<-geosphere::distGeo(c(fireCenter[1],fireCenter[2]), geoStns)/1000
ranking<-rank(distances, ties.method = "first")
# get closet RAWS dataframe
tempDF<-burnList[[which.min(ranking)]]
burnDays<-tempDF[which(tempDF$date>=discDate & tempDF$date<=contDate),]
# merge in FRP data to burn days DF
burnDays<-merge(burnDays, dailyFRP, by.x="date", by.y="DateAZ")

ggplot(burnDays, aes(bhrs20,sumFRP))+
  geom_point()

ggplot(burnDays)+
  geom_line(aes(date,bhrs20))+
  geom_line(aes(date,sumFRP), color="red")

ggplot(burnDays, aes(x=date)) +
  geom_line( aes(y=bhrs20), size=1, color="red") + 
  geom_line( aes(y=sumFRP/4000), size=1, color="black") +
  scale_y_continuous(
    # Features of the first axis
    name = "Burn Hours <20%",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*4000, name="sum FRP")
  )

# get gridmet data for fire events from thredds
library(RCurl)
library(ncdf4)
library(raster)

# find 2022 end date
begDate<-as.Date("1979-01-01")
endDate<-as.Date("2022-12-31")
endDay<-as.numeric((endDate-begDate))
dates<-seq.Date(begDate,endDate,by="day")

# link with 1 day and full grid of values
#URL<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc?lat[0:1:584],lon[0:1:1385],precipitation_amount[0:1:0][0:1:584][0:1:1385],day[0:1:15084]"# open into env
URL<-paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_rmin_1979_CurrentYear_CONUS.nc?lat[0:1:584],lon[0:1:1385],day[0:1:",endDay,"],daily_minimum_relative_humidity[0:1:0][0:1:0][0:1:0]")
#URL<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_rmin_1979_CurrentYear_CONUS.nc?lat[0:1:584],lon[0:1:1385],daily_minimum_relative_humidity[0:1:0][0:1:0][0:1:0]"
#http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_rmin_1979_CurrentYear_CONUS.nc?lat[0:1:584],lon[0:1:1385],day[0:1:16213],daily_minimum_relative_humidity[0:1:0][0:1:0][0:1:0]

# get ncdf file info
nc<-nc_open(URL)
# get lon and lat values
lons<-nc$dim$lon$vals
lats<-nc$dim$lat$vals

# get lat/lon indices for perimeter of fire
LonIdxMin <- min(which( nc$dim$lon$vals >= extent(perim)@xmin))-1 # subtract 1 to make base 0
LonIdxMax <- min(which( nc$dim$lon$vals >= extent(perim)@xmax))-1 # subtract 1 to make base 0
LatIdxMin <- min(which( nc$dim$lat$vals <= extent(perim)@ymax))-1   # subtract 1 to make base 0
LatIdxMax <- min(which( nc$dim$lat$vals <= extent(perim)@ymin))-1   # subtract 1 to make base 0

# put back in to request string
URL<-paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_rmin_1979_CurrentYear_CONUS.nc?lat[",LatIdxMin,":1:",LatIdxMax,"],lon[",LonIdxMin,":1:",LonIdxMax,"],day[0:1:",endDay,"],daily_minimum_relative_humidity[0:1:",endDay,"][",LatIdxMin,":1:",LatIdxMax,"][",LonIdxMin,":1:",LonIdxMax,"]")
#URL<-paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_rmin_1979_CurrentYear_CONUS.nc?lat[0:1:584],lon[0:1:1385],daily_minimum_relative_humidity[0:1:0][0:1:0][0:1:0]")

nc<-nc_open(URL)
# get lon and lat values
#lons<-nc$dim$lon$vals
#lats<-nc$dim$lat$vals

# see more at https://rstudio-pubs-static.s3.amazonaws.com/350043_e077809976ee44e19f53f146ad223a60.html
#names(dataset$var)
#names(dataset$dim)
lon <- ncvar_get(dataset, "lon")
lat <- ncvar_get(dataset, "lat")

# process array into lists and rasters...from stateMaps_ACIS.R
var<-ncvar_get(nc, "daily_minimum_relative_humidity")
matrixList <- vector("list",dim(var)[3])
for(i in 1:dim(var)[3]){
  matrixList[[i]]<-t(var[,,i])
  print(i)
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridStack<-setExtent(gridStack, extent(list(x=lon,y=lat)), keepres=FALSE, snap=FALSE)
names(gridStack)<-dates
# plot up test
#plot(gridStack[[892]])
#plot(perim, add=TRUE)
