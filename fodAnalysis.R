# load and analysis FOD with burn hours
# MAC 04/02/23

# to do
# - get mean/median stats between discovery and containment dates
# - develop daily anomalies

library(sf)
library(rgdal)
library(raster)

# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# get station list from burnList
stations<-do.call(rbind, lapply(burnList,function(x) x[1,c("STA_NAME","LATITUDE","LONGITUDE")]))
# e<-extent(c(min(stations$LONGITUDE),max(stations$LONGITUDE),
#           min(stations$LATITUDE),max(stations$LATITUDE)))
# e<-extent(c(-115,-102.8,31,37.1))

# ##### read in FOD from GDB ----
# # # read feature class from FOD gdb using sf
# # fc <- sf::st_read("./data/fod/FPA_FOD_20221014.gdb")
# # fc <- sf::st_read(dsn = "./data/fod/FPA_FOD_20221014.gdb",
# #                       layer = "Fires")
# 
# # use rgdal to get FOD from gdb
# # The input file geodatabase
# fgdb <- "./data/fod/FPA_FOD_20221014.gdb"
# 
# # List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
#   fc_list <- ogrListLayers(fgdb)
#   print(fc_list)
# 
# # Read the feature class
# fc <- readOGR(dsn=fgdb,layer="Fires")
# 
# # # Determine the FC extent, projection, and attribute information
# # summary(fc)
# # # View the feature class
# # plot(fc)
# 
# # crop data down to SW and save as RData
# fc<-raster::crop(fc,e)
# save(fc,file = "./data/swFOD.RData")
# #####

# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")

# work with dataframe only
fc<-fc@data

# remove factors
fc[,] <- lapply(fc, function(x) type.convert(as.character(x), as.is = TRUE))

# adjust dates on fc DF  
fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,"%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,"%m/%d/%Y")
# subset to post 2000 fires
fc<-subset(fc, DISCOVERY_DATE>="2000-01-01")
# subset to large fires, https://www.nwcg.gov/data-standards/approved/fire-size-class
table(fc$FIRE_SIZE_CLASS)
#fc<-subset(fc, FIRE_SIZE_CLASS=="G")
fc<-subset(fc, FIRE_SIZE_CLASS %in% c("F","G"))

# find closest RAWS to fire and attached burn period hours
library(geosphere)

# new list for FOD burn period
fodBurnPeriod<-list()
# temporary RAWS station list for distances
geoStns<-stations[,c("LONGITUDE","LATITUDE")]

# mean/median burn periods from discovery to containment date
# loop through fires to attached burn period to discovery data
for(i in 1:nrow(fc)){
  fire<-c(fc$LONGITUDE[i],fc$LATITUDE[i])
  distances<-geosphere::distGeo(fire, geoStns)/1000
  ranking<-rank(distances, ties.method = "first")
  # get closet RAWS dataframe
  tempDF<-burnList[[which.min(ranking)]]
  burnDay<-tempDF[which(tempDF$date==fc$DISCOVERY_DATE[i]),]
  # get max burn hours in fire event
  temp<-tempDF[which(tempDF$date>=fc$DISCOVERY_DATE[i] & tempDF$date<=(fc$DISCOVERY_DATE[i]+5)),]
  #temp<-tempDF[which(tempDF$date>=fc$DISCOVERY_DATE[i] & tempDF$date<=fc$CONT_DATE[i]),]
    maxBH<-sapply(temp[,c("bhrs25","bhrs20","bhrs15","bhrs10","bhrs5","bhrs20_med_anom","bhrs20_avg_anom","maxVPD","minVPD","maxWS","maxT","maxFFWI","maxHDW")],max)
      maxBH[maxBH<0]<-NA
      maxBH<-as.data.frame(t(maxBH));
      colnames(maxBH)<-paste0(colnames(maxBH),"max")
    meanBH<-sapply(temp[,c("bhrs25","bhrs20","bhrs15","bhrs10","bhrs5","bhrs20_med_anom","bhrs20_avg_anom","maxVPD","minVPD","maxWS","maxT","maxFFWI","maxHDW")],mean)
      meanBH<-as.data.frame(t(round(meanBH,1)));
      colnames(meanBH)<-paste0(colnames(meanBH),"mean")
    minRH<-min(temp$minRH)
      minRH<-as.data.frame(minRH)
      minRH[minRH<0]<-NA
      minRH[minRH>100]<-NA
      colnames(minRH)<-"minRH_event"
  # combine into df  fc[i,],
  tempDF<-cbind.data.frame(fc[i,c("MTBS_FIRE_NAME","FIRE_YEAR","DISCOVERY_DATE","CONT_DATE","FIRE_SIZE")],burnDay,
                           meanBH,maxBH,minRH,distances[which.min(ranking)])
  colnames(tempDF)[ncol(tempDF)]<-"dist_to_RAWS_km"
  
  fodBurnPeriod[[i]]<-tempDF
  print(i)
}

fodBurnPeriod<-do.call(rbind,fodBurnPeriod)

save(fodBurnPeriod, file="./data/FODBurnPeriod.RData")

##### Analysis ----
load("~/RProjects/BurnPeriodResearch/data/FODBurnPeriod.RData")

library(ggplot2)

fodBurnPeriod$log_Fire_Size<-log(fodBurnPeriod$FIRE_SIZE)
fodBurnPeriod$duration<-fodBurnPeriod$CONT_DATE-fodBurnPeriod$DISCOVERY_DATE
fodBurnPeriod$maxBP12hrs_20<-ifelse(fodBurnPeriod$bhrs20max>=12, 1,0)

ggplot(fodBurnPeriod, aes(log_Fire_Size,bhrs20max))+
  geom_point()+
  #geom_hline(yintercept=0)+
  geom_smooth(method='lm', formula= y~x)+
  #facet_wrap(.~month)+
  ggtitle("Log Fire Size (Class F and G) vs Max Burn Period (RH 20)")

#cor(fodBurnPeriod$log_Fire_Size,fodBurnPeriod$bhrs20_med_anommax, use="na.or.complete")
#cor(fodBurnPeriod$log_Fire_Size,fodBurnPeriod$minRH_event, use="na.or.complete")

library(dplyr)
fodBurnPeriod %>%
  group_by(month) %>%
  summarize(CORmed=cor(log_Fire_Size, bhrs20_med_anommax, use="na.or.complete"),
            CORavg=cor(log_Fire_Size, bhrs20_avg_anommax, use="na.or.complete"))


ggplot(fodBurnPeriod, aes(log_Fire_Size,bhrs5max))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)



