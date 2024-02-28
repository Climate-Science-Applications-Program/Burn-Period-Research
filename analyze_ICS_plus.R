# import and analyze SIT 209 plus datasets
# MAC 09/27/23

# import incident dataset
incidents <- readr::read_csv("data/ICS209plus/ics209-plus-wf_incidents_1999to2020.csv")
# parse exp containment date
incidents$EXPECTED_CONTAINMENT_DATE<-parsedate::parse_date(incidents$EXPECTED_CONTAINMENT_DATE)

# create sp dataframe
incidents<-subset(incidents, !is.na(POO_LONGITUDE))
sp::coordinates(incidents)<- ~POO_LONGITUDE+POO_LATITUDE
# crop to AZ/NM
incidents<-raster::crop(incidents,raster::extent(c(-115,-102.8,31,37.1)))
# add lat/lon back into df
incidents<-cbind.data.frame(incidents@data,incidents@coords)
# filter to post 2000
incidents<-subset(incidents, DISCOVERY_DATE>as.POSIXct("1999-12-31"))

# subset to fire size
incidents<-subset(incidents, FINAL_ACRES>=50000)

# import and subset sitreps
sitreps <- readr::read_csv("data/ICS209plus/ics209-plus-wf_sitreps_1999to2020.csv")
sitreps <-subset(sitreps, INCIDENT_ID %in% incidents$INCIDENT_ID)


#####
# combine ICS incidents and burn period metrics

# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")
# get station list from burnList
stations<-do.call(rbind, lapply(burnList,function(x) x[1,c("STA_NAME","LATITUDE","LONGITUDE")]))

# find closest RAWS to fire and attached burn period hours
library(geosphere)

# new list for FOD burn period
incBurnPeriod<-list()
sitrepBurnPeriod<-list()
# temporary RAWS station list for distances
geoStns<-stations[,c("LONGITUDE","LATITUDE")]

# mean/median burn periods from discovery to containment date
# loop through fires to attached burn period to discovery data
for(i in 1:nrow(incidents)){
  fire<-c(incidents$POO_LONGITUDE[i],incidents$POO_LATITUDE[i])
  distances<-geosphere::distGeo(fire, geoStns)/1000
  ranking<-rank(distances, ties.method = "first")
  # get closet RAWS dataframe
  tempDF<-burnList[[which.min(ranking)]]
  # join burn period to incident max days
  bpDF<-cbind.data.frame(tempDF[which(tempDF$date== as.Date(incidents$DISCOVERY_DATE[i])),c("bhrs20","bhrs20_med_anom")],
                        #tempDF[which(tempDF$date== as.Date(incidents$WF_PEAK_AERIAL_DATE[i])),c("bhrs20","bhrs20_med_anom")]
                        #tempDF[which(tempDF$date== as.Date(incidents$WF_PEAK_PERSONNEL_DATE[i])),c("bhrs20","bhrs20_med_anom")]
                        tempDF[which(tempDF$date== as.Date(incidents$WF_MAX_GROWTH_DATE[i])),c("bhrs20","bhrs20_med_anom")])
  colnames(bpDF)<-c("disc_bhrs20","disc_bhrs20_med_anom","maxGrowth_bhrs20","maxGrowth_bhrs20_med_anom")
  
  tempDFbp<-cbind.data.frame(incidents[i,], bpDF,tempDF$STA_NAME[2],distances[which.min(ranking)])
  colnames(tempDFbp)[(ncol(tempDFbp)-1):ncol(tempDFbp)]<-c("STATION_NAME","dist_to_RAWS_km")
  
  # join bp to sitreps
  tempSIT<-subset(sitreps, sitreps$INCIDENT_ID==incidents$INCIDENT_ID[i])
  tempSIT$joinDate<-as.Date(tempSIT$REPORT_TO_DATE)
  tempSIT$report_n<-nrow(tempSIT)
  tempSIT<-merge(tempSIT, tempDF[,c("date","bhrs20","bhrs20_med_anom")], by.x ="joinDate", by.y="date")
  
  # save to lists
  incBurnPeriod[[i]]<-tempDFbp
  sitrepBurnPeriod[[i]]<-tempSIT
  
  print(i)
}

incBurnPeriod<-do.call(rbind,incBurnPeriod)
sitrepBurnPeriod<-do.call(rbind, sitrepBurnPeriod)

#####

# look at variability in burn period through events, does it vary a great deal in large fires?
# does it correspond to control measures and increase in containment?

library(ggplot2)
library(magrittr)


ggplot(subset(sitrepBurnPeriod, report_n>5))+
  geom_line(aes(joinDate,bhrs20), color="orange")+
  #geom_line(aes(joinDate,bhrs20_med_anom), color="red")+
  geom_line(aes(joinDate, PCT_CONTAINED_COMPLETED))+
  #geom_line( aes(x = joinDate, y = c(NA, PCT_CONTAINED_COMPLETED %>% diff())))+
  facet_wrap(.~INCIDENT_NAME, scales = "free")
  
  
