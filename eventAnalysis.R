# analyze fire progression with RAWS/burn period data
# MAC 06/20/23

library(raster)
library(geosphere)

# load fire progression data from fireProgression_loop.R
load("~/RProjects/BurnPeriodResearch/data/fireProgression_Stats2.RData")
# load burnperiod climo dataset from burnPeriodClimo.R
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# get station list from burnList
stations<-do.call(rbind, lapply(burnList,function(x) x[1,c("STA_NAME","LATITUDE","LONGITUDE")]))

# find closest RAWS to fire and attached burn period hours
# temporary RAWS station list for distances
geoStns<-stations[,c("LONGITUDE","LATITUDE")]

# loop through events and attach RAWS data
fireEventsRAWS<-list()
for(i in 1:length(fireProgList)){
  
  # find closest RAWS 
  fireCenter<-rgeos::gCentroid(fireProgList[[i]][[3]])
  fireCenter<-c(fireCenter@coords[1],fireCenter@coords[2])
  distances<-geosphere::distGeo(fireCenter, geoStns)/1000
  ranking<-rank(distances, ties.method = "first")
  
  # get data from closest RAWS dataframe
  tempDF<-burnList[[which.min(ranking)]]
  tempEvent<-fireProgList[[i]][[1]]
  tempDF<-merge(tempEvent,tempDF, by.x="DateAZ",by.y="date")
  tempDF$RAWSdist<-distances[which.min(ranking)]
    tempDF$cum20Bhrs<-cumsum(tempDF$bhrs20)
  # put data into list
  fireEventsRAWS[[i]]<-tempDF
}

fireEventsRAWS<-do.call(rbind, fireEventsRAWS)

library(ggplot2)

ggplot(fireEventsRAWS, aes(acres, bhrs20_avg_anom))+
  geom_point()+
  geom_hline(yintercept = 0)+
  facet_wrap(~FireName, scales="free_x")+
  ggtitle("Burn Period Anomaly vs Daily Acres Burned")

ggplot(fireEventsRAWS, aes(bhrs20_avg_anom))+
  geom_histogram()+
  geom_vline(xintercept = 0, color="red")+
  facet_wrap(~FireName, scales="free")
  #facet_wrap(~FireName, scales="free_y")

ggplot(fireEventsRAWS, aes(cumSum, cum20Bhrs))+
  geom_point()+
  facet_wrap(~FireName, scales="free")+
  ggtitle("Cumulative Acres Burned vs Cumulative Burn Hours")

ggplot(fireEventsRAWS, aes(DateAZ, bhrs20))+
  geom_line(aes(DateAZ, bhrs20), color="red")+
  geom_line(aes(DateAZ, acres/1000), color="black")+
  geom_hline(yintercept = 24, color="red")+
  facet_wrap(~FireName, scales = "free")+
  ggtitle("Burn Hours and Daily Acres Burned")

ggplot(fireEventsRAWS, aes(bhrs20, maxVPD))+
  geom_point()+
  facet_wrap(~FireName, scales="free")+
  ggtitle("Burn Hours vs max VPD")+
  geom_smooth(method=lm, se=FALSE)

ggplot(fireEventsRAWS, aes(maxFFWI, acres))+
  geom_point()+
  facet_wrap(~FireName, scales="free")+
  ggtitle("max VPD vs Daily Acres Burned")+
  geom_smooth(method=lm, se=FALSE)

ggplot(fireEventsRAWS, aes(bhrs20, sumFRP/countFRP))+
  geom_point()+
  facet_wrap(~FireName, scales="free_y")+
  ggtitle("Burn Hours vs Max FRP")+
  geom_smooth(method=lm, se=FALSE)

# test out fire event stats
library(dplyr)
eventsSumm<- fireEventsRAWS %>% group_by(FireName) %>%
                          summarize(avgBhrs=sum(bhrs20,na.rm = TRUE)/n())


# map fire progression with burn hours
for(i in 1:length(fireProgList)){
 print(fireProgList[[i]][[1]]$FireName[1])
}

i=12
fireProg<-fireProgList[[i]][[4]]
plot(fireProg)
rasterVis::levelplot(fireProg, margin=FALSE, main=fireProgList[[i]][[1]]$FireName[1], par.settings = rasterVis::PuOrTheme)

tempDF<-subset(fireEventsRAWS, FireName==fireProgList[[i]][[1]]$FireName[1])
tempDF<-tempDF[,c("doy","bhrs20")]
#tempDF<-tempDF[,c("doy","maxHDW")]
fireProg[fireProg>tempDF$doy[nrow(tempDF)]]<-NA

rc <- reclassify(fireProg, as.matrix(tempDF))
 rasterVis::levelplot(rc, margin=FALSE, main=fireProgList[[i]][[1]]$FireName[1])
 
# plot fire event info
 
 fireEvent<-subset(fireEventsRAWS, FireName=="TELEGRAPH")
 
 fireEvent<-fireEvent[c("DateAZ","maxFRP","acres","bhrs20","bhrs10","minRH",
                        "maxVPD","minDP","maxWS","maxT","maxFFWI","maxHDW",
                        "bhrs20_avg_anom")]
 fireEventLong<- tidyr::gather(fireEvent,variable,value,2:13)
 
 ggplot(fireEventLong, aes(DateAZ, value))+
   geom_line()+
   facet_wrap(~variable, scales = "free", ncol=2)+
   ggtitle("Telegraph Fire Daily Variables")
 