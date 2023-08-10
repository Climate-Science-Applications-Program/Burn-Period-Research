# analyze fire progression with RAWS/burn period data
# using mtbs perims, adapted from eventAnalysis.R
# MAC 07/20/23

library(raster)
library(geosphere)

# load fire progression data from fireProgression_loop_mtbs.R
#load("./data/fireProgression_Stats_mtbs_perims_gt50K.RData")
load("./data/fireProgression_Stats_mtbs_perims_gt10K.RData")

# scan for empty fires, thin out list
temp<-c()
for(i in 1:length(fireProgList)){
  temp[i]<-fireProgList[[i]][[1]]$DateAZ[1]
}
fireProgList<-fireProgList[which(!is.na(temp))] 

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
  
  if(nrow(tempDF)==0){
    tempDF[1,]<-rep(NA,ncol(tempDF))
  }else{
  }
  tempDF$RAWSdist<-distances[which.min(ranking)]  
  tempDF$cum20Bhrs<-cumsum(tempDF$bhrs20)
  # put data into list
  fireEventsRAWS[[i]]<-tempDF
}



fireEventsRAWS<-do.call(rbind, fireEventsRAWS)
fireEventsRAWS<-subset(fireEventsRAWS, !is.na(DateAZ))
# drop fires with only one observation
# filter extreme daily spread rates
fireCounts<-as.data.frame(table(fireEventsRAWS$FireName))
length(unique(fireCounts$Var1))

# fire event group stats
library(dplyr)
eventStats<- fireEventsRAWS %>% group_by(FireName) %>%
                                  summarize(days=n(),
                                            maxAc=max(cumSum),
                                            maxDayAc=max(acres),
                                            month=first(month),
                                            year=first(year),
                                            meanBhrs20=mean(bhrs20),
                                            meanMaxVPD=mean(maxVPD),
                                            meanBhrs20_med_anom=mean(bhrs20_med_anom),
                                            meanBhrs20_avg_anom=mean(bhrs20_avg_anom),
                                            cumBhrs20=sum(bhrs20),
                                            cumBhrs20_med_anom=sum(bhrs20_med_anom),
                                            cumAcres=sum(acres),
                                            cumMaxVPD=sum(maxVPD),
                                            RAWSdist=first(RAWSdist))
  
  
library(ggplot2)

# ggplot(eventStats, aes(cumMaxVPD,log(cumAcres)))+
#   geom_point()+
#   #facet_wrap(~FireName, scales="free")+
#   ggtitle("bhrs20 anom vs total Ac")+
#   geom_smooth(method=lm, se=FALSE)


ggplot(fireEventsRAWS, aes(acres, bhrs20_avg_anom))+
  geom_point()+
  geom_hline(yintercept = 0)+
  #facet_wrap(~FireName, scales="free_x")+
  ggtitle("Burn Period Anomaly vs Daily Acres Burned")

ggplot(fireEventsRAWS, aes(bhrs20,acres))+
  geom_point()+
  geom_hline(yintercept = 0)+
  #facet_wrap(~FireName, scales="free_x")+
  ggtitle("Burn Period vs Daily Acres Burned")


ggplot(fireEventsRAWS, aes(bhrs20))+
  geom_histogram()+
  geom_vline(xintercept = 0, color="red")+
  facet_wrap(~FireName, scales="free")
  #facet_wrap(~FireName, scales="free_y")

ggplot(fireEventsRAWS, aes(cumSum, cum20Bhrs))+
  geom_point()+
  #facet_wrap(~FireName, scales="free")+
  ggtitle("Cumulative Acres Burned vs Cumulative Burn Hours")

ggplot(fireEventsRAWS, aes(DateAZ, bhrs20))+
  geom_line(aes(DateAZ, bhrs20), color="red")+
  geom_line(aes(DateAZ, acres/1000), color="black")+
  geom_hline(yintercept = 24, color="red")+
  facet_wrap(~FireName, scales = "free")+
  ggtitle("Burn Hours and Daily Acres Burned")

ggplot(fireEventsRAWS, aes(bhrs20_med_anom,log(acres)))+
  geom_point()+
  #facet_wrap(~FireName, scales="free")+
  ggtitle("daily acres vs max VPD")+
  geom_smooth(method=lm, se=FALSE)
cor(fireEventsRAWS$maxVPD,log(fireEventsRAWS$acres), use="pairwise.complete.obs")


ggplot(fireEventsRAWS, aes(maxFFWI, acres))+
  geom_point()+
  #facet_wrap(~FireName, scales="free")+
  ggtitle("max FFWI vs Daily Acres Burned")+
  geom_smooth(method=lm, se=FALSE)

ggplot(fireEventsRAWS, aes(bhrs20_med_anom, sumFRP/countFRP))+
  geom_point()+
  #facet_wrap(~FireName, scales="free_y")+
  ggtitle("Burn Hours anom vs Avg FRP/day")+
  geom_smooth(method=lm, se=FALSE)

ggplot(fireEventsRAWS, aes(bhrs20_med_anom, log(sumFRP/countFRP)))+
  geom_point()+
  #facet_wrap(~FireName, scales="free_y")+
  ggtitle("Burn Hours 20 Anom vs Avg FRP")+
  geom_smooth(method=lm, se=FALSE)
cor(fireEventsRAWS$bhrs20_med_anom,log(fireEventsRAWS$sumFRP/fireEventsRAWS$countFRP), use="pairwise.complete.obs", method="spearman")
cor.test(fireEventsRAWS$bhrs20,log(fireEventsRAWS$nightMaxFRP), use="pairwise.complete.obs", method="spearman")$p.value

ggplot(fireEventsRAWS, aes(bhrs20, log(maxFRP)))+
  #geom_point()+
  #geom_point(alpha = .2)+
  geom_point(position = position_jitter(width = .20, height = 0))+
  #facet_wrap(~FireName, scales="free_y")+
  ggtitle("Burn Hours 20 vs Max FRP")+
  geom_smooth(method=lm, se=FALSE)
cor(fireEventsRAWS$bhrs20_med_anom,log(fireEventsRAWS$maxFRP), use="pairwise.complete.obs", method="spearman")
cor.test(fireEventsRAWS$bhrs20,log(fireEventsRAWS$maxFRP), use="pairwise.complete.obs", method="spearman")$p.value




ggplot(fireEventsRAWS, aes(bhrs20, log(nightSumFRP/nightCountFRP)))+
  geom_point()+
  #facet_wrap(~FireName, scales="free_y")+
  ggtitle("Burn Hours vs Avg Night FRP")+
  geom_smooth(method=lm, se=FALSE)
cor(fireEventsRAWS$bhrs20,log(fireEventsRAWS$nightSumFRP/fireEventsRAWS$nightCountFRP), use="pairwise.complete.obs")


ggplot(fireEventsRAWS, aes(bhrs20_med_anom, log(sumFRP/countFRP)))+
  geom_point()+
  #facet_wrap(~FireName, scales="free_y")+
  ggtitle("Burn Hours Anom vs avgFRP")+
  geom_smooth(method=lm, se=FALSE)
cor(fireEventsRAWS$bhrs20_med_anom,log(fireEventsRAWS$sumFRP/fireEventsRAWS$countFRP), use="pairwise.complete.obs")
#cor(fireEventsRAWS$bhrs20_med_anom,log(fireEventsRAWS$nightSumFRP/fireEventsRAWS$nightCountFRP), use="pairwise.complete.obs")

ggplot(fireEventsRAWS, aes(maxVPD, log(nightSumFRP/nightCountFRP)))+
  geom_point()+
  #facet_wrap(~FireName, scales="free_y")+
  ggtitle("Max VPD vs AvgNightFRP")+
  geom_smooth(method=lm, se=FALSE)
cor(fireEventsRAWS$maxVPD,log(fireEventsRAWS$nightSumFRP/fireEventsRAWS$nightCountFRP), use="pairwise.complete.obs")

# fire event 
cumEvent<-fireEventsRAWS %>% group_by(FireName) %>%
                            summarise(nDay=n(),
                                      bhrs20=cumsum(bhrs20),
                                      bhrs20_med_anom=cumsum(bhrs20_med_anom),
                                      acres=cumsum(acres),
                                      maxVPD=cumsum(maxVPD))


#corr matrix
library("PerformanceAnalytics")
#my_data <- fireEventsRAWS[, c(2:9,16:19,21:35,43:44)]
#my_data <- fireEventsRAWS[, c(2:9,16:19,43:44)]
my_data <- fireEventsRAWS[, c(2:9,17,43:44)]
chart.Correlation(my_data, histogram=TRUE, pch=19,method = c("spearman"))


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
 
##### plot top 10 fires for AZ/NM ----

topten<-c("WALLOW","RODEO","CAVE CREEK COMPLEX","HORSESHOE 2","BUSH","TELEGRAPH","WOODBURY",
   "BIGHORN","WILLOW","ASPEN","HERMITS PEAK","BLACK","WHITEWATER-BALDY","LAS CONCHAS",
   "SILVER","DONALDSON","DRY LAKE COMPLEX (DRY LAKES)","PASCO","MCDONALD","PONIL COMPLEX")
 
 