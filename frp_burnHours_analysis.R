# compare FRP values with nearby RAWS burn hours by event
# adapted from fireProgression_loop.R
# MAC 07/17/23

library(raster)
library(dplyr)
#library(gstat)

# turn off dplyr warnings
options(dplyr.summarise.inform = FALSE)

##### load burnperiod climo dataset from burnPeriodClimo.R ----
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# get station list from burnList
stations<-do.call(rbind, lapply(burnList,function(x) x[1,c("STA_NAME","LATITUDE","LONGITUDE")]))

# find closest RAWS to fire and attached burn period hours
# temporary RAWS station list for distances
geoStns<-stations[,c("LONGITUDE","LATITUDE")]
#####

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
load("~/RProjects/BurnPeriodResearch/data/SV-C2_FRP_MODIS_SW.RData")
#####

# combine VIIRS and MODIS df
modiscrop<-rbind(modiscrop,snppcrop)
rm(snppcrop)

# adjust to local time
modiscrop$DateTimeUTC<-as.POSIXct(paste0(modiscrop$ACQ_DATE," ",modiscrop$ACQ_TIME), format="%Y-%m-%d %H%M")
modiscrop$DateTimeAZ7=as.POSIXct(format(modiscrop$DateTimeUTC,tz="America/Phoenix"))
modiscrop$DateAZ<-as.Date(format(modiscrop$DateTimeAZ7, "%Y-%m-%d"))
modiscrop$DateAZ_doy<-as.numeric(format(modiscrop$DateAZ,"%j"))

# load/filter mtbs perims ----
load("~/RProjects/BurnPeriodResearch/data/AZNM_mtbs_perims_1984_2022_wProv.RData")
  # remove factors
  perimsCrop$BurnBndAc<-as.numeric(as.character(perimsCrop$BurnBndAc))
  perimsCrop$Incid_Name<-as.character(perimsCrop$Incid_Name)
  perimsCrop$Ig_Date<-as.Date(perimsCrop$Ig_Date, "%Y-%m-%d")
  perimsCrop$year<-as.numeric(format(as.Date(perimsCrop$Ig_Date, format="%Y/%m/%d"),"%Y"))
  #min(fc$DISCOVERY_DATE); max(fc$CONT_DATE)
  #min(modiscrop$ACQ_DATE); max(modiscrop$ACQ_DATE)
  perimsCrop<-subset(perimsCrop, Ig_Date>=min(modiscrop$ACQ_DATE))
  perimsCrop<-subset(perimsCrop, BurnBndAc>=1000)
  perimsCrop<-subset(perimsCrop, Incid_Type=="Wildfire")
#####

##### loop through perims and extract FRP/get RAWS data
varsList<-list()
for(i in 1:nrow(perimsCrop@data)){  
  perim<-perimsCrop[i,]
    
  fc_idx<-min(which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name & 
          fc$FIRE_YEAR==perim@data$year))
  
  # get discovery and containment dates
  discDate<-min(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name & 
                           fc$FIRE_YEAR==perim@data$year),"DISCOVERY_DATE"])
  contDate<-max(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name &
                           fc$FIRE_YEAR==perim@data$year),"CONT_DATE"])
  # deal with event date issues
      # if fc data is missing
      if(is.na(as.character(discDate))){
        discDate<-as.Date(perim$Ig_Date[1],"%Y/%m/%d")
      }else{
      }
      
      # give 30 day window if 
      if(is.na(as.character(contDate))){
        contDate<-discDate+30
        contDateUnknown<-"yes"
      }else{
        contDateUnknown<-"no"
      }
  ##
 
  # get fire size 
  fcFireSize<-sum(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name &
                             fc$FIRE_YEAR==perim@data$year),"FIRE_SIZE"])
  if(fcFireSize==0){
    fcFireSize<-as.numeric(as.character(perim$BurnBndAc[1]))
  }else{}
  
  mtbsFireSize<-perim$BurnBndAc
  # subset modis FRP in event dates
  #modisFRP<-subset(modiscrop,ACQ_DATE>=discDate & ACQ_DATE<=contDate )
  modisFRP<-subset(modiscrop,DateAZ>=discDate & DateAZ<=contDate )
  colnames(modisFRP)[1:2]<-c("Latitude","Longitude")
   
  if(nrow(modisFRP)!=0){
    
    # look for identical points
    modisFRP$latlon<-paste0(modisFRP$Latitude,"_",modisFRP$Longitude)
    nonDup<-modisFRP[!duplicated(modisFRP$latlon),]
    setdiff(modisFRP$latlon,nonDup$latlon)
    # trim duplicates
    which(!duplicated(modisFRP$latlon)==FALSE)
    modisFRP<-modisFRP[!duplicated(modisFRP$latlon),]
    
    # look at UTC vs local date
    #modisFRP$dateDiff<- as.numeric(modisFRP$ACQ_DATE-modisFRP$DateAZ)
    
    # convert frp to spatial points
    coordinates(modisFRP)<- ~ Longitude + Latitude
    proj4string(modisFRP) <- proj4string(perim)
    
    # subset points to perim
    temp<-sp::over(modisFRP, perim) # or change to perim for no buffer
    modisFRP<-modisFRP[-(which(is.na(temp$Event_ID)==TRUE)),] # change temp$Event_ID for perim 
    
    if(nrow(modisFRP@data)!=0){
      
      # plot(perim)
      # plot(modisFRP, add=TRUE)
      
      tempFRP<-modisFRP@data %>% 
        #group_by(DateAZ) %>%
        summarize(sumFRP=sum(FRP),
                  maxFRP=max(FRP),
                  avgFRP=mean(FRP),
                  countFRP=n(),
                  nightSumFRP=sum(FRP[DAYNIGHT=="N"]),
                  nightMaxFRP=max(FRP[DAYNIGHT=="N"]),
                  nightMeanFRP=mean(FRP[DAYNIGHT=="N"]),
                  nightCountFRP=sum(DAYNIGHT=="N"),
                  minDate=min(ACQ_DATE),
                  maxDate=max(ACQ_DATE))
      #tempFRP$nDays<-as.numeric(tempFRP$maxDate-tempFRP$minDate)+1
      
    }else{
      
      tempFRP<-cbind.data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      colnames(tempFRP)<-c("sumFRP","maxFRP","avgFRP","countFRP","nightSumFRP","nightMaxFRP","nightMeanFRP","nightCountFRP","minDate","maxDate")
      
    }
    
  }else{
    
    tempFRP<-cbind.data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    colnames(tempFRP)<-c("sumFRP","maxFRP","avgFRP","countFRP","nightSumFRP","nightMaxFRP","nightMeanFRP","nightCountFRP","minDate","maxDate")
    
  } 
   
  
    
  # get Burn Period stats
  # find closest RAWS 
  fireCenter<-rgeos::gCentroid(perim)
  fireCenter<-c(fireCenter@coords[1],fireCenter@coords[2])
  distances<-geosphere::distGeo(fireCenter, geoStns)/1000
  ranking<-rank(distances, ties.method = "first")
  
  # get data from closest RAWS dataframe
  tempDF<-burnList[[which.min(ranking)]]

  burnDay<-tempDF[which(tempDF$date==discDate),]
  # get max burn hours in fire event
  temp<-tempDF[which(tempDF$date>=discDate & tempDF$date<=contDate),]
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
  tempDF<-cbind.data.frame(fc[fc_idx,c("MTBS_FIRE_NAME","FIRE_YEAR","DISCOVERY_DATE","CONT_DATE","FIRE_SIZE")],burnDay,
                           meanBH,maxBH,minRH,distances[which.min(ranking)])
  colnames(tempDF)[ncol(tempDF)]<-"dist_to_RAWS_km"
  
  tempDF$mtbs_fire_size<-mtbsFireSize
  tempDF$cont_date_unknown<-contDateUnknown
  tempDF$nDays<-as.numeric(contDate-discDate)+1
  
  tempDF<-cbind.data.frame(tempDF,tempFRP)
  
  varsList[[i]]<-tempDF
  
  print(i)
}
  
climVarsDF<-do.call(rbind, varsList)  
  
##### analyze results -----
library(ggplot2)

table(climVarsDF$cont_date_unknown)
climVarsDF<-subset(climVarsDF, cont_date_unknown=="no")

summary(climVarsDF)

climVarsDF$bhr24<-ifelse(climVarsDF$bhrs20max==24,"24","<24")

ggplot(climVarsDF, aes(FIRE_SIZE))+
  facet_wrap(.~as.factor(bhr24), ncol=1)+
  geom_histogram()

ggplot(climVarsDF, aes(log(mtbs_fire_size),bhrs20max, color=as.factor(month)))+
  geom_point()+
  geom_hline(yintercept = 12)
table(climVarsDF$bhrs20max)

ggplot(climVarsDF, aes(log(mtbs_fire_size),bhrs20_med_anommax))+
  geom_point()+
  geom_hline(yintercept = 0)

ggplot(climVarsDF, aes(log(mtbs_fire_size),bhrs20max))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

quantile(climVarsDF$bhrs20max, c(0.1,0.5,0.9))
quantile(climVarsDF$bhrs10max, c(0.1,0.5,0.9))
median(climVarsDF$bhrs20max)
quantile(climVarsDF$bhrs20mean, c(0.1,0.5,0.9))

length(which(climVarsDF$bhrs20max>12))/nrow(climVarsDF)*100
length(which(climVarsDF$bhrs20max>18))/nrow(climVarsDF)*100

ggplot(climVarsDF, aes(log(FIRE_SIZE),bhrs20_med_anommax))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(climVarsDF, aes(log(nightMaxFRP),bhrs20_med_anommax))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(climVarsDF, aes(log(nightMaxFRP),bhrs20max))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(climVarsDF, aes(log(nightCountFRP),bhrs20max))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(climVarsDF, aes((nightCountFRP/FIRE_SIZE),bhrs20max))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(climVarsDF, aes(log(nightSumFRP/nDays),bhrs20_med_anommean, color=nDays))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_hline(yintercept = 0)

#cor(log(climVarsDF$nightSumFRP/climVarsDF$nDays),climVarsDF$bhrs20_med_anommean, use = "pairwise.complete")

ggplot(climVarsDF, aes(log(nightMeanFRP),bhrs20_med_anommean))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_hline(yintercept = 0)
  #facet_wrap(.~month)

ggplot(climVarsDF, aes(log(nightMeanFRP),bhrs20mean))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month)
  

ggplot(climVarsDF, aes(log(nightMaxFRP),bhrs20max))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_hline(yintercept = 0)+
  facet_wrap(.~month)


ggplot(climVarsDF, aes(log(avgFRP),bhrs20_med_anommean))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_hline(yintercept = 0)

  