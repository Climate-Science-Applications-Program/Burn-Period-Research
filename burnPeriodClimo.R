# create Burn Period climatologies from DRI-CEFA gap-filled RAWS data
# MAC 02/19/23

library(dplyr)
library(XML)
library(RCurl)
library(plantecophys)
library(firebehavioR)

file_list <- list.files(path="/home/crimmins/RProjects/BurnPeriodTracker/data/raws", full.names = TRUE, recursive = TRUE)
file_names<- list.files(path="/home/crimmins/RProjects/BurnPeriodTracker/data/raws", full.names = FALSE, recursive = TRUE, include.dirs = FALSE)

#####
# calculate burning hours
burnList<-list()
  k=1
offStn<-list()
  j=1
rhDayList<-list()
  l=1
rhHrList<-list()  
  m=1
rhHrMoList<-list()  
  n=1
  
  
for(i in 1:length(file_list)){
  # read in RAWS file
  tempRAWS<-read.csv(file_list[i])
  tempRAWS$DateTime<-as.POSIXct(tempRAWS$DateTime, format = "%Y-%m-%dT%H:%M:%S")
  tempRAWS$year<-as.numeric(format(tempRAWS$DateTime,"%Y"))
  tempRAWS$month<-as.numeric(format(tempRAWS$DateTime,"%m"))
  tempRAWS$day<-as.numeric(format(tempRAWS$DateTime,"%d"))
  tempRAWS$hour<-as.numeric(format(tempRAWS$DateTime,"%H"))
  
  # calc fire weather indices
  fireWXidx = fireIndex(temp=((tempRAWS$Temperature.F.-32)*(5/9)), u= (tempRAWS$WindSpeed.mph.*1.60934), rh = tempRAWS$RelativeHumidity...)
  # combine with temp df
  tempRAWS$ffwi<-fireWXidx$fosberg
  tempRAWS$hdw<-fireWXidx$hotDryWindy
  
  # get station metadata and check for recent data
  #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-",format(Sys.time(), "%Y"),"&end=31-Dec-",format(Sys.time(), "%Y"),"&time=&sort=&ndays=")
  stnID<-substr(file_names[i], 4,9)
  url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",stnID,"&sig=&type=&start=&end=&time=&sort=asc&ndays=10&user=")
  # past year
  #url<-paste0("https://famprod.nwcg.gov/wims/xsql/obs.xsql?stn=",temp$StationNum[1],"&sig=&user=&type=&start=01-Jan-","2021","&end=31-Dec-","2021","&time=&sort=&ndays=")
    xData <- getURL(url)
    xmldoc <- xmlParse(xData)
    rawsMeta <- xmlToDataFrame(xData)
  
  # skip station, if empty response  
    if(nrow(rawsMeta)==0){
      offStn[[j]]<-stnID
      print(paste0(stnID,", no response"))
      j=j+1
    }else{
      
      # station info
      stnName<-as.character(rawsMeta$sta_nm[1])
      lat<-as.numeric(as.character(rawsMeta$latitude))
      lon<-as.numeric(as.character(rawsMeta$longitude))
        lat<-lat[1]
        lon<-lon[1]
      elev<-elevatr::get_elev_point(data.frame(x=lon,y=lat),prj = "EPSG:4326", src = "aws")
      # calc air pressure based on elevation
      kPa<-(101325*((1-(2.25577*10^-5)*elev@data$elevation)^5.25588))/1000
      # calc vpd
      tempRAWS$vpd_kPa<-RHtoVPD(tempRAWS$RelativeHumidity..., ((tempRAWS$Temperature.F.-32)*(5/9)), Pa = kPa)
      # calculate dewpoint
      tempRAWS$dp_F<-weathermetrics::humidity.to.dewpoint(tempRAWS$RelativeHumidity...,tempRAWS$Temperature.F.,
                                           temperature.metric = "fahrenheit") 
      
      # count of burn hours
      burnHRS<-  tempRAWS %>%
        group_by(year, month, day) %>%
        summarize(n_hours = n(),
                  bhrs25 = sum(`RelativeHumidity...` <= 25, na.rm = TRUE),
                  bhrs20 = sum(`RelativeHumidity...` <= 20, na.rm = TRUE),
                  bhrs15 = sum(`RelativeHumidity...` <= 15, na.rm = TRUE),
                  bhrs10 = sum(`RelativeHumidity...` <= 10, na.rm = TRUE),
                  bhrs5 = sum(`RelativeHumidity...` <= 5, na.rm = TRUE),
                  minRH = min(as.numeric(as.character(`RelativeHumidity...`)), na.rm = TRUE),
                  maxRH = max(as.numeric(as.character(`RelativeHumidity...`)), na.rm = TRUE),
                  meanRH = mean(as.numeric(as.character(`RelativeHumidity...`)), na.rm = TRUE),
                  minVPD= min(vpd_kPa, na.rm=TRUE),
                  maxVPD= max(vpd_kPa, na.rm=TRUE),
                  meanVPD= mean(vpd_kPa, na.rm=TRUE),
                  minDP = min(dp_F, na.rm = TRUE),
                  maxDP = max(dp_F, na.rm = TRUE),
                  meanDP = mean(dp_F, na.rm = TRUE),
                  maxWS= max(`WindSpeed.mph.`, na.rm=TRUE),
                  minT=min(`Temperature.F.`, na.rm = TRUE),
                  maxT= max(`Temperature.F.`, na.rm = TRUE),
                  meanT=mean(`Temperature.F.`, na.rm = TRUE),
                  maxFFWI= max(ffwi,na.rm = TRUE),
                  maxHDW= max(hdw, na.rm = TRUE))
      
      # add in date field
      burnHRS$date<-as.Date(paste0(as.numeric(burnHRS$month),"-",as.numeric(burnHRS$day),"-",as.numeric(burnHRS$year)), format="%m-%d-%Y")
      
      # thin out days with fewer than 21 hours ~ 10% missing and create complete dataframe
      #burnHRS<-tidyr::complete(burnHRS, date = seq.Date(min(date), max(date), by="day"), fill = list())
      burnHRS<-subset(burnHRS, n_hours>=21)
      # complete dates
      dates<-  as.data.frame(seq.Date(min(burnHRS$date), max(burnHRS$date), by="day"))
      colnames(dates)<-"date"
      # complete list
      burnHRS<-merge(burnHRS, dates, by="date", all.y=TRUE)
      
      # stnName<-rawsMeta$sta_nm[1]
      # lat<-rawsMeta$latitude[1]
      # lon<-rawsMeta$longitude[1]
      
      # add in station info
      burnHRS$LATITUDE<-lat
      burnHRS$LONGITUDE<-lon
      burnHRS$STA_NAME<-stnName
      #burnHRS$PSA_NAME<-sw_rawsDF$PSA_NAME[i]
      burnHRS$StationNum<-stnID
      #elev<-elevatr::get_elev_point(data.frame(x=lon,y=lat),prj = "EPSG:4326", src = "aws")
        burnHRS$elev<-round(elev@data$elevation*3.28084,0)
      
      # add in doy
      burnHRS$doy<-as.numeric(format(burnHRS$date, "%j"))
      burnHRS$day<-as.numeric(format(burnHRS$date, "%d"))
      burnHRS$year<-as.numeric(format(burnHRS$date, "%Y"))
      burnHRS$month<-as.numeric(format(burnHRS$date, "%m"))
      
      # limit to set period of record
      burnHRS<-subset(burnHRS, year<=2022)
      
      ##### calc anomalies ----
      ## create 5-day window to increase observations in quantile calc, from isd_lite.R
      dayGroup<-burnHRS[,c("doy","bhrs20")]
      temp2p<-dayGroup
        temp2p$doy<-temp2p$doy+2
      temp1p<-dayGroup
        temp1p$doy<-temp1p$doy+1
      temp0<-dayGroup
      temp1m<-dayGroup
        temp1m$doy<-(temp1m$doy)-1
      temp2m<-dayGroup
        temp2m$doy<-temp2m$doy-2
        dayGroup=rbind.data.frame(temp2p,temp1p,temp0,temp1m,temp2m)
      rm(temp0,temp1m,temp1p,temp2m,temp2p)
      # find LT 1 and GT 365 
      dayGroup$doy[dayGroup$doy <= 0] <- 1
      dayGroup$doy[dayGroup$doy > 365] <- 365
      # add in a day 366
      temp<-dayGroup[dayGroup$doy==365,]
        temp$doy<-366
      dayGroup<-rbind.data.frame(dayGroup,temp)
      # calc daily stats
      dayQuant<- dayGroup %>% group_by(doy) %>%
                              summarise(median=quantile(bhrs20,0.50,na.rm=TRUE),
                                        avg=mean(bhrs20, na.rm = TRUE),
                                        iqr=IQR(bhrs20, na.rm = TRUE))
      # add in smooth 
      temp1<-dayQuant
        temp1$doy<-temp1$doy+366
      temp2<-temp1
        temp2$doy<-temp2$doy+366
      smoothDay<-rbind.data.frame(dayQuant,temp1,temp2)
      
      smoothDay<-smoothDay %>%
                  mutate(
                         rollMean29_avg_bhrs = zoo::rollmean(avg, k=29, fill=NA, align='center'),
                         rollMean29_med_bhrs = zoo::rollmean(median, k=29, fill=NA, align='center')
                         )
      smoothDay<-subset(smoothDay, doy>=367 & doy<=732)
      smoothDay$doy<-smoothDay$doy-366
      
      # library(ggplot2)
      # ggplot(smoothDay)+
      #   geom_point(aes(doy,rollMean29_avg_bhrs),color="red")+
      #   geom_point(aes(doy,rollMean29_med_bhrs),color="blue")+
      #   geom_point(aes(doy,rollMed2_bhrs),color="orange")+
      #   geom_point(aes(doy,rollMean2_bhrs))

      # merge to burn hours, calc anomaly
      burnHRS<-merge(burnHRS, smoothDay[,c("doy","rollMean29_avg_bhrs","rollMean29_med_bhrs")], by="doy")
      burnHRS$bhrs20_med_anom<-burnHRS$bhrs20-burnHRS$rollMean29_med_bhrs
      burnHRS$bhrs20_avg_anom<-burnHRS$bhrs20-burnHRS$rollMean29_avg_bhrs
      
      # p<-ggplot(burnHRS)+
      #   geom_point(aes(date,bhrs20_med_anom))
      # plotly::ggplotly(p)
      #####
      
      
      # put into list
      burnList[[k]]<-burnHRS
        k=k+1
      
      # day RH stats
      monthRH<-data.frame(do.call(rbind,tapply(tempRAWS$RelativeHumidity..., tempRAWS$month, summary))) 
      annual <- data.frame(t(unclass(summary(tempRAWS$RelativeHumidity...))),  # Convert summary to data frame
                           check.names = FALSE)
      colnames(annual)<-colnames(monthRH)
      monthRH<-rbind.data.frame(monthRH, annual)
      monthRH$month<-1:13
      # add in station data
      monthRH$lat<-lat
      monthRH$lon<-lon
      monthRH$sta_name<-stnName
      # put into list
      rhDayList[[l]]<-monthRH
        l=l+1
      
      # diurnal RH stats
      hourRH<-data.frame(do.call(rbind,tapply(tempRAWS$RelativeHumidity..., tempRAWS$hour, summary))) 
      hourRH$hour<-0:23
      # add in station data
      hourRH$lat<-lat
      hourRH$lon<-lon
      hourRH$sta_name<-stnName
      # put into list
      rhHrList[[m]]<-hourRH
        m=m+1
      
      # diurnal RH/month stats
      hourMoRH<-tempRAWS %>% group_by(hour, month) %>%
        summarize(avgRH=mean(RelativeHumidity...),
                  medRH=median(RelativeHumidity...))
      # ggplot(hourMoRH)+
      #   geom_point(aes(hour,avgRH, color=as.factor(month)))
      rhHrMoList[[n]]<-hourMoRH
        n=n+1
      
      # print loop update
      print(paste0(stnName,", ",i," out of ",length(file_list)," (",lat,",",lon,")"))
    }

}

save(burnList,rhDayList,rhHrList, file = "./data/burnClimoList.RData")
#####

