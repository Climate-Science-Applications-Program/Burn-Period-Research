# script to build dataframe from NOAA ISD-lite dataset
# MAC 8/3/2023
# adapted from isdLite_AZTempTrends/analysis.R
# data info https://www.ncei.noaa.gov/pub/data/noaa/
# https://www.ncei.noaa.gov/pub/data/noaa/isd-lite/isd-lite-format.txt

##### get ISD history list
library(tidyverse)
url<-"https://www.ncei.noaa.gov/pub/data/noaa/isd-history.txt"
download.file(url,destfile="isd_history.txt")
isdHistory<-read_fwf("isd_history.txt",
               skip=20)
colnames(isdHistory)<-isdHistory[1,]
isdHistory<-isdHistory[2:nrow(isdHistory),]
# trim rows with missing data
isdHistory<-isdHistory %>% drop_na(USAF)
# split out years from begin/end cols
isdHistory<-separate(isdHistory, BEGIN, into = c("begYear", "begMoDy"), sep=4)
isdHistory<-separate(isdHistory, END, into = c("endYear", "endMoDy"), sep=4)

##### set up station list
stnList<-list()

# alt method - create station/yr list for Flagstaff ----
temp<-
  isdHistory[which(stringr::str_detect(isdHistory$`STATION NAME`, 'FLAGSTAFF')==TRUE),]
meta<-temp[which.max(as.numeric(temp$endYear)),]

stns<-as.data.frame(rbind(cbind(seq(1948,1950,1), rep("723755-03103",length(seq(1948,1950,1)))),
                          cbind(seq(1951,1972,1), rep("999999-03103",length(seq(1951,1972,1)))),
                          cbind(seq(1973,1976,1), rep("723750-99999",length(seq(1973,1976,1)))),
                          cbind(seq(1977,2004,1), rep("723755-03103",length(seq(1977,2004,1)))),
                          cbind(seq(2005,2022,1), rep("723750-03103",length(seq(2005,2022,1))))))
colnames(stns)<-c("yr","stn")
stns$stn<-as.character(stns$stn)
stns<-cbind.data.frame(meta[,1:9],stns)
  stnList[[1]]<-stns
#####
  
##### Tucson station list -----
  temp<-
    isdHistory[which(stringr::str_detect(isdHistory$`STATION NAME`, 'TUCSON')==TRUE),]
  meta<-temp[which.max(as.numeric(temp$endYear)),]
  stns<-as.data.frame(rbind(cbind(seq(1948,1972,1), rep("999999-23160",length(seq(1948,1972,1)))),
                            cbind(seq(1973,2022,1), rep("722740-23160",length(seq(1973,2022,1))))))
  colnames(stns)<-c("yr","stn")
  stns$stn<-as.character(stns$stn)
  stns<-cbind.data.frame(meta[,1:9],stns)
  stnList[[2]]<-stns
#stn1<-"722740-23160" # Tucson 1973-2019
#stn2<-"999999-23160" # Tucson 1946-1972
#####

###### Phoenix stations ----
  temp<-
    isdHistory[which(stringr::str_detect(isdHistory$`STATION NAME`, 'PHOENIX SKY')==TRUE),]
  meta<-temp[which.max(as.numeric(temp$endYear)),]
  stns<-as.data.frame(rbind(cbind(seq(1948,1972,1), rep("999999-23183",length(seq(1948,1972,1)))),
                            cbind(seq(1973,2022,1), rep("722780-23183",length(seq(1973,2022,1))))))
  colnames(stns)<-c("yr","stn")
  stns$stn<-as.character(stns$stn)
  stns<-cbind.data.frame(meta[,1:9],stns)
  stnList[[3]]<-stns
# phoenix
#stn1<-"722780-23183" # Phoenix 1973-2019 
#stn2<-"999999-23183" # Phoenix 1948-1972
#####

##### Albuquerque -----
  temp<-
    isdHistory[which(stringr::str_detect(isdHistory$`STATION NAME`, 'ALBUQUERQUE')==TRUE),]
  meta<-temp[which.max(as.numeric(temp$endYear)),]
  stns<-as.data.frame(rbind(cbind(seq(1941,1970,1), rep("723650-23050",length(seq(1941,1970,1)))),
                            cbind(seq(1971,1972,1), rep("999999-23050",length(seq(1971,1972,1)))),
                            cbind(seq(1973,2022,1), rep("723650-23050",length(seq(1973,2022,1))))
                            ))
  colnames(stns)<-c("yr","stn")
  stns$stn<-as.character(stns$stn)
  stns<-cbind.data.frame(meta[,1:9],stns)
  stnList[[4]]<-stns
######

##### El Paso -----
  temp<-
    isdHistory[which(stringr::str_detect(isdHistory$`STATION NAME`, 'EL PASO')==TRUE),]
  meta<-temp[which.max(as.numeric(temp$endYear)),]
  stns<-as.data.frame(rbind(cbind(seq(1941,1966,1), rep("722700-23044",length(seq(1941,1966,1)))),
                            cbind(seq(1967,1971,1), rep("999999-23044",length(seq(1967,1971,1)))),
                            cbind(seq(1972,2022,1), rep("722700-23044",length(seq(1972,2022,1))))
  ))
  colnames(stns)<-c("yr","stn")
  stns$stn<-as.character(stns$stn)
  stns<-cbind.data.frame(meta[,1:9],stns)
  stnList[[5]]<-stns
  
# add Roswell and El Paso ----- 
  # temp<-
  #   isdHistory[which(stringr::str_detect(isdHistory$`STATION NAME`, 'ROSWELL')==TRUE),]
  # meta<-temp[which.max(as.numeric(temp$endYear)),]
  # # stns<-as.data.frame(rbind(cbind(seq(1942,1947,1), rep("722680-23009",length(seq(1942,1947,1)))),
  # #                           cbind(seq(1948,1969,1), rep("999999-23043",length(seq(1948,1969,1)))),
  # #                           cbind(seq(1970,2022,1), rep("722680-23009",length(seq(1970,2022,1))))
  # # ))
  # stns<-as.data.frame(cbind(seq(1970,2022,1), rep("722680-23009",length(seq(1970,2022,1)))))
  # colnames(stns)<-c("yr","stn")
  # stns$stn<-as.character(stns$stn)
  # stns<-cbind.data.frame(meta[,1:9],stns)

# set vars
baseurl<-"ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/"
suffix<-".gz"

isdData<-list()

for(j in 1:length(stnList)){
  
  datalist = list()
  stns<-stnList[[j]]
  # loop through all years for station
  for(i in 1:nrow(stns)){
    #stn<-ifelse(yrs[i]<=switchYr, stn2, stn1)
    stn<-stns$stn[i]
    url<-paste(baseurl,as.character(stns$yr[i]),'/',stn,'-',as.character(stns$yr[i]),suffix, sep="")  
    download.file(url,destfile="tmp.gz")
    temp<-read.table(gzfile("tmp.gz"))
    temp<-cbind.data.frame(stns[i,],temp)
    datalist[[i]] <- temp
  }
  # combine into dataframe
  climData = do.call(rbind, datalist)
  
  # format df
  colnames=c("year","month","day","hour","temp","dewpoint","mslp","wdir","wspd","sky","precip1hr",
             "precip6hr")
  # set column names
  colnames(climData)[12:23]<-colnames
  # replace 999 with na
  climData[climData == -9999] <- NA
  # replace neg 1hr precip -0.1 Trace to 0.001
  climData$precip1hr[climData$precip1hr == -0.1] <- 0.001
  # scale column values
  climData$temp<-climData$temp*0.1
  climData$dewpoint<-climData$dewpoint*0.1
  #climData$dewpoint<-celsius.to.fahrenheit(climData$dewpoint, round = 2)
  climData$mslp<-climData$mslp*0.1
  climData$wspd<-climData$wspd*0.1
  climData$precip1hr<-climData$precip1hr*0.1
  climData$precip6hr<-climData$precip6hr*0.1
  
  # create date/time field and day ofyear; assumes all observations are present even if NA
  # HOURS IN UTC! Convert to LST
  climData$date_hr<-as.POSIXct(strptime(paste(climData$year, climData$month, climData$day, climData$hour),"%Y %m %d %H"), tz="GMT")
  climData$date_hr<-as.POSIXct(format(climData$date_hr, tz="America/Phoenix",usetz=TRUE),"%Y %m %d %H")
  climData$doy<-strptime(climData$date_hr, "%Y-%m-%d")$yday+1  
  climData$date<-as.Date(paste0(climData$year,"-",climData$month,"-",climData$day),"%Y-%m-%d")
  climData$hour_lst<-format(climData$date_hr, "%H")
  
  climData$RH <- weathermetrics::dewpoint.to.humidity(t = climData$temp,
                                                      dp = climData$dewpoint,
                                                      temperature.metric = 'celsius')
  
  # save datafile 
  isdData[[j]]<-climData
  
}

# save data list
save(isdData, file="./data/isd/isdHourly.RData")

#####
# process hourly into daily means/metrics
# adapted from burn period climo
load("~/RProjects/BurnPeriodResearch/data/isd/isdHourly.RData")

library(dplyr)
library(plantecophys)
library(firebehavioR)

burnListISD<-list()

for(i in 1:length(isdData)){
  
  # put df into temp 
  temp<-isdData[[i]]
  
  # calculate VPD
  lat<-as.numeric(as.character(temp$LAT[1]))
  lon<-as.numeric(as.character(temp$LON[1]))
  elev<-elevatr::get_elev_point(data.frame(x=lon,y=lat),prj = "EPSG:4326", src = "aws")
  # calc air pressure based on elevation
  kPa<-(101325*((1-(2.25577*10^-5)*elev@data$elevation)^5.25588))/1000
  # VPD
  temp$vpd_kPa<-RHtoVPD(temp$RH, temp$temp, Pa = kPa)

  # calculate fire weather indices
  # calc fire weather indices
  fireWXidx = fireIndex(temp=temp$temp, u=temp$wspd*3.6, rh = temp$RH)
  # combine with temp df
  temp$ffwi<-fireWXidx$fosberg
  temp$hdw<-fireWXidx$hotDryWindy
  
  # CALC BURN PERIOD
  # count of burn hours
  burnHRS<-  temp %>%
    group_by(year, month, day) %>%
    summarize(n_hours = n(),
              bhrs25 = sum(RH <= 25, na.rm = TRUE),
              bhrs20 = sum(RH <= 20, na.rm = TRUE),
              bhrs15 = sum(RH <= 15, na.rm = TRUE),
              bhrs10 = sum(RH <= 10, na.rm = TRUE),
              bhrs5 = sum(RH <= 5, na.rm = TRUE),
              minRH = min(as.numeric(as.character(RH)), na.rm = TRUE),
              maxRH = max(as.numeric(as.character(RH)), na.rm = TRUE),
              meanRH = mean(as.numeric(as.character(RH)), na.rm = TRUE),
              minVPD= min(vpd_kPa, na.rm=TRUE),
              maxVPD= max(vpd_kPa, na.rm=TRUE),
              meanVPD= mean(vpd_kPa, na.rm=TRUE),
              minDP = min(dewpoint, na.rm = TRUE),
              maxDP = max(dewpoint, na.rm = TRUE),
              meanDP = mean(dewpoint, na.rm = TRUE),
              sdDP = sd(dewpoint, na.rm = TRUE),
              maxWS= max(wspd, na.rm=TRUE),
              minT=min(temp, na.rm = TRUE),
              maxT= max(temp, na.rm = TRUE),
              meanT=mean(temp, na.rm = TRUE),
              sdT=sd(temp, na.rm = TRUE),
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
  burnHRS$STA_NAME<-temp$`STATION NAME`[1]
  #burnHRS$PSA_NAME<-sw_rawsDF$PSA_NAME[i]
  burnHRS$StationNum<-temp$WBAN[1]
  #elev<-elevatr::get_elev_point(data.frame(x=lon,y=lat),prj = "EPSG:4326", src = "aws")
  burnHRS$elev_ft<-round(elev@data$elevation*3.28084,0)
  
  # add in doy
  burnHRS$doy<-as.numeric(format(burnHRS$date, "%j"))
  burnHRS$day<-as.numeric(format(burnHRS$date, "%d"))
  burnHRS$year<-as.numeric(format(burnHRS$date, "%Y"))
  burnHRS$month<-as.numeric(format(burnHRS$date, "%m"))
  
  # limit to set period of record
  burnHRS<-subset(burnHRS, year>=1949)
  
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
  burnListISD[[i]]<-burnHRS

}

save(burnListISD, file = "./data/isd/ISD_burnClimoList.RData")

#####





