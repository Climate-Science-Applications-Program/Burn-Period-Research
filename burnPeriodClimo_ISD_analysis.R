# climatological analysis of burn period and other fire metrics
# needs data from isd_download.R
# adapted from burnPeriodClimo_analysis.R
# MAC 08/05/23

library(dplyr)
library(ggplot2)
#library(colorspace)

# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/isd/ISD_burnClimoList.RData")

# ggplot inset data
states <- map_data("state")

# get monthly average burn hours per station 
burnListDF<-do.call(rbind, burnListISD)

# monthl/year summary
moYrStats<- burnListDF %>% group_by(STA_NAME,month,year) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            medBhrs20=median(bhrs20, na.rm=TRUE),
            madBhrs20=mad(bhrs20, na.rm = TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            iqrBhrs20=IQR(bhrs20, na.rm = TRUE),
            q50Bhrs20=quantile(bhrs20, probs=0.50, na.rm=TRUE),
            q90Bhrs20=quantile(bhrs20, probs=0.90, na.rm=TRUE),
            #bhrs20vmaxVPD=cor(bhrs20,maxVPD),
            #bhrs20vmaxVPD=as.numeric(cor.test(bhrs20, maxVPD, method = "spearman")$estimate),
            #bhrs20vminDP=as.numeric(cor.test(bhrs20, minDP, method = "spearman")$estimate),
            #bhrs20vmaxT=as.numeric(cor.test(bhrs20, maxT, method = "spearman")$estimate),
            meanMaxT=mean(maxT,na.rm=TRUE),
            meanMinDP=mean(minDP,na.rm=TRUE),
            meanMinRH=mean(minRH, na.rm=TRUE),
            meanMeanT=mean(meanT,na.rm=TRUE),
            meanMeanDP=mean(meanDP,na.rm=TRUE),
            meanMeanRH=mean(meanRH, na.rm=TRUE),
            sdMeanT=sd(meanT, na.rm = TRUE),
            sdMeanDP=sd(meanDP, na.rm = TRUE),
            elev=first(elev_ft),
            lat=first(LATITUDE),
            lon=first(LONGITUDE))

moYrStats$date<-as.Date(paste0(moYrStats$year,"-",moYrStats$month,"-01"))

# plot results

temp<-moYrStats[,c("STA_NAME","date","meanBhrs20")]

ggplot(temp, aes(date,sdMeanDP, color=STA_NAME))+
  geom_line()+
  geom_smooth(method = 'lm')+
  facet_wrap(.~STA_NAME, ncol=1)

# compare two stations... TIA and Saguaro
temp<-subset(temp, STA_NAME=="TUCSON INTERNATIONAL AIRPORT")
temp<-subset(temp, date>="2000-01-01")
temp$sdMeanT<-(temp$sdMeanT*9/5)
# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")
# get monthly average burn hours per station 
burnListDF<-do.call(rbind, burnList)
temp2<-subset(burnListDF, STA_NAME=="SAGUARO")

# monthl/year summary ----
moYrStats2<- temp2 %>% group_by(STA_NAME,month,year) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            medBhrs20=median(bhrs20, na.rm=TRUE),
            madBhrs20=mad(bhrs20, na.rm = TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            iqrBhrs20=IQR(bhrs20, na.rm = TRUE),
            q50Bhrs20=quantile(bhrs20, probs=0.50, na.rm=TRUE),
            q90Bhrs20=quantile(bhrs20, probs=0.90, na.rm=TRUE),
            #bhrs20vmaxVPD=cor(bhrs20,maxVPD),
            #bhrs20vmaxVPD=as.numeric(cor.test(bhrs20, maxVPD, method = "spearman")$estimate),
            #bhrs20vminDP=as.numeric(cor.test(bhrs20, minDP, method = "spearman")$estimate),
            #bhrs20vmaxT=as.numeric(cor.test(bhrs20, maxT, method = "spearman")$estimate),
            meanMaxT=mean(maxT,na.rm=TRUE),
            meanMinDP=mean(minDP,na.rm=TRUE),
            meanMinRH=mean(minRH, na.rm=TRUE),
            meanMeanT=mean(meanT,na.rm=TRUE),
            meanMeanDP=mean(meanDP,na.rm=TRUE),
            meanMeanRH=mean(meanRH, na.rm=TRUE),
            sdMeanT=sd(meanT, na.rm = TRUE),
            sdMeanDP=sd(meanDP, na.rm = TRUE),
            elev=first(elev),
            lat=first(LATITUDE),
            lon=first(LONGITUDE))
moYrStats2$date<-as.Date(paste0(moYrStats2$year,"-",moYrStats2$month,"-01"))
temp2<-moYrStats2[,c("STA_NAME","date","meanBhrs20")]
  temp3<-rbind.data.frame(temp,temp2)
# TIA vs Saguaro RAWS  
  ggplot(temp3, aes(date,meanBhrs20, color=STA_NAME))+
    geom_line() +theme(legend.position="bottom") 
  
temp3<- tidyr::pivot_wider(temp3, names_from = STA_NAME, values_from = meanBhrs20)
temp3$month<-as.numeric(format(temp3$date, "%m"))
  ggplot(temp3, aes(`TUCSON INTERNATIONAL AIRPORT`, `SAGUARO`, color=as.factor(month)))+
    geom_point()+
    geom_abline(slope = 1)+
    ggtitle("Mean Burn Period (hrs) 2000-2022")
cor(temp3$`TUCSON INTERNATIONAL AIRPORT`,temp3$SAGUARO)
  tdr::tdStats(temp3$`TUCSON INTERNATIONAL AIRPORT`,temp3$SAGUARO)
  
# monthly summary
moStats<- burnListDF %>% group_by(STA_NAME,month) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            medBhrs20=median(bhrs20, na.rm=TRUE),
            madBhrs20=mad(bhrs20, na.rm = TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            iqrBhrs20=IQR(bhrs20, na.rm = TRUE),
            q50Bhrs20=quantile(bhrs20, probs=0.50, na.rm=TRUE),
            q90Bhrs20=quantile(bhrs20, probs=0.90, na.rm=TRUE),
            #bhrs20vmaxVPD=cor(bhrs20,maxVPD),
            #bhrs20vmaxVPD=as.numeric(cor.test(bhrs20, maxVPD, method = "spearman")$estimate),
            #bhrs20vminDP=as.numeric(cor.test(bhrs20, minDP, method = "spearman")$estimate),
            #bhrs20vmaxT=as.numeric(cor.test(bhrs20, maxT, method = "spearman")$estimate),
            meanMaxT=mean(maxT,na.rm=TRUE),
            meanMinDP=mean(minDP,na.rm=TRUE),
            meanMinRH=mean(minRH, na.rm=TRUE),
            meanMeanT=mean(meanT,na.rm=TRUE),
            meanMeanDP=mean(meanDP,na.rm=TRUE),
            meanMeanRH=mean(meanRH, na.rm=TRUE),
            sdMeanT=sd(meanT, na.rm = TRUE),
            sdMeanDP=sd(meanDP, na.rm = TRUE),
            elev=first(elev_ft),
            lat=first(LATITUDE),
            lon=first(LONGITUDE))

temp<- moStats %>% group_by(month) %>%
  summarise(avgSDTemp=mean(sdMeanT),
            avgSDdp=mean(sdMeanDP))
temp<-tidyr::gather(temp,var,value, 2:3)

ggplot(temp, aes(month,value, fill=var))+
  geom_bar(position=position_dodge(),stat="identity")+
  scale_x_continuous(name="month", breaks=seq(1,12,1))+
  ggtitle("ISD Stations - monthly var")


# compare two stations... TIA and Saguaro


