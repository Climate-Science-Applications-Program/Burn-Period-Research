# develop fire/climate stats by PSA 
# MAC 07/22/23

library(dplyr)
library(ggplot2)
library(raster)
library(plotly)
#library(colorspace)

##### load and join data ----
# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# ggplot inset data
states <- map_data("state")

#####
# load spatial data
# psa zones
psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/BurnPeriodTracker/shapes", layer="National_PSA_Current")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")
# get psa centroids for factor order
sw_psaDF<- cbind.data.frame(sw_psa, rgeos::gCentroid(sw_psa,byid=TRUE))
sw_psa_df<-fortify(sw_psa)
#####

# get area of sw psa
#f <- sf::st_read("/home/crimmins/RProjects/BurnPeriodTracker/shapes/National_PSA_Current.shp")



# get monthly average burn hours per station 
burnListDF<-do.call(rbind, burnList)

# get station list and lat/lons
stnLatLon<-burnListDF %>% group_by(STA_NAME) %>%
  summarise(lat=first(LATITUDE),
            lon=first(LONGITUDE))

# assign PSA to RAWS
coordinates(stnLatLon)<- ~ lon + lat
proj4string(stnLatLon) <- proj4string(sw_psa)
stnPSA<-sp::over(stnLatLon,sw_psa)
stnLatLon$psa <- stnPSA$PSANationa

burnListDF<-merge(burnListDF,stnLatLon@data, by="STA_NAME")
#####


##### year
yrStats<- burnListDF %>% group_by(year) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            meanMaxVPD=mean(maxVPD, na.rm=TRUE),
            q50Bhrs20=quantile(bhrs20, probs=0.50),
            q90Bhrs20=quantile(bhrs20, probs=0.90),
            meanBhrs20_anom=mean(bhrs20_med_anom))
ggplot(yrStats, aes(year,meanBhrs20))+
  geom_line()+
  ggtitle("Annual Avg Burn Period (<20%) - AZ/NM 2000-2002")+
  theme_bw()

ggplot(yrStats, aes(year,meanMaxVPD))+
  geom_line()+
  ggtitle("Annual Avg Max VPD (<20%) - AZ/NM 2000-2002")+
  theme_bw()

ggplot(yrStats, aes(year,stdevBhrs20))+
  geom_line()+
  ggtitle("Annual Avg Max VPD (<20%) - AZ/NM 2000-2002")+
  theme_bw()

##### mo/year
moyrStats<- burnListDF %>% group_by(year,month) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            meanMaxVPD=mean(maxVPD, na.rm=TRUE),
            meanMeanVPD=mean(meanVPD, na.rm=TRUE),
            meanMeanRH=mean(meanRH, na.rm=TRUE),
            meanMeanT=mean(meanT, na.rm=TRUE),
            meanMeanDP=mean(meanDP, na.rm=TRUE),
            sdMaxVPD=sd(maxVPD, na.rm=TRUE),
            q50Bhrs20=quantile(bhrs20, probs=0.50),
            q90Bhrs20=quantile(bhrs20, probs=0.90),
            meanBhrs20_anom=mean(bhrs20_med_anom))

temp<-moyrStats
temp<-subset(moyrStats, month %in% c(6,7))

ggplot(temp, aes(year,meanBhrs20, color=as.factor(month), group=as.factor(month)))+
  geom_line()+
  geom_point()+
  ggtitle("Monthly Average Burn Period (RH<20%)")+
  theme_bw()

ggplot(temp, aes(year,meanBhrs20, fill=as.factor(month), group=as.factor(month)))+
  geom_bar(stat="identity", position = "dodge")+
  #geom_line()+
  #geom_point()+
  geom_hline(yintercept = mean(subset(temp, month==6)$meanBhrs20))+
  geom_hline(yintercept = mean(subset(temp, month==7)$meanBhrs20))+
  ggtitle("Monthly Average Burn Period (RH<20%)")+
  theme_bw()

mean(subset(temp, month==6)$meanBhrs20)

ggplot(temp, aes(year,meanMeanDP, color=as.factor(month), group=as.factor(month)))+
  geom_line()+
  geom_point()+
  ggtitle("Monthly Average Dewpoint (C)")+
  theme_bw()
ggplot(temp, aes(meanBhrs20,meanMeanDP,color=as.factor(month), group=as.factor(month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Monthly Burn Period (RH<%20) vs Mean DP")


ggplot(temp, aes(year))+
  geom_line(aes(y=meanBhrs20, color=as.factor(month), group=as.factor(month)))+
  geom_line(aes(y=meanMeanDP, color=as.factor(month), group=as.factor(month)))
  #scale_y_continuous(sec.axis = sec_axis(~ . * 5))


ggplot(temp, aes(year,meanMaxVPD, color=as.factor(month), group=as.factor(month)))+
  geom_line()+
  ggtitle("Monthly Average maxVPD")
ggplot(temp, aes(meanBhrs20,meanMeanVPD,color=as.factor(month), group=as.factor(month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Monthly Burn Period (RH<%20) vs Mean VPD")

ggplot(temp, aes(meanMeanRH,meanMeanDP,color=as.factor(month), group=as.factor(month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Monthly Mean RH vs Mean T")


p<-ggplot(temp, aes(month,meanBhrs20, color=as.factor(year), group=as.factor(year)))+
  geom_line()
ggplotly(p)

ggplot(burnListDF, aes(bhrs20,meanDP))+
  geom_point() +
  geom_hline(yintercept = 20)+
  theme_bw()+
  ggtitle("Burn period (RH<20) vs Daily Avg Dewpoint")


# mo/yr/psa
moPSAStats<- burnListDF %>% group_by(month,year,psa) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            meanMaxVPD=mean(maxVPD, na.rm=TRUE),
            meanMeanVPD=mean(meanVPD, na.rm=TRUE),
            meanMeanRH=mean(meanRH, na.rm=TRUE),
            meanMeanT=mean(meanT, na.rm=TRUE),
            meanMeanDP=mean(meanDP, na.rm=TRUE),
            sdMaxVPD=sd(maxVPD, na.rm=TRUE),
            meanAnom=mean(bhrs20_med_anom),
            q50Bhrs20=quantile(bhrs20, probs=0.50),
            q90Bhrs20=quantile(bhrs20, probs=0.90))
            #bhrs20vmaxVPD=cor(bhrs20,maxVPD),
            #bhrs20vmaxVPD=as.numeric(cor.test(bhrs20, maxVPD, method = "spearman")$estimate),
            #bhrs20vminDP=as.numeric(cor.test(bhrs20, minDP, method = "spearman")$estimate),
            #bhrs20vmaxT=as.numeric(cor.test(bhrs20, maxT, method = "spearman")$estimate),
            #elev=first(elev),
            #lat=first(LATITUDE),
            #lon=first(LONGITUDE))
moPSAStats$date<-as.Date(paste0(moPSAStats$year,"-",moPSAStats$month,"-01"))

# plot results
temp<-moPSAStats
temp<-subset(moPSAStats, month %in% c(8))
temp<-subset(temp, !is.na(psa))

ggplot(temp, aes(year,meanBhrs20, color=psa))+
  geom_line()+
  geom_point()+
  ggtitle("August Average Burn Period (RH<20%)")+
  theme_bw()


ggplot(temp, aes(date,meanBhrs20))+
  geom_line()+
  facet_wrap(.~psa)

ggplot(temp, aes(date,meanBhrs20,fill=psa))+
geom_bar(position="dodge", stat="identity")
geom_line()

ggplot(temp, aes(meanBhrs20,meanMaxVPD,color=month))+
  geom_point()

# psa correlations/regions
psaBhrs<-tidyr::spread(moPSAStats[,c(1:4)], psa, meanBhrs20)

round(cor(psaBhrs[,3:18]), 2)
# pca
# https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r
pca_psa        <- prcomp(psaBhrs[,3:17], center=T, scale=T)
rawLoadings     <- pca_psa$rotation[,1:ncomp] %*% diag(pca_psa$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(psaBhrs[,3:17]) %*% invLoadings
print(scores[1:5,])                   # Scores computed via rotated loadings

temp<-subset(moPSAStats, psa %in% c("SW01","SW06S","SW10","SW14N"))

ggplot(temp, aes(date,meanBhrs20, color=psa))+
  geom_line()
  #facet_wrap(.~psa, ncol=1)

p<-ggplot(temp, aes(meanBhrs20,meanMeanVPD, color=as.factor(month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(.~psa, ncol=1)
plotly::ggplotly(p)



# month climo stats
moStats<- burnListDF %>% group_by(month) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            medBhrs20=median(bhrs20, na.rm = TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            meanMaxVPD=mean(maxVPD, na.rm=TRUE),
            q50Bhrs20=quantile(bhrs20, probs=0.50),
            q90Bhrs20=quantile(bhrs20, probs=0.90))


# VPD vs burn hours relation
temp<-subset(burnListDF, psa=="SW06S")
temp<-subset(temp,STA_NAME=="EMPIRE")

temp2<- temp %>% group_by(year,month) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            meanMaxVPD=mean(maxVPD, na.rm=TRUE),
            sdMaxVPD=sd(maxVPD, na.rm=TRUE),
            q50Bhrs20=quantile(bhrs20, probs=0.50),
            q90Bhrs20=quantile(bhrs20, probs=0.90),
            meanBhrs20_anom=mean(bhrs20_med_anom),
            count=n())

ggplot(temp2, aes(meanBhrs20,meanMaxVPD,color=as.factor(month), group=as.factor(month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Monthly Burn Period (RH<%20) vs Mean Max VPD")
# look at 5/2006 and 8/2009
temp3<-subset(temp, year==2006 & month==5)
temp3$tempDPdiff<-temp3$meanT-temp3$meanDP
  plot(temp3$maxVPD,temp3$bhrs20,ylim = c(0, 24))
  plot(temp3$meanVPD,temp3$meanT)
temp4<-subset(temp, year==2009 & month==8)
temp4$tempDPdiff<-temp4$meanT-temp4$meanDP
  plot(temp4$meanVPD,temp4$meanT)
  points(temp4$maxVPD,temp4$bhrs20, col="red")


##### FOD data ----
# swap in MTBS for longer record?

# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")

proj4string(fc) <- proj4string(sw_psa)
temp<-sp::over(fc,sw_psa)
fc$psa <- temp$PSANationa

fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,"%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,"%m/%d/%Y")
fc$fire_duration<-as.numeric(fc$CONT_DATE-fc$DISCOVERY_DATE)+1
fc$month<-as.numeric(format(fc$DISCOVERY_DATE, "%m"))

##### year Stats
yrFires<- fc@data %>% group_by(FIRE_YEAR) %>%
  summarise(totalAC=sum(FIRE_SIZE),
            totalFires=n(),
            avgDuration=mean(fire_duration, na.rm=TRUE))

yrStats<-merge(yrStats,yrFires, by.x="year",by.y="FIRE_YEAR")
yrStats$logAc<-log(yrStats$totalAC)
yrStats$logFires<-log(yrStats$totalFires)

temp<-yrStats[,c("meanBhrs20","meanMaxVPD","logAc")]
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
GGally::ggpairs(temp, title="correlogram with ggpairs()",lower = list(continuous = my_fn)) 


ggplot(yrStats, aes(logAc,meanBhrs20))+
  geom_point()+
  geom_smooth(method="lm")

temp<-tidyr::gather(yrStats,var,value, 2:13)
temp<-subset(temp, var %in% c("meanBhrs20","meanMaxVPD","logAc"))

ggplot(temp, aes(year,value,color=var))+
  geom_line()

ggplot(temp,aes())

ggplot(temp, aes(year,value))+
  geom_line()+
  facet_wrap(.~var, scales = "free")

ggplot(yrStats, aes(meanMaxVPD,totalAC))+
  geom_point()

#corr matrix
library("PerformanceAnalytics")
#my_data <- fireEventsRAWS[, c(2:9,16:19,21:35,43:44)]
#my_data <- fireEventsRAWS[, c(2:9,16:19,43:44)]
#my_data <- fireEventsRAWS[, c(2:9,17,43:44)]
chart.Correlation(yrStats, histogram=TRUE, pch=19,method = c("spearman"))

##### month stats
temp<-fc@data %>% group_by(month,FIRE_YEAR) %>%
  summarise(TotalAC=sum(FIRE_SIZE),
            TotalFires=n(),
            avgDuration=mean(fire_duration, na.rm=TRUE),
            medDuration=median(fire_duration, na.rm = TRUE))

temp<-subset(temp, FIRE_YEAR>=2000)

moFiresMed<- temp %>% group_by(month) %>%
  summarise(avgTotalAC=mean(TotalAC),
            medTotalAC=median(TotalAC),
            avgTotalFires=mean(TotalFires),
            medTotalFires=median(TotalFires),
            avgDuration=mean(avgDuration, na.rm=TRUE),
            medDuration=median(medDuration, na.rm = TRUE))

# moFires<- fc@data %>% group_by(month) %>%
#   summarise(avgTotalAC=sum(FIRE_SIZE)/((max(FIRE_YEAR)-min(FIRE_YEAR))+1),
#             avgTotalFires=n()/((max(FIRE_YEAR)-min(FIRE_YEAR))+1),
#             avgDuration=mean(fire_duration, na.rm=TRUE))

ggplot(moFires, aes(month,avgDuration))+
  geom_bar(stat = "identity")

moFires<-merge(moStats,moFiresMed, by.x="month",by.y="month")

ggplot(moFires, aes(meanBhrs20,meanMaxVPD,color=as.factor(month)))+
  geom_point()+
  ggtitle("Monthly Avg VPD vs BurnHours20")

ggplot(moFires, aes(avgTotalAC,meanBhrs20,color=as.factor(month)))+
  geom_point()+
  ggtitle("Monthly Avg VPD vs BurnHours20")

#temp<-tidyr::gather(moFires,var,value, 2:10)
#temp<-subset(temp, var %in% c("meanBhrs20","meanMaxVPD","avgTotalAC","avgTotalFires"))
temp<-tidyr::gather(moFires,var,value, 2:14)
temp<-subset(temp, var %in% c("medBhrs20","medTotalAC","medTotalFires"))



ggplot(temp, aes(month,value))+
  geom_bar(stat="identity")+
  scale_x_continuous(name="month", breaks=seq(1,12,1))+
  facet_wrap(.~var, scales="free", ncol=1)+
  ggtitle("Average Monthly Fire Stats - AZ/NM 2000-2020")

##### mo/yr fire-climate 
moYrFires<- fc@data %>% group_by(month,FIRE_YEAR) %>%
  summarise(totalAC=sum(FIRE_SIZE),
            totalFires=n(),
            avgDuration=mean(fire_duration, na.rm=TRUE))

moYrStats<-merge(moyrStats,moYrFires, by.x=c("month","year"),by.y=c("month","FIRE_YEAR"))

temp<-moYrStats
temp<-subset(moYrStats, month %in% c(3:8))

p1<-ggplot(temp, aes(year,totalAC, fill=as.factor(month), group=as.factor(month)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_line()+
  #geom_point()+
  ggtitle("Monthly Total Ac burned")+
  theme_bw()

p2<-ggplot(temp, aes(year,meanBhrs20, fill=as.factor(month), group=as.factor(month)))+
  geom_bar(stat = "identity", position = "dodge")+
  #geom_line()+
  #geom_point()+
  ggtitle("Mean Burn Hours")+
  theme_bw()

p3<-cowplot::plot_grid(p1,p2,labels=c("a.","b."), ncol=1,align="v", rel_heights = c(1, 1))

ggplot(temp, aes(meanBhrs20,log(totalAC), color=as.factor(month), group=as.factor(month)))+
  #geom_bar(stat = "identity", position = "dodge")+
  #geom_line()+
  geom_point()+
  geom_smooth(method="lm")+
  ggtitle("Mean Burn Hours")+
  theme_bw()


temp<-moYrStats
temp<-subset(moYrStats, month %in% c(4:7))

fireYr<-temp %>% group_by(year) %>% 
                  summarize(bhrs20=mean(meanBhrs20),
                            maxVPD=mean(meanMaxVPD),
                            totalAC=sum(totalAC))
fireYr$logAc<-log(fireYr$totalAC)

temp<-fireYr[,c("bhrs20","maxVPD","logAc")]
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
GGally::ggpairs(temp, title="SW Fire Season (Annual) Correlations",lower = list(continuous = my_fn)) 

cor(fireYr$bhrs20,fireYr$maxVPD, method = "pearson")
cor(fireYr$bhrs20,fireYr$logAc, method = "pearson")
cor(fireYr$maxVPD,fireYr$logAc, method = "pearson")



##### mo/yr/psa fire-climate
moYrPSAFires<- fc@data %>% group_by(month,FIRE_YEAR,psa) %>%
  summarise(totalAC=sum(FIRE_SIZE),
            totalFires=n(),
            avgDuration=mean(fire_duration, na.rm=TRUE))

moYrPSAStats<-merge(moPSAStats,moYrPSAFires, by.x=c("month","year","psa"),by.y=c("month","FIRE_YEAR","psa"))

temp<-moYrPSAStats
temp<-subset(moYrPSAStats, month %in% c(3:8))

corMoYr <- temp %>% group_by(psa) %>%
                            summarise(Bhrs20vAc=cor(meanBhrs20,log(totalAC)),
                                      Bhrs10vAc=cor(meanBhrs10,log(totalAC)),
                                      VPDmaxvAc=cor(meanMaxVPD,log(totalAC)),
                                      VPDmeanvAc=cor(meanMeanVPD,log(totalAC)),
                                      VPDmaxvBhrs20=cor(meanMaxVPD,meanBhrs20),
                                      Bhrs20anomVac=cor(meanAnom,log(totalAC)),
                                      )
summary(corMoYr)


ggplot(temp, aes(meanBhrs20,log(totalAC), color=as.factor(month)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(.~psa)+
  ggtitle("Mean monthly Burn Hours vs Monthly Total Acres Burned ~ PSA")

