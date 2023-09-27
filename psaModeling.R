# daily fire probability modeling by PSA
# MAC 08/25/23
# adapted from psaAnalysis.R

library(dplyr)
library(ggplot2)
library(raster)
#library(plotly)
#library(colorspace)

##### load and join data ----
# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# ggplot inset data
#states <- map_data("state")

#####
# load spatial data
# psa zones
psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/BurnPeriodTracker/shapes", layer="National_PSA_Current")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")
# get psa centroids for factor order
sw_psaDF<- cbind.data.frame(sw_psa, rgeos::gCentroid(sw_psa,byid=TRUE))
sw_psa_df<-fortify(sw_psa)
#####

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

#####
# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")

proj4string(fc) <- proj4string(sw_psa)
temp<-sp::over(fc,sw_psa)
fc$psa <- temp$PSANationa

fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,"%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,"%m/%d/%Y")
fc$fire_duration<-as.numeric(fc$CONT_DATE-fc$DISCOVERY_DATE)+1
fc$month<-as.numeric(format(fc$DISCOVERY_DATE, "%m"))
#####


#####
# calc daily averages by PSA, total fire counts per day and psa
psaBhrs<- burnListDF %>% group_by(date, psa) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            meanBhrs10=mean(bhrs10, na.rm=TRUE))

psaFires<-fc@data %>% group_by(DISCOVERY_DATE, psa) %>%
  summarize(totalFires=n())

#####


#####
# merge weather and fire DFs
psaFires<-merge(psaBhrs, psaFires, by.x=c('date','psa'), by.y=c('DISCOVERY_DATE','psa'), all.x=TRUE)
psaFires$month<-as.numeric(format(psaFires$date,"%m"))
psaFires$year<-as.numeric(format(psaFires$date,"%Y"))
psaFires$totalFires[is.na(psaFires$totalFires)]<-0
psaFires$fireDay<-ifelse(psaFires$totalFires>0,1,0)

ggplot(psaFires, aes(meanBhrs20,fireDay,color=as.factor(month)))+
  geom_point(alpha=0.5)+
  facet_wrap(.~psa)


#####
# subset psa/month, try logistic regression
temp<-subset(psaFires, psa=="SW06N")

ggplot(temp, aes(meanBhrs20,fireDay,color=as.factor(month)))+
  geom_point(alpha=0.5)+
  facet_wrap(.~month)

temp<-subset(temp, month==7)

#fit logistic regression model https://www.statology.org/plot-logistic-regression-in-r/
model <- glm(fireDay ~ meanBhrs20, data=temp, family=binomial)

#define new data frame that contains predictor variable
newdata <- data.frame(meanBhrs20=seq(min(temp$meanBhrs20), max(temp$meanBhrs20),len=500))

#use fitted model to predict values of vs
newdata$fireDay = predict(model, newdata, type="response")

#plot logistic regression curve
plot(fireDay ~ meanBhrs20, data=temp, col="steelblue")
lines(fireDay ~ meanBhrs20, newdata, lwd=2)











