# climatological analysis of burn period and other fire metrics
# needs data from burnPeriodClimo.R
# MAC 06/29/23

library(dplyr)
library(ggplot2)
library(colorspace)

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


# get monthly average burn hours per station 
burnListDF<-do.call(rbind, burnList)

# correlations of all obs
 colnames(burnListDF)
 my_data <- burnListDF[, c(8,10,12:27, 35,36)]
cor(my_data, method = "spearman")
GGally::ggcorr(my_data)
rm(my_data)
# PerformanceAnalytics::chart.Correlation(my_data, histogram=TRUE, pch=19)
#as.numeric(cor.test(burnListDF$bhrs20, burnListDF$minVPD, method = "spearman")$estimate)


moStats<- burnListDF %>% group_by(STA_NAME,month) %>%
                        summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
                                  meanBhrs10=mean(bhrs10, na.rm=TRUE),
                                  medBhrs20=median(bhrs20, na.rm=TRUE),
                                  madBhrs20=mad(bhrs20, na.rm = TRUE),
                                  stdevBhrs20=sd(bhrs20, na.rm = TRUE),
                                  iqrBhrs20=IQR(bhrs20, na.rm = TRUE),
                                  q50Bhrs20=quantile(bhrs20, probs=0.50),
                                  q90Bhrs20=quantile(bhrs20, probs=0.90),
                                  #bhrs20vmaxVPD=cor(bhrs20,maxVPD),
                                  bhrs20vmaxVPD=as.numeric(cor.test(bhrs20, maxVPD, method = "spearman")$estimate),
                                  bhrs20vminDP=as.numeric(cor.test(bhrs20, minDP, method = "spearman")$estimate),
                                  bhrs20vmaxT=as.numeric(cor.test(bhrs20, maxT, method = "spearman")$estimate),
                                  bhrs20vmeanWS=as.numeric(cor.test(bhrs20, meanWS, method = "spearman")$estimate),
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


# get station list and lat/lons
stnLatLon<-burnListDF %>% group_by(STA_NAME) %>%
                      summarise(lat=first(LATITUDE),
                                lon=first(LONGITUDE))

# distributions of bhrs
ggplot(burnListDF, aes(bhrs20))+
  geom_histogram(bins=25)+
  scale_x_continuous(breaks = seq(0, 24, 1))+
  facet_wrap(.~month)+
  ggtitle("Burn Period (RH<20%)")

# correlation with fire metrics
cor(burnListDF$bhrs20, burnListDF$maxVPD)
test<-as.numeric(cor.test(burnListDF$bhrs20, burnListDF$maxVPD, method = "spearman")$estimate)

# dist of correlations
ggplot(moStats, aes(bhrs20vmaxVPD))+
  geom_histogram(bins=20)+
  #scale_x_continuous(breaks = seq(0, 1, 0.05))+
  facet_wrap(.~month)+
  ggtitle("maxVPD vs Burn Hours (RH<20) correlations")


# average
p<-ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = moStats, aes(x = lon, y = lat, color=medBhrs20), size=1.5)+
  scale_color_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="Burn Period (hrs)", limits=c(0,24))+
  facet_wrap(.~month)+
  ggtitle("Median Burn Period (RH<20%)")
p
# variability
p<-ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = moStats, aes(x = lon, y = lat, color=madBhrs20), size=1.5)+
  scale_color_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="Burn Period (hrs)")+
  facet_wrap(.~month)+
  ggtitle("IQR Burn Period (RH<20%)")
p
# both median and variability
p<-ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = moStats, aes(x = lon, y = lat, color=medBhrs20, size=iqrBhrs20))+
  scale_color_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="Median (hrs)")+
  #scale_color_gradientn(colors = c('#225ea8','#41b6c4','#c7e9b4','#ffffd9'),name="Median (hrs)")+
  #scale_size(range = c(0.5, 2), name="IQR (hrs)")+
  scale_size_binned(breaks=c(0,1,6,12,18,24),range = c(0.25, 3),name="IQR (hrs)")+
  facet_wrap(.~month)+
  ggtitle("Median and IQR Burn Period (RH<20%)")+
  theme_bw()
p

# monthly climate 
p<-ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = moStats, aes(x = lon, y = lat, color=bhrs20vmeanWS), size=1.5)+
  #scale_color_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="r")+
  scale_color_gradient2(low="blue",mid="grey",high="red", midpoint = 0)+
  facet_wrap(.~month)+
  ggtitle("Burn Hrs 20 v mean WS")
p

temp<- moStats %>% group_by(month) %>%
  summarise(avgMeanT=mean(meanMeanT),
            avgMeanDP=mean(meanMeanDP),
            avgMeanRH=mean(meanMeanRH),
            avgSDTemp=mean(sdMeanT),
            avgSDdp=mean(sdMeanDP),
            medBhrs20=median(medBhrs20),
            meanBhrs20=mean(meanBhrs20)
            )
temp<-tidyr::gather(temp,var,value, 5:6)
#temp<-subset(temp, var %in% c("medBhrs20","medTotalAC","medTotalFires"))
ggplot(temp, aes(month,value))+
  geom_bar(stat="identity")+
  scale_x_continuous(name="month", breaks=seq(1,12,1))+
  facet_wrap(.~var, scales="free", ncol=1)+
  ggtitle("Average Monthly Fire Stats - AZ/NM 2000-2020")

temp<- moStats %>% group_by(month) %>%
  summarise(avgSDTemp=mean(sdMeanT),
            avgSDdp=mean(sdMeanDP))
temp<-tidyr::gather(temp,var,value, 2:3)

ggplot(temp, aes(month,value, fill=var))+
  geom_bar(position=position_dodge(),stat="identity")+
  scale_x_continuous(name="month", breaks=seq(1,12,1))


# correlations
p<-ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = moStats, aes(x = lon, y = lat, color=bhrs20vmaxVPD), size=1.5)+
  scale_color_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="r")+
  facet_wrap(.~month)+
  ggtitle("maxVPD v Burn Period (RH<20) correlations")
p

# elev vs burn hours
ggplot(moStats, aes(elev,meanBhrs20))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  facet_wrap(.~month)+
  ggtitle("Elevation vs Burn Hours (RH<20%)")

# monthly variability metrics by mo/yr then mo

moYrStats<- burnListDF %>% group_by(STA_NAME,month,year) %>%
  summarise(medBhrs20=median(bhrs20, na.rm=TRUE),
            iqrBhrs20=IQR(bhrs20, na.rm = TRUE),
            meanMeanT=mean(meanT,na.rm=TRUE),
            meanMeanDP=mean(meanDP,na.rm=TRUE),
            sdMeanT=sd(meanT, na.rm = TRUE),
            sdMeanDP=sd(meanDP, na.rm = TRUE),
            elev=first(elev),
            lat=first(LATITUDE),
            lon=first(LONGITUDE))

moStats<- moYrStats %>% group_by(STA_NAME,month) %>%
  summarise(medBhrs20=mean(medBhrs20, na.rm=TRUE),
            iqrBhrs20=mean(iqrBhrs20, na.rm = TRUE),
            meanMeanT=mean(meanMeanT,na.rm=TRUE),
            meanMeanDP=mean(meanMeanDP,na.rm=TRUE),
            sdMeanT=mean(sdMeanT, na.rm = TRUE),
            sdMeanDP=mean(sdMeanDP, na.rm = TRUE),
            elev=first(elev),
            lat=first(lat),
            lon=first(lon))

p<-ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = moStats, aes(x = lon, y = lat, color=iqrBhrs20), size=1.5)+
  scale_color_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="r")+
  #scale_color_gradient2(low="blue",mid="grey",high="red", midpoint = 0)+
  facet_wrap(.~month)+
  ggtitle("SD Dew Point")
p



# to do...
# season length based on burn period metric
# time series analysis of temporal variability
# fire season based on percentile threshold, count of hours above threshold
# first and last day of year crosses 50th tile?
# develop model of temp/dp on RH and burn hours
# quantreg with maxT/minDP
# 

##### quantreg trends -----

rqList<-list()

for(i in 1:length(burnList)){
  temp<-burnList[[i]]
  temp<-subset(temp, month %in% c(6))
  temp$dateNum<-as.numeric(temp$date)
  
  rq_model <- quantreg::rq(
    formula = bhrs20 ~ year,
    tau = c(0.5,0.9),
    data = temp
  )
  #stats<-broom::tidy(x = rq_model, se="nid")
  # stats<-cbind.data.frame(temp$STA_NAME[1],temp$LATITUDE[1],temp$LONGITUDE[1],
  #                         stats$estimate[2],stats$estimate[4])
  stats<-cbind.data.frame(temp$STA_NAME[1],temp$LATITUDE[1],temp$LONGITUDE[1],
                          rq_model[["coefficients"]][2],rq_model[["coefficients"]][4])
  
  colnames(stats)<-c("Station","lat","lon","r50","r90")
  rqList[[i]]<-stats
}
  
rqStats<-do.call(rbind,rqList)

# plot trends
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = rqStats, aes(x = lon, y = lat, color=r90), size=1.5)+
  scale_color_gradient2(low="blue",mid="grey",high="red", midpoint=0) +
  ggtitle("Trend in median Burn Period (RH<20%) (April-May-June)")

# test<-burnList[[2]]
# ggplot(test, aes(date,bhrs20))+
#   geom_point()+
#   geom_quantile(quantiles=c(0.5,0.9))
#####


# model burn hours by temp and dp variability
library(broom)
library(dplyr)
library(tidyr)
library(purrr)

# calculate anomalies
anoms<-burnListDF[,c("month","STA_NAME","meanDP","meanT","bhrs20","doy","date","bhrs20_avg_anom","bhrs20_med_anom")]
meanDOY<- anoms %>% group_by(STA_NAME,doy) %>%
                    summarise(meanTmean=mean(meanT),
                              meanDPmean=mean(meanDP))
anoms<-merge(anoms, meanDOY, by=c("STA_NAME","doy"))
  anoms$anomDP<-anoms$meanDP-anoms$meanDPmean
  anoms$anomT<-anoms$meanT-anoms$meanTmean

# raw values models ----- 
#moModels<- burnListDF %>% group_by(STA_NAME,month) %>%
#  do(tidy(lm(bhrs20 ~ meanDP + meanT, .)))
# scaled version https://stackoverflow.com/questions/24305271/extracting-standardized-coefficients-from-lm-in-r
moModels<- burnListDF %>% group_by(STA_NAME,month) %>%
  do(tidy(lm(scale(bhrs20) ~ scale(meanDP) + scale(meanT), .)))
moModelsR2<- burnListDF %>% group_by(STA_NAME,month) %>%
  do(glance(lm(scale(bhrs20) ~ scale(meanDP) + scale(meanT), .)))
summary(moModelsR2$r.squared)
r2<-moModelsR2 %>% group_by(month) %>%
  summarize(avgR2=mean(r.squared),
            minR2=min(r.squared),
            maxR2=max(r.squared),
            medR2=median(r.squared))
boxplot(r.squared~month,moModelsR2)

# get summary stats of model output
moSumm<-moModels %>%  group_by(term, month) %>%
      summarise(meanEst=mean(estimate),
                meanPval=mean(p.value))
ggplot(subset(moSumm, term %in% c("scale(meanT)","scale(meanDP)")), aes(month,meanEst, fill=term))+
  geom_bar(stat = "identity")

# subset models
moModels<-merge(moModels, stnLatLon, by="STA_NAME")
  tModel<-subset(moModels, term=="scale(maxT)")
  dpModel<-subset(moModels, term=="scale(minDP)")
#####
  
# anomaly model -----
# anomModels<- anoms %>% group_by(STA_NAME,month) %>%
#     do(tidy(lm(bhrs20 ~ anomDP + anomTx, .)))  
anomModels<- anoms %>% group_by(STA_NAME,month) %>%
  do(tidy(lm(scale(bhrs20_med_anom) ~ scale(anomDP) + scale(anomT), .)))  
anomModels2<- anoms %>% group_by(STA_NAME,month) %>%
  do(glance(lm(scale(bhrs20_med_anom) ~ scale(anomDP) + scale(anomT), .)))  

# quick look at model diagnostics
summary(anomModels2$r.squared)
r2<-anomModels2 %>% group_by(month) %>%
  summarize(avgR2=mean(r.squared),
            minR2=min(r.squared),
            maxR2=max(r.squared),
            medR2=median(r.squared))
boxplot(r.squared~month,moModelsR2)

# get summary stats of model output
anomSumm<-anomModels %>%  group_by(term, month) %>%
  summarise(meanEst=mean(estimate),
            meanPval=mean(p.value))
# ggplot(moSumm, aes(month,meanEst, fill=term))+
#   geom_bar(stat = "identity")
ggplot(subset(anomSumm, term %in% c("scale(anomDP)","scale(anomT)")), aes(month,meanEst, fill=term))+
  geom_bar(stat = "identity")
temp<-spread(anomSumm[,1:3],term,meanEst, drop = TRUE)
temp$ratio<-(temp$`scale(anomDP)`^2)/(temp$`scale(anomT)`^2)
  
# subset models
anomModels<-merge(anomModels, stnLatLon, by="STA_NAME")
  tModel<-subset(anomModels, term=="anomT")
  dpModel<-subset(anomModels, term=="anomDP")

estDiff<-spread(anomModels[,1:4], key=term, value=estimate)
  estDiff$diff<-estDiff$`scale(anomDP)`+estDiff$`scale(anomT)`
  estDiff<-merge(estDiff, stnLatLon, by="STA_NAME")
estSumm<-estDiff %>%  group_by(month) %>%
    summarise(meanDiff=mean(diff))

anomModels2<-merge(anomModels2, stnLatLon, by="STA_NAME")
#####

#####
# https://stackoverflow.com/questions/61201770/is-it-enough-to-use-group-by-in-order-to-run-a-grouped-linear-regression  
# model_one <- function(data) {
#   lm(bhrs20~minDP + maxT, data = data)
# }
# models <- burnListDF %>%
#   group_by(STA_NAME,month) %>%
#   tidyr::nest() %>%
#   mutate(model = map(data, model_one))
# summarized <- models %>%
#   mutate(summary = map(model, broom::tidy)) %>%
#   unnest(summary)
#####

# plot raw value coefficients
  ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
    geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
    #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
    coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
    geom_point(data = subset(anomModels, term=="scale(anomT)"), aes(x = lon, y = lat, color=estimate), size=1.5)+
    scale_color_gradient2(low="blue",mid="grey",high="red", midpoint=0) +
    facet_wrap(.~month)+
    ggtitle("Anom MeanT coefficients (scale(bhrs20_med_anom) ~ scale(anomDP) + scale(anomT))")  

ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="gray94", color="grey", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = anomModels2, aes(x = lon, y = lat, color=r.squared), size=1.5)+
  scale_color_gradient2(low="blue",mid="yellow",high="red", midpoint=0.5) +
  facet_wrap(.~month)+
  ggtitle("Model R.Squared (scale(bhrs20_med_anom) ~ scale(anomDP) + scale(anomT))")  

##### plot time series of station data -----
which(unique(burnListDF$STA_NAME)=="FLAGSTAFF")
temp<-subset(anoms, STA_NAME=="FLAGSTAFF")
temp<-subset(temp, date>="2010-06-20" & date<="2010-06-30")
temp<-temp[,c("date","bhrs20_med_anom","anomT","anomDP","meanT","meanDP","bhrs20")]
temp<-tidyr::gather(temp, var, value, 2:4)

ggplot(temp, aes(date,value, color=var))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ggtitle("FLAGSTAFF RAWS - Schulz Fire 6/20-6/30")

# get RAWS time series for comparison with raws.dri.edu data
temp<-subset(burnListDF, STA_NAME=="SAGUARO")
temp<-subset(temp, date>="2020-05-01" & date<="2020-08-01")
temp<-temp[,c("date","bhrs20_med_anom","anomT","anomDP")]



#####

#####


# trying out poisson regression -----
# https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/
library(MASS)

#test<-subset(burnListDF, STA_NAME=="SAGUARO")
test<-subset(anoms, STA_NAME=="LAKESIDE")
  test<-subset(test, month==2)
  #test$minDP<-round(test$minDP,0)
hist(test$bhrs20_avg_anom)
mean(test$bhrs20_avg_anom)
var(test$bhrs20_avg_anom)

my_data <- test[, c(6,8,12,13)]
cor(my_data, method = "spearman")

lm.model<-lm(bhrs20_avg_anom~anomDP+anomTx, data=test)
  summary(lm.model)
  lm(data.frame(scale(lm.model$model))) # standardized coefficients
  plot(fitted(lm.model), resid(lm.model))
    abline(0,0)
ggplot(test, aes(date,bhrs20_avg_anom))+
  geom_point()
plot(x=predict(lm.model), y= test$bhrs20_avg_anom,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)
test$pred<-predict(lm.model)
ggplot(test)+
  geom_point(aes(date,bhrs20_avg_anom), color="green")+
  geom_point(aes(date,pred), color="red")
  

        
  ggplot(test, aes(anomDP, bhrs20_avg_anom))+
    geom_point()+
    geom_smooth(method = "lm")
  ggplot(test, aes(anomTx, bhrs20_avg_anom))+
    geom_point()+
    geom_smooth(method = "lm")
  poisson.model <- glm(bhrs20_avg_anom~anomDP+anomTx, test, family = poisson(link = "log"))
  summary(poisson.model)
  plot(fitted(poisson.model), resid(poisson.model))
    abline(0,0)

# diagnostic plots    
jtools::plot_summs(poisson.model, scale = TRUE, exp = TRUE)
jtools::plot_summs(lm.model, scale = TRUE, exp = FALSE)    
    
# predict with model    
newdat <- data.frame(minDP = seq(30,60,1), maxT=seq(80,110,1))
newdat$predLM <- predict(lm.model, newdata = newdat, type = "response")
newdat$predPOIS <- predict(poisson.model, newdata = newdat, type = "response")      
  plot(bhrs20~minDP, test)
  lines(predLM ~ minDP, newdat, col = 2)
  lines(predPOIS ~ minDP, newdat, col = 4)
  legend("topleft", legend = c("lm", "poisson"), col = c(2,4), lty = 1)
  plot(bhrs20~maxT, test)
  lines(predLM ~ maxT, newdat, col = 2)
  lines(predPOIS ~ maxT, newdat, col = 4)
  legend("topleft", legend = c("lm", "poisson"), col = c(2,4), lty = 1)
#####  


# https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
# summary(m1 <- glm.nb(bhrs20~minDP+maxT, data=test))
# summary(m3 <- glm(bhrs20~minDP+maxT, family = "poisson", data = test))
# 
# m3 <- glm(bhrs20~minDP+maxT, family = "poisson", data = test)
# pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)
# (est <- cbind(Estimate = coef(m1), confint(m1)))
# exp(est)

#####
# https://stackoverflow.com/questions/60982870/how-to-plot-a-linear-regression-model-and-a-poisson-regression-model-on-the-same
# summary(test$minDP)
# summary(test$maxT)
# fitLM <- glm(bhrs20~minDP, test, family = gaussian(link = "identity"))
# summary(fitLM)
# fitPOIS <- glm(bhrs20~minDP, test, family = poisson(link = "log"))
# summary(fitPOIS)
# AIC(fitLM, fitPOIS) # poisson has lower AIC
# 
# newdat <- data.frame(minDP = seq(1,70,1))
# newdat$predLM <- predict(fitLM, newdata = newdat, type = "response")
# newdat$predPOIS <- predict(fitPOIS, newdata = newdat, type = "response")
# 
# plot(bhrs20~minDP, test)
# lines(predLM ~ minDP, newdat, col = 2)
# lines(predPOIS ~ minDP, newdat, col = 4)
# legend("topleft", legend = c("lm", "poisson"), col = c(2,4), lty = 1)
#####