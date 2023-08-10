# derivation of burn period, analysis of driving vars
# MAC 07/24/23

# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# get monthly average burn hours per station 
burnListDF<-do.call(rbind, burnList)

minT<-min(burnListDF$minT)
maxT<-max(burnListDF$maxT)

minDP<-min(burnListDF$minDP)
maxDP<-max(burnListDF$maxDP)

summary(burnListDF$minDP)
summary(burnListDF$maxDP)

# create combinations
minT <-round(((-20-32)*(5/9)),0)
maxT <-round(((120-32)*(5/9)),0)
minDP<-round(((-20-32)*(5/9)),0)
maxDP<-round(((80-32)*(5/9)),0)
df <- as.data.frame(expand.grid(minT:maxT, minDP:maxDP))   
colnames(df)<-c("temp","dewpoint")

library(dplyr)    
df %>% 
  distinct(temp, dewpoint)

df$rh<-weathermetrics::dewpoint.to.humidity(df$dewpoint,df$temp,temperature.metric = "celsius")
df$rh20<-ifelse(df$rh<=20,1,NA)
df$rhCut<-cut(df$rh,breaks=c(0,5,10,15,20,100))

df$vpd_kPa<-plantecophys::RHtoVPD(df$rh,df$temp,Pa=101)  

library(ggplot2)
ggplot(df, aes(x = dewpoint, y = temp, fill = rhCut)) +
  geom_tile()+
  ggtitle("Dewpoint v Temperature and RH Thresholds")

df$vpd_kPa_rh20<-ifelse(df$rh<=20,df$vpd_kPa,NA)
ggplot(df, aes(x = dewpoint, y = temp, fill = vpd_kPa_rh20)) +
  geom_tile()+
  scale_fill_gradientn(colours = terrain.colors(10))+
  ggtitle("Dewpoint v Temperature and VPD (filter to RH<20%)")+
  theme_bw()

