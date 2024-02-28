# manuscript figures for Burn Period Tracker paper
# MAC 10/2/23
# code from multiple scripts

library(dplyr)
library(ggplot2)
library(colorspace)
library(raster)

# load burnperiod climo dataset
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# ggplot inset data
states <- map_data("state")
countries<-map_data("world")
countries<-subset(countries, region %in% c("Canada","Mexico"))


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
# create master DF
burnListDF<-merge(burnListDF,stnLatLon@data, by="STA_NAME")
# add in month labels
burnListDF$moLab<-factor(month.abb[burnListDF$month],levels = month.abb[1:12])
# convert temps from F to C
burnListDF$meanT<-(burnListDF$meanT-32)*(5/9)
burnListDF$meanDP<-(burnListDF$meanDP-32)*(5/9)


# drop RAWS outside of SW GACC
burnListDF<-subset(burnListDF, !is.na(psa))

#####

##### FIGURE 2 - overview map ----

# get stats
annStats<- burnListDF %>% group_by(STA_NAME) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            medBhrs20=median(bhrs20, na.rm=TRUE),
            madBhrs20=mad(bhrs20, na.rm = TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            iqrBhrs20=IQR(bhrs20, na.rm = TRUE),
            elev=first(elev),
            lat=first(LATITUDE),
            lon=first(LONGITUDE))

library(elevatr)
library(tidyverse)
library(raster)
library(marmap)

# Turn the matrix into a raster
rast <- raster::raster(matrix(rnorm(400),1000,1000))
# Give it lat/lon coords for 36-37°E, 3-2°S
raster::extent(rast) <- (raster::extent(sw_psa))*1.1

# ... and assign a projection
raster::projection(rast) <- sp::CRS("+proj=longlat +datum=WGS84")

# z zoom ranges from 0 (low) to 14 (high)
elev <- get_elev_raster(
  locations = rast, 
  prj= "+proj=longlat +datum=WGS84 +no_defs",
  z = 6, 
  clip = "locations") 

elev_df <- as.data.frame(elev, xy = T) %>%
  na.omit()
colnames(elev_df)[3] <- "elevation"

elev_map <- ggplot() +
  geom_tile(data = elev_df, 
            aes(x = x, y = y, fill = elevation)) +
  scale_fill_etopo() +
  # geom_contour(data=elev_df,aes(x=x,y=y,z=elevation),
  #              breaks=c(0, 1000, 2000, 3000, 4000),
  #              colour="black", size=0.2
  # ) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="grey17", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill=NA, color="grey30", size=0.5)+
  geom_point(data=annStats, aes(x = lon, y = lat), size=2, shape=21, fill="grey87",color="black", alpha=1)+
  #coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")+
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  #theme_minimal()+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  geom_text(data=sw_psaDF,aes(label=PSANationa, 
            x=x,
            y=y),
            size = 3.5, hjust=0.5, vjust=0, fontface="bold", color="grey1") 
  # theme(text = element_text(family = "georg", color = "#22211d"),
  #       axis.line = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       
  #       panel.grid.major = element_line(color = "white", size = 0.2),
  #       panel.grid.minor = element_blank(),
  #       plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
  #       plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
  #       plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
  #       plot.background = element_rect(fill = "white", color = NA), 
  #       panel.background = element_rect(fill = "white", color = NA),
  #       panel.border = element_blank())

sw_psa_df<-fortify(sw_psa)
#stationLatLon<-temp[1,c("LATITUDE","LONGITUDE")]

##### inset map1 ----
# insetmap<-ggplot() +
#   geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
#   geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="grey88", color="black", size=0.05)  + # get the state border back on top
#   #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
#   coord_fixed(xlim=c(-125, -66), ylim=c(25, 49), ratio = 1) +
#   #geom_point(data = stationLatLon, aes(x = LONGITUDE, y = LATITUDE), size=0.75, color='red')+
#   theme_bw(base_size=5)+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
#####

##### inset map2 ----
insetmap<-ggplot() + 
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), fill="grey", color="grey", size=0.1)  +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill="white", color="grey", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill="grey88", color="black", size=0.05)  + # get the state border back on top
  coord_equal( xlim = c(-123, -69), ylim = c(24, 51))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "powderblue"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        line = element_blank(),
        plot.margin=grid::unit(c(0,0,-1,-1), "mm"))
#####

#(-123-(-69))/(51-24)

g <- ggplotGrob(insetmap)
elev_map<-elev_map + annotation_custom(grob = g, ymin = 31.05, ymax = 32.30, xmin = -114.95, xmax = -112.5)

#(-115-(-112.5))/(32.45-31.2)

## add in legend for stations, DEM elevs?, scale?, inset overview map

ggsave(filename="./figures/Fig2_overviewMap.png", width=7, height=4, dpi = 600, device='png', elev_map)

#####

##### FIGURE 2 -- monthly station stats ----

moStats<- burnListDF %>% group_by(STA_NAME, month) %>%
  summarise(meanBhrs20=mean(bhrs20, na.rm=TRUE),
            medBhrs20=median(bhrs20, na.rm=TRUE),
            madBhrs20=mad(bhrs20, na.rm = TRUE),
            stdevBhrs20=sd(bhrs20, na.rm = TRUE),
            iqrBhrs20=IQR(bhrs20, na.rm = TRUE),
            meanMeanT=mean(meanT,na.rm=TRUE),
            meanMeanDP=mean(meanDP,na.rm=TRUE),
            sdMeanT=sd(meanT, na.rm = TRUE),
            sdMeanDP=sd(meanDP, na.rm = TRUE),
            elev=first(elev),
            lat=first(LATITUDE),
            lon=first(LONGITUDE))

moStats$moLab<-factor(month.abb[moStats$month],levels = month.abb[1:12])

# global month stats
moStatsAll<- moStats %>% group_by(month) %>%
  summarise(
            medBhrs20_mean=round(mean(medBhrs20, na.rm=TRUE),1),
            iqrBhrs20_mean=round(mean(iqrBhrs20, na.rm = TRUE),1),
            bhrs_gt_8=sum(medBhrs20>=8,na.rm = TRUE),
            bhrs_gt_12=sum(medBhrs20>=12,na.rm = TRUE),
            n_stations = n())
moStatsAll$moLab<-factor(month.abb[moStatsAll$month],levels = month.abb[1:12])
moStatsAll$label<-paste0(moStatsAll$medBhrs20_mean," (\u00b1",moStatsAll$iqrBhrs20_mean,")")

# both median and variability
p<-ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="grey50", size=0.2)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill=NA, color="grey43", alpha=0.8)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  geom_point(data = moStats, aes(x = lon, y = lat, fill=medBhrs20, size=iqrBhrs20), shape=21, color='black', alpha=0.8)+
  scale_fill_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="Median (hrs)")+
  #scale_fill_gradientn(colors = rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')),
  #                     name="Median (hrs)")+
  #scale_color_gradientn(colors = c('#225ea8','#41b6c4','#c7e9b4','#ffffd9'),name="Median (hrs)")+
  #scale_size(range = c(0.5, 2), name="IQR (hrs)")+
  scale_size_binned(breaks=c(0,1,6,12,18,24),range = c(0.25, 3),name="IQR (hrs)")+
  facet_wrap(.~moLab, ncol=3)+
  #ggtitle("Median and IQR Burn Period (RH<20%)")+
  theme_bw()+
  #theme_classic()+
  #theme_minimal()+
  theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=10),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "bottom")+
  geom_text(x = -113.6, y = 31.2, aes(label = label), data = moStatsAll, size=3.25)


ggsave(filename="./figures/Fig3_moBPstats.png", width=8.5, height=8, dpi = 600, device='png', p)

# some stats for interpretation


#####

##### FIGURE 3 PSA boxplots ----
# use PSA daily mean or RAWS level data?

p<-ggplot(subset(burnListDF, psa %in% c("SW02","SW06S","SW12")), aes(y=bhrs20, x=moLab,fill=psa))+
  geom_boxplot(outlier.shape = NA)+
  scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"),name="")+
  scale_y_continuous(limits=c(0,24), breaks = c(0,6,12,18,24))+
  ylab("hours/day")+
  #xlab("")+
  theme_bw()+
  theme(legend.position="bottom",
        axis.title.x = element_blank())

p
ggsave(filename="./figures/Fig4_moPSAbox.png", width=7, height=5, dpi = 600, device='png', p)
#####

##### FIGURE 4 temp/dewpoint modeling -----

# model burn hours by temp and dp variability
library(broom)
library(purrr)

# calculate anomalies
anoms<-burnListDF[,c("month","STA_NAME","meanDP","meanT","bhrs20","doy","date","bhrs20_avg_anom","bhrs20_med_anom")]
# convert F to C
#anoms$meanDP<-(anoms$meanDP-32)*(5/9)
#anoms$meanT<-(anoms$meanT-32)*(5/9)
# group and merge
meanDOY<- anoms %>% group_by(STA_NAME,doy) %>%
  summarise(meanTmean=mean(meanT),
            meanDPmean=mean(meanDP),
            medBhrs20=median(bhrs20))
# smoothed means
meanDOY<-meanDOY %>% group_by(STA_NAME) %>%
  mutate(
    meanTmean_sm = zoo::na.fill(zoo::rollmean(meanTmean, k=10, fill=NA, align='center'),"extend"),
    meanDPmean_sm = zoo::na.fill(zoo::rollmean(meanDPmean, k=10, fill=NA, align='center'),"extend")
  )


anoms<-merge(anoms, meanDOY, by=c("STA_NAME","doy"))
# create anoms
anoms$anomDP<-anoms$meanDP-anoms$meanDPmean
anoms$anomT<-anoms$meanT-anoms$meanTmean
# create anoms -- smoothed means
#anoms$anomDP<-anoms$meanDP-anoms$meanDPmean_sm
#anoms$anomT<-anoms$meanT-anoms$meanTmean_sm

# BP means
anoms$anomBhrs20<-anoms$bhrs20-anoms$medBhrs20

# calculate regressions
anomModels<- anoms %>% group_by(STA_NAME,month) %>%
  do(tidy(lm(scale(anomBhrs20) ~ scale(anomDP) + scale(anomT), .)))  
anomModels2<- anoms %>% group_by(STA_NAME,month) %>%
  do(glance(lm(scale(anomBhrs20) ~ scale(anomDP) + scale(anomT), .)))  

# quick look at model diagnostics
summary(anomModels2$r.squared)
r2<-anomModels2 %>% group_by(month) %>%
  summarize(avgR2=mean(r.squared),
            minR2=min(r.squared),
            maxR2=max(r.squared),
            medR2=median(r.squared))
boxplot(r.squared~month,anomModels2)

# get summary stats of model output
anomSumm<-anomModels %>%  group_by(term, month) %>%
  summarise(meanEst=mean(estimate),
            sdEst=sd(estimate),
            meanPval=mean(p.value))
anomSumm$moLab<-factor(month.abb[anomSumm$month],levels = month.abb[1:12])


p<-ggplot(subset(anomSumm, term %in% c("scale(anomDP)","scale(anomT)")), aes(moLab,meanEst, fill=term))+
  geom_bar(stat = "identity")+
  geom_errorbar(data=subset(anomSumm, term %in% c("scale(anomDP)","scale(anomT)")),
                aes(x=moLab, ymin=meanEst-sdEst, ymax=meanEst+sdEst),
                width=0.4, colour="orange", alpha=0.9, size=1.3)+
  scale_fill_manual(values=c("#1b9e77","#d95f02"), labels = c("dewpoint temperature", "air temperature"),name="")+
  ylab("standardized coefficient")+
  xlab("month")+
  theme_bw()+
  theme(legend.position="bottom",
        axis.title.x = element_blank())

ggsave(filename="./figures/Fig5_moCoeffs.png", width=7, height=5, dpi = 600, device='png', p)


# boxplot
# anomModels$moLab<-factor(month.abb[anomModels$month],levels = month.abb[1:12])
# ggplot(subset(anomModels, term %in% c("scale(anomDP)","scale(anomT)")), 
#        aes(y=estimate, x=moLab,fill=term))+
#   geom_boxplot(outlier.shape = NA)+
#   scale_fill_manual(values=c("#1b9e77","#d95f02"))+
#   #scale_y_continuous(limits=c(0,24), breaks = c(0,6,12,18,24))+
#   ylab("scaled coefficient")+
#   xlab("month")+
#   geom_hline(yintercept = 0)+
#   theme_bw()+
#   theme(legend.position="bottom")

# interpreting beta coefficients page 183 Kachigan
temp<-tidyr::spread(anomSumm[,1:3],term,meanEst, drop = TRUE)
temp$ratio<-(temp$`scale(anomDP)`^2)/(temp$`scale(anomT)`^2)

(mean(temp$`scale(anomDP)`)^2)/(mean(temp$`scale(anomT)`)^2)

summary(temp$`scale(anomDP)`)
summary(temp$`scale(anomT)`)
summary(temp$ratio)


# # both temperature median and variability ----
# p<-ggplot() +
#   geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="grey50", size=0.2)  +
#   geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill=NA, color="grey43", alpha=0.8)  + # get the state border back on top
#   #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
#   coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
#   geom_point(data = moStats, aes(x = lon, y = lat, fill=meanMeanT, size=sdMeanT), shape=21, color='black', alpha=0.8)+
#   scale_fill_gradientn(colors = c('#74add1','#fee090','#f46d43','#a50026'),name="Median (hrs)")+
#   #scale_fill_gradientn(colors = rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')),
#   #                     name="Median (hrs)")+
#   #scale_color_gradientn(colors = c('#225ea8','#41b6c4','#c7e9b4','#ffffd9'),name="Median (hrs)")+
#   #scale_size(range = c(0.5, 2), name="IQR (hrs)")+
#   #scale_size_binned(breaks=c(0,1,6,12,18,24),range = c(0.25, 3),name="IQR (hrs)")+
#   facet_wrap(.~moLab, ncol=3)+
#   #ggtitle("Median and IQR Burn Period (RH<20%)")+
#   theme_bw()+
#   #theme_classic()+
#   #theme_minimal()+
#   theme(
#     axis.line = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom")
# p
# 
# ggsave(filename="./figures/Fig2_moBPstats.png", width=8.5, height=8.5, dpi = 600, device='png', p)
#####

#####
##### plot time series of station data -----
# which(unique(burnListDF$STA_NAME)=="FLAGSTAFF")
# temp<-subset(anoms, STA_NAME=="FLAGSTAFF")
# temp<-subset(temp, date>="2010-06-01" & date<="2010-07-10")
# temp<-temp[,c("date","bhrs20_med_anom","anomT","anomDP","meanT","meanDP","bhrs20")]
# temp<-tidyr::gather(temp, var, value, 2:4)
# 
# ggplot(temp, aes(date,value, color=var))+
#   geom_line()+
#   geom_hline(yintercept = 0)+
#   ggtitle("FLAGSTAFF RAWS - Schulz Fire 6/20-6/30")+
#   geom_rect(aes(xmin = as.Date("2010-06-21"),
#                 xmax = as.Date("2010-07-04"),
#                 ymin = -13,
#                 ymax = -12),
#             fill = "goldenrod1", color = "firebrick", size = 0.5)+
#   theme_bw()+
#   theme(legend.position='bottom')
#####

# telegraph fire - anomalies
which(unique(burnListDF$STA_NAME)=="GLOBE")
temp<-subset(anoms, STA_NAME=="GLOBE")
temp<-subset(temp, date>="2021-06-01" & date<="2021-07-05")
  temp1<-temp
temp<-temp[,c("date","bhrs20_med_anom","anomT","anomDP","meanT","meanDP","bhrs20")]

# get temp and dp variability
#sd(subset(temp, date >= as.Date("2021-06-04") & date <= as.Date("2021-07-02"))$meanT)
#sd(subset(temp, date >= as.Date("2021-06-04") & date <= as.Date("2021-07-02"))$meanDP)

# gather into ggplot df
temp<-tidyr::gather(temp, var, value, 2:4)

p1<-ggplot(temp, aes(date,value, fill=var))+
  #geom_line()+
  geom_rect(aes(xmin = as.Date("2021-06-04"),
                xmax = as.Date("2021-07-02"),
                ymin = -Inf,
                ymax = Inf),
            fill = "beige", color = "grey", size = 0.5,alpha=0.05)+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept = 0)+
  #ggtitle("Telegraph Wildfire")+
  # geom_rect(aes(xmin = as.Date("2021-06-04"),
  #               xmax = as.Date("2021-06-25"),
  #               ymin = -17,
  #               ymax = -16),
  #           fill = "goldenrod1", color = "firebrick", size = 0.5)+
  scale_fill_manual(values=c("anomDP" = "#1b9e77", "anomT" = "#d95f02", "bhrs20_med_anom" = "#7570b3"),
                    labels=c("Dewpoint (°C)  ","Temperature (°C)  ", "Burn Period (hrs)  "),
                    name="")+
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day",
               date_labels = "%B-%d")+
  ylab("anomaly (°C and hours)")+
  theme_bw()+
  theme(legend.position='bottom')
p1
ggsave(filename="./figures/Fig1supp_TelegraphRAWSAnoms.png", width=7.5, height=5, dpi = 600, device='png', p1)

# telegraph fire - raw values
which(unique(burnListDF$STA_NAME)=="GLOBE")
temp<-subset(anoms, STA_NAME=="GLOBE")
temp<-subset(temp, date>="2021-06-01" & date<="2021-07-05")
temp<-temp[,c("date","bhrs20_med_anom","anomT","anomDP","meanT","meanDP","bhrs20")]


temp<-tidyr::gather(temp, var, value, 5:7)
temp$var<-factor(temp$var, levels=c("meanDP","meanT","bhrs20"))

p2<-ggplot(temp, aes(date,value, fill=var))+
  #geom_line()+
  geom_rect(aes(xmin = as.Date("2021-06-04"),
                xmax = as.Date("2021-07-02"),
                ymin = -Inf,
                ymax = Inf),
            fill = "beige", color = "grey", size = 0.5,alpha=0.05)+
  geom_bar(stat="identity", position="dodge")+
  geom_hline(yintercept = 0)+
  #geom_hline(yintercept = 24, color="#7570b3")+
  #ggtitle("Telegraph Wildfire")+
  # geom_rect(aes(xmin = as.Date("2021-06-04"),
  #               xmax = as.Date("2021-06-25"),
  #               ymin = -17,
  #               ymax = -15),
  #           fill = "goldenrod1", color = "firebrick", size = 0.5)+
  scale_fill_manual(values=c("meanDP" = "#1b9e77", "meanT" = "#d95f02", "bhrs20" = "#7570b3"),
                    labels=c("Dewpoint (°C)  ","Temperature (°C)  ", "Burn Period (hrs)  "),
                    name="")+
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day",
               date_labels = "%B-%d")+
  ylab("°C and hours")+
  theme_bw()+
  theme(legend.position='bottom')
p2
#ggsave(filename="./figures/Fig6_TelegraphRAWS.png", width=7.5, height=5, dpi = 600, device='png', p2)

#cowplot::plot_grid(p1,p2,labels=c("a.","b."), ncol=1,align="v")

# Telegraph SIT 209 data

Telegraph209 <- readr::read_csv("data/SIT209/Telegraph_2021_SIT209_simple.csv",col_types = readr::cols(date = readr::col_date(format = "%m/%d/%Y")))

# prog data
TeleProg<-Telegraph209[,c(1,4,5)]
  TeleProg$date[1]<-as.Date("2021-06-03")
  TeleProg$CURR_INCIDENT_AREA[1]<-0
TeleProg<-merge(temp1[,c("date","doy")],TeleProg, by="date", all.x=TRUE)
TeleProg<-TeleProg[!duplicated(TeleProg$date), ]
  
pProg<-ggplot(TeleProg, aes(date,CURR_INCIDENT_AREA))+
  geom_bar(stat = "identity", position="dodge", fill="grey79", color="black")+
  #geom_point()+
  #geom_line()+
  ylab("fire size (acres)")+
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day",
               date_labels = "%B-%d")+ # limits = as.Date(c('2021-06-01','2021-07-01'))
  theme_bw()
  
  
p3<-cowplot::plot_grid(p2,pProg,labels=c("(a)","(b)"), ncol=1,align="v", rel_heights = c(2, 1))
ggsave(filename="./figures/Fig6_TelegraphRAWS_wSize.png", width=7.5, height=7, dpi = 600, device='png', p3)


##### annual/monthly time series

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

p<-ggplot(temp, aes(year,meanBhrs20, fill=as.factor(month), group=as.factor(month)))+
  geom_bar(stat="identity", position = "dodge", color="black")+
  scale_fill_manual(values=c("6" = "#ff7f00", "7" = "#377eb8"),
                    labels=c("June","July"),
                    name="")+
  #geom_line()+
  #geom_point()+
  ylab("hours")+
  geom_hline(yintercept = mean(subset(temp, month==6)$meanBhrs20), color="#ff7f00")+
  geom_hline(yintercept = mean(subset(temp, month==7)$meanBhrs20), color="#377eb8")+
  #ggtitle("Monthly Average Burn Period (RH<20%)")+
  theme_bw()+
  theme(legend.position='bottom')

cor(subset(temp, month==6)$meanBhrs20,subset(temp, month==7)$meanBhrs20)
cor(subset(temp, month==6)$meanBhrs20,subset(temp, month==6)$meanMeanDP)
cor(subset(temp, month==7)$meanBhrs20,subset(temp, month==7)$meanMeanDP)

temp1<-cbind.data.frame(subset(temp[,c("year","month","meanBhrs20")], month==6),
subset(temp[,c("year","month","meanBhrs20")], month==7))
temp1<-temp1[,c(1,2,3,6)]
temp1$diff<-temp1$meanBhrs20-temp1$meanBhrs20.1

temp1$June<-scale(temp1$meanBhrs20, center = TRUE, scale=FALSE)
temp1$July<-scale(temp1$meanBhrs20.1, center = TRUE, scale=FALSE)


ggsave(filename="./figures/Fig7_monthlyBhrs.png", width=7, height=5, dpi = 600, device='png', p)

