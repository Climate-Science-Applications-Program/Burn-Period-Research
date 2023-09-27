# Burn period case study fires with SIT-209 data -- LAS CONCHAS FIRE
# analyze fire progression with RAWS/burn period data
# using mtbs perims, adapted from eventAnalysis.R
# adapted from eventAnalysis_mtbsPerims.R, AZNM_largeFires_analysis.R
# MAC 9/7/23

library(raster)
library(geosphere)
library(ggplot2)
library(dplyr)
library(cowplot)

library(readxl)
SITdf <- read_excel("data/SIT209/LasConchas_2011_SIT209.xlsx", 
                                            col_types = c("date", "text", "text", 
                                                                   "text", "date", "text", "text", "text", 
                                                                    "text", "text", "numeric", "numeric", 
                                                                    "text", "numeric", "numeric", "text", 
                                                                   "numeric", "text", "numeric", "text", 
                                                                    "numeric", "text", "numeric", "numeric", 
                                                                    "text", "text", "text", "text", "text", 
                                                                    "numeric", "text", "text", "text", 
                                                                    "text", "numeric", "text", "text", 
                                                                    "text", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text", "text", "text", 
                                                                    "text", "text", "text", "text", "text", 
                                                                    "text", "text", "text", "numeric", 
                                                                    "text", "text", "text", "text", "text", 
                                                                    "text", "text", "text", "numeric", 
                                                                    "numeric", "text", "numeric", "text", 
                                                                    "text", "text", "text", "text", "numeric", 
                                                                    "numeric", "text", "text", "numeric", 
                                                                    "numeric", "text", "text", "text", 
                                                                    "text", "text", "text", "text", "text"))

# thin out duplicate, extraneous days
SITdf <- SITdf[2:(nrow(SITdf)-1),]

SITdf$REPORT_DATE <- as.Date(SITdf$REPORT_DATE)
SITstart <- min(SITdf$REPORT_DATE)
SITend <- max(SITdf$REPORT_DATE)


# load fire progression data from fireProgression_loop_mtbs.R
# load("./data/fireProgression_Stats_mtbs_perims_gt50K.RData")
load("./data/fireProgression_Stats_mtbs_perims_gt10K.RData")

# scan for empty fires, thin out list
temp <- c()
names <- c()
for (i in 1:length(fireProgList)) {
  temp[i] <- fireProgList[[i]][[1]]$DateAZ[1]
  names[i] <- fireProgList[[i]][[1]]$FireName[1]
}
# fireProgList<-fireProgList[which(!is.na(temp))]

# subset to AZ/NM fires
caseFire <- c("LAS CONCHAS")

fireProgList <- fireProgList[which(names %in% caseFire)]

# load burnperiod climo dataset from burnPeriodClimo.R
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# get station list from burnList
stations <- do.call(rbind, lapply(burnList, function(x) x[1, c("STA_NAME", "LATITUDE", "LONGITUDE")]))

# find closest RAWS to fire and attached burn period hours
# temporary RAWS station list for distances
geoStns <- stations[, c("LONGITUDE", "LATITUDE")]

# find closest RAWS
fireCenter <- rgeos::gCentroid(fireProgList[[1]][[3]])
fireCenter <- c(fireCenter@coords[1], fireCenter@coords[2])
distances <- geosphere::distGeo(fireCenter, geoStns) / 1000
ranking <- rank(distances, ties.method = "first")

# get data from closest RAWS dataframe
tempDF <- burnList[[which.min(ranking)]]
tempDF <- subset(tempDF, date >= as.Date(SITstart) & date <= as.Date(SITend))
tempDF$RAWSdist <- distances[which.min(ranking)]
tempEvent <- fireProgList[[1]][[1]]
tempDF <- merge(tempEvent, tempDF, by.x = "DateAZ", by.y = "date", all = TRUE)

# merge with SITdf
SITdf <- merge(SITdf, tempDF, by.x = "REPORT_DATE", by.y = "DateAZ", all = TRUE)
# thin out df
SITthin <- SITdf[, c(
  "REPORT_DATE", "INCIDENT_NAME", "IMT_TYPE", "AREA", "P_CONTAIN", "FUELS", "OBS_FIRE_BEHAVE", "SIG_EVENT",
  "GROWTH_POTENTIAL", "TARGETS_MET", "maxFRP", "nightMaxFRP", "acres", "cumSum", "FireName",
  "doy", "year", "month", "day", "bhrs20", "meanRH", "minRH", "maxVPD", "minDP", "meanDP", "maxFFWI",
  "maxHDW", "STA_NAME", "bhrs20_med_anom", "bhrs20_avg_anom", "RAWSdist"
)]
SITthin$AREA[is.na(SITthin$AREA)] <- 0

# calculate daily wildfire acres burned
SITthin <- SITthin %>%
  group_by(INCIDENT_NAME) %>%
  arrange(REPORT_DATE, .by_group = TRUE) %>%
  mutate(dayAcres = AREA - lag(AREA),
         dayContain= P_CONTAIN - lag(P_CONTAIN))
# swap NA with first day Area
SITthin$dayAcres <- ifelse(is.na(SITthin$dayAcres), SITthin$AREA, SITthin$dayAcres)
SITthin$dayContain <- ifelse(is.na(SITthin$dayContain), 0, SITthin$dayContain)
# set dayAcres<0 to 0
SITthin$dayAcres[SITthin$dayAcres < 0] <- 0
SITthin$dayContain[SITthin$dayContain < 0] <- 0

temp<-subset(SITthin,INCIDENT_NAME == "Las Conchas")
temp<-temp[,c("REPORT_DATE","AREA","dayAcres","acres")]

##### hourly RAWS data ---- 
# get hourly RAWS data from station  

file_list <- list.files(path="/home/crimmins/RProjects/BurnPeriodTracker/data/raws", full.names = TRUE, recursive = TRUE)
file_names<- list.files(path="/home/crimmins/RProjects/BurnPeriodTracker/data/raws", full.names = FALSE, recursive = TRUE, include.dirs = FALSE)

# read in RAWS file
tempRAWS<-read.csv(file_list[grep(tempDF$StationNum[2], file_names, value = FALSE)])
tempRAWS$DateTime<-as.POSIXct(tempRAWS$DateTime, format = "%Y-%m-%dT%H:%M:%S")
tempRAWS$year<-as.numeric(format(tempRAWS$DateTime,"%Y"))
tempRAWS$month<-as.numeric(format(tempRAWS$DateTime,"%m"))
tempRAWS$day<-as.numeric(format(tempRAWS$DateTime,"%d"))
tempRAWS$hour<-as.numeric(format(tempRAWS$DateTime,"%H"))

# calc fire weather indices
library(firebehavioR)
library(plantecophys)
fireWXidx = fireIndex(temp=((tempRAWS$Temperature.F.-32)*(5/9)), u= (tempRAWS$WindSpeed.mph.*1.60934), rh = tempRAWS$RelativeHumidity...)
# combine with temp df
tempRAWS$ffwi<-fireWXidx$fosberg
tempRAWS$hdw<-fireWXidx$hotDryWindy

elev<-elevatr::get_elev_point(data.frame(x=tempDF$LONGITUDE[2],y=tempDF$LATITUDE[2]),prj = "EPSG:4326", src = "aws")
# calc air pressure based on elevation
kPa<-(101325*((1-(2.25577*10^-5)*elev@data$elevation)^5.25588))/1000
# calc vpd
tempRAWS$vpd_kPa<-RHtoVPD(tempRAWS$RelativeHumidity..., ((tempRAWS$Temperature.F.-32)*(5/9)), Pa = kPa)
# calculate dewpoint
tempRAWS$dp_F<-weathermetrics::humidity.to.dewpoint(tempRAWS$RelativeHumidity...,tempRAWS$Temperature.F.,
                                                    temperature.metric = "fahrenheit") 

tempRAWS<- subset(tempRAWS, tempRAWS$DateTime>=as.POSIXct(min( SITthin$REPORT_DATE))+7*60*60 &
                    tempRAWS$DateTime<=as.POSIXct(max(SITthin$REPORT_DATE))+7*60*60)

##### plot event data ----
temp <- subset(SITthin, INCIDENT_NAME == "Las Conchas")


# ggplot(subset(SITthin, INCIDENT_NAME == "Cave Creek Complex"), aes(REPORT_DATE, AREA, color = INCIDENT_NAME)) +
#   geom_line() +
#   geom_line(aes(REPORT_DATE, dayAcres), color = "orange")

p1 <- ggplot(temp, aes(REPORT_DATE, dayAcres)) +
  geom_bar(stat = "identity")+
  scale_x_date(
    date_breaks = "2 day",
    labels = scales::date_format("%m-%d"),
    limits = as.Date(c(SITstart, SITend))
  )

p2 <- ggplot(temp, aes(REPORT_DATE, acres)) +
  geom_bar(stat = "identity") +
  scale_x_date(
    date_breaks = "2 day",
    labels = scales::date_format("%m-%d"),
    limits = as.Date(c(SITstart, SITend))
  )

p3 <- ggplot(temp, aes(REPORT_DATE, bhrs20)) +
  geom_bar(stat = "identity")+
  scale_x_date(
    date_breaks = "2 day",
    labels = scales::date_format("%m-%d"),
    limits = as.Date(c(SITstart, SITend))
  )

plot_grid(p1, p2, p3, ncol = 1, align = "hv", axis = "b")


p1<-ggplot(subset(SITthin, INCIDENT_NAME == "Las Conchas"), aes(REPORT_DATE, AREA)) +
  geom_line()+
  geom_text(aes(REPORT_DATE, 50000, label=P_CONTAIN))
p2 <- ggplot(temp, aes(REPORT_DATE, dayAcres)) +
  geom_line()
#geom_bar(stat = "identity")
p3<-ggplot(temp, aes(REPORT_DATE, bhrs20)) +
  geom_line()
p4<-ggplot(temp, aes(REPORT_DATE, bhrs20_med_anom)) +
  geom_line(color='red')+
  geom_hline(yintercept = 0)
p5<-ggplot(temp, aes(REPORT_DATE, minRH)) +
  geom_line()
p6<-ggplot(temp, aes(REPORT_DATE, meanDP)) +
  geom_line()
p7<-ggplot(temp, aes(REPORT_DATE, maxHDW)) +
  geom_line()
p8<-ggplot(temp, aes(REPORT_DATE, maxFRP)) +
  geom_line()
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, ncol = 1, align = "hv", axis = "b")


p1<-ggplot(subset(SITthin, INCIDENT_NAME == "Las Conchas"), aes(REPORT_DATE, AREA)) +
  geom_line()+
  ggtitle("Las Conchas")
p2 <- ggplot(temp, aes(REPORT_DATE, dayAcres)) +
  geom_line()
p3 <- ggplot(temp, aes(REPORT_DATE, P_CONTAIN)) +
  geom_line()
#geom_bar(stat = "identity")
p4<-ggplot(temp, aes(REPORT_DATE, bhrs20)) +
  geom_line()
p5<-ggplot(temp, aes(REPORT_DATE, bhrs20_med_anom)) +
  geom_line(color='red')+
  geom_hline(yintercept = 0)
p6<-ggplot(temp, aes(REPORT_DATE, meanDP)) +
  geom_line()
plot_grid(p1,p2,p3,p4,p5,p6, ncol = 1, align = "hv", axis = "b")

# plot RAWS data
p1<-ggplot(tempRAWS)+
  geom_line(aes(DateTime, `Temperature.F.`))+
  geom_line(aes(DateTime, dp_F), color='forestgreen')+
  geom_line(aes(DateTime, `RelativeHumidity...`), color='blue')+
  geom_hline(yintercept = 20)

p2<-ggplot(temp, aes(REPORT_DATE, bhrs20)) +
  geom_line()
p3<-ggplot(temp, aes(REPORT_DATE, bhrs20_med_anom)) +
  geom_line(color='red')

p4 <- ggplot(temp, aes(REPORT_DATE, dayAcres)) +
  geom_line()
p5 <- ggplot(temp, aes(REPORT_DATE, dayContain)) +
  geom_line()
p6<-ggplot(tempRAWS)+
  geom_line(aes(DateTime, vpd_kPa))

plot_grid(p1,p2,p3,p4,p5,p6, ncol = 1, align = "hv", axis = "b")

ggplot(temp, aes(dayContain,bhrs20))+
  geom_point()

# create a table
library(knitr)
library(kableExtra)
kable(SITthin[,c("REPORT_DATE","P_CONTAIN","AREA","OBS_FIRE_BEHAVE","GROWTH_POTENTIAL","TARGETS_MET","bhrs20","bhrs20_med_anom")]) %>%
  #kable_styling(latex_options = "striped") %>%
  kable_styling(full_width = T, html_font = "Cambria", font_size = 12) %>%
  save_kable("Las_Conchas_summary.png")


