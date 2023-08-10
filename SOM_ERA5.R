# SOM weather typing using ERA5 Reanalysis
# adapted from SOM_NARR.R
# MAC 04/24/23

library(raster)
library(kohonen)
library(reshape2)
library(tidyr)
library(dplyr)

ptm <- proc.time()

# set rasteroptions
rasterOptions(progress = 'text')

# functions
leap_every_year <- function(x) {
  ifelse(lubridate::yday(x) > 59 & lubridate::leap_year(x) == FALSE, lubridate::yday(x) + 1, lubridate::yday(x))
}

# map layers
states <- getData('GADM', country='United States', level=1)
us <- getData('GADM', country='United States', level=0)
mx <- getData('GADM', country='Mexico', level=0)

# # load pre-processed raster stacks from getECMWFdata.R
gh500<-stack("./data/ecmwf/dailyAvg_reanalysis-era5_500mb_2000_2022_SWUS_Mar-Jul.grd")

# dates - find and remove leap days
dates<-as.data.frame(as.Date(names(gh500),format="X%Y.%m.%d"))
  colnames(dates)<-"date"
  dates$month<-as.numeric(format(dates$date, "%m"))
  dates$day<-as.numeric(format(dates$date, "%d"))
  dates$year<-as.numeric(format(dates$date, "%Y"))
  dates$doy<-as.numeric(format(dates$date, "%j"))
  dates$doy_ly<-leap_every_year(dates$date) # this day of year without leap day shifts

# subset layers to months of interest
#mos<-c(6,7,8,9)
mos<-c(4,5,6)
subDates<-dates[which(dates$month %in% mos),]
subLayers<-gh500[[which(dates$month %in% mos)]]

# convert layers to dataframe
layers.df<-(as.data.frame(subLayers, long=TRUE, xy=TRUE))
colnames(layers.df)<-c("lon","lat","date","value")  
# long to wide
df.wide<-reshape2::dcast(layers.df, formula = date~lat+lon, value.var = "value")

# kohonen SOM
nrows=4
ncols=6
som.gh500 <- som(as.matrix(df.wide[,2:ncol(df.wide)]), grid = somgrid(ncols, nrows, "rectangular"))
codebook<-as.data.frame(som.gh500$codes)
code_grid<-as.data.frame(som.gh500$grid$pts)
code_grid$mapUnit<-seq(1,nrow(code_grid))
code_grid<-code_grid %>%
  unite(y,x, col="codes", sep="_") # add mapunit back in if needed

# deal with code book
codebook<-cbind(code_grid,codebook)
codebook.long<-melt(codebook, id.vars = 1)
# separate out mapunits  ----
mapunits<-codebook.long[1:(nrows*ncols),]
codebook.long<-codebook.long[-(1:(nrows*ncols)),]
# ####
codebook.long<-separate(codebook.long, variable, convert = TRUE, into = c("lat", "lon"), sep="_.")
codebook.long$lat<-as.numeric(gsub("X", "", codebook.long$lat))
codebook.long$lon<--codebook.long$lon
codebook.long<-separate(codebook.long, codes, convert = FALSE, remove = FALSE, into = c("xCols", "yRows"), sep="_")

# assign days to nodes
nodes<-map(som.gh500)
somTime<-as.data.frame(cbind(df.wide$date, nodes$unit.classif, nodes$distances))
colnames(somTime)<-c("date","mapUnit","errorDist")
somTime$date<-as.character(somTime$date)
somTime$date<-as.Date(somTime$date, format="X%Y.%m.%d")
somTime$month<-as.numeric(format(somTime$date, format="%m"))
somTime$year <-as.numeric(format(somTime$date, format="%Y"))
somTime$day  <-as.numeric(format(somTime$date, format="%d"))
#somTime<-separate(somTime, as.character(date), convert = TRUE, remove = FALSE, into = c("year","month","day"), sep=".")
#somTime$date<-as.Date(paste(somTime$year,"-",somTime$day,"-",somTime$month,sep=""), format="%Y-%d-%m")
somTime$doy<-as.numeric(format(somTime$date, "%j"))
somTime$mapUnit<-as.integer(somTime$mapUnit)
somTime$errorDist<-as.numeric(as.character(somTime$errorDist))

# join nodes/dates to target summary layer
nodeDates<-somTime[,c("date","mapUnit")]
layers.df<-merge(layers.df, nodeDates, by="date")
targetGrid <- layers.df %>% group_by(lon, lat, mapUnit) %>% summarise(medPrec=median(value, na.rm=TRUE))
targetGrid <- merge(targetGrid, mapunits, by.x="mapUnit", by.y="value")

# RASTERVIS mapping of SOM results
library(rasterVis)
# create stack of codebook results
codeList<-unique(codebook.long$codes)
cbRaster<-stack()
for (i in 1:length(codeList)) {
  temp<-codebook.long[which(codebook.long$codes==codeList[i]),c(5,4,6)]
  temp<-rasterFromXYZ(temp)
  cbRaster<-stack(cbRaster, temp)
}
names(cbRaster)<-codeList

# https://gis.stackexchange.com/questions/121306/how-to-reproduce-contour-style-labeling-with-rasterviscontourplot
#pptRaster[pptRaster == 0] <- NA
#at=c(seq(0.01,8,1),10)
levels<-seq(5400,6000, 25)
p<- levelplot(cbRaster, contour=FALSE, margin=FALSE, par.settings=viridisTheme, at=levels,layout=c(ncols,nrows),
              main="GH500 JJAS 5x7 SOM - NARR 1979-2019")+
  # contourplot(cbRaster,linetype = "dashed", pretty=TRUE, at=levels,
  #             labels = list(cex = 0.4),
  #             label.style = 'align')+
  layer(sp.polygons(us, col = 'gray40', lwd=1))+
  layer(sp.polygons(mx, col = 'gray40', lwd=1))
png("./ERA5_GH500_SOM.png", width = 10, height = 6, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off() 


