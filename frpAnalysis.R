# load NASA FIRMS data
# MAC 04/01/23

# https://firms.modaps.eosdis.nasa.gov/download/
# column names https://firms.modaps.eosdis.nasa.gov/descriptions/FIRMS_MODIS_Firehotspots.html

# questions:
# what should the resolution be?
# what is the distribution of FRP values? how does resolution impact magnitude of FRP?
# does 12am to 12am burn period work for timing of FRP values? day vs night values...
# look at distribution of timing of FRP values (are higher ones always during day?) stratify raster into day vs night values/day
# 


library(sf)
library(raster)

# VIIRS NOAA-20 375m: Temporal Coverage: 1 January 2020 - present
noaa20<-st_read("./data/frp/fire_nrt_J1V-C2_332874.shp")
 #st_geometry_type(noaa20)
  st_crs(noaa20)
noaa20crop<-st_intersection(noaa20, st_set_crs(st_as_sf(as(raster::extent(-115, -102, 31, 37), "SpatialPolygons")), st_crs(noaa20)))
rm(noaa20)
noaa20crop <- noaa20crop %>% st_drop_geometry()
length(unique(noaa20crop$ACQ_DATE)); min(unique(noaa20crop$ACQ_DATE)); max(unique(noaa20crop$ACQ_DATE))
  save(noaa20crop, file="./data/J1V-C2_FRP_MODIS_SW.RData")

# VIIRS S-NPP 375m: Temporal Coverage: 20 January 2012 - present
snpp<-st_read("./data/frp/fire_archive_SV-C2_332875.shp")
#st_geometry_type(noaa20)
st_crs(snpp)
length(unique(snpp$ACQ_DATE))
snppcrop<-st_intersection(snpp, st_set_crs(st_as_sf(as(raster::extent(-115, -102, 31, 37), "SpatialPolygons")), st_crs(snpp)))
rm(snpp)
snppcrop <- snppcrop %>% st_drop_geometry()
length(unique(snppcrop$ACQ_DATE)); min(unique(snppcrop$ACQ_DATE)); max(unique(snppcrop$ACQ_DATE))
  save(snppcrop, file="./data/SV-C2_FRP_MODIS_SW.RData")


# MODIS Collection 6.1: Temporal Coverage: 11 November 2000 - present  
modis<-st_read("./data/frp/fire_archive_M-C61_332873.shp")
#st_geometry_type(noaa20)
st_crs(modis)
length(unique(modis$ACQ_DATE))
modiscrop<-st_intersection(modis, st_set_crs(st_as_sf(as(raster::extent(-115, -102, 31, 37), "SpatialPolygons")), st_crs(modis)))  
length(unique(modiscrop$ACQ_DATE)); min(unique(modiscrop$ACQ_DATE)); max(unique(modiscrop$ACQ_DATE))
rm(modis)
modiscrop <- modiscrop %>% st_drop_geometry()

save(modiscrop, file="./data/M-C61_FRP_MODIS_SW.RData")

quantile(modiscrop$FRP, probs=c(0.5,0.75,0.9))

# convert to raster
# see https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation
xyzDF<-st_drop_geometry(modiscrop)[,c("LONGITUDE","LATITUDE","FRP")]
  colnames(xyzDF)<-c('x', 'y', 'z')
  e<-extent(xyzDF[,1:2])
  r<-raster(e,ncol=1000,nrow=1000)
  r <- rasterize(xyzDF[, 1:2], r, xyzDF[,3], fun=mean)
  plot(r)
  