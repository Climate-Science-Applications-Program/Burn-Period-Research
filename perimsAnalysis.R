# read/analyze fire perimeters, https://data-nifc.opendata.arcgis.com/search?tags=Category%2Chistoric_wildlandfire_opendata
# MAC 04/02/2023

library(sf)
library(raster)

perimeters<-st_read("./data/perimeters/US_HIST_FIRE_PERIMTRS_2000_2018_DD83.shp")
#perimeters<-st_read("./data/perimeters/InterAgencyFirePerimeterHistory_All_Years_View.shp")
#st_geometry_type(noaa20)
st_crs(perimeters)
length(unique(modis$ACQ_DATE))
modiscrop<-st_intersection(modis, st_set_crs(st_as_sf(as(raster::extent(-115, -102, 31, 37), "SpatialPolygons")), st_crs(modis)))  
length(unique(modiscrop$ACQ_DATE)); min(unique(modiscrop$ACQ_DATE)); max(unique(modiscrop$ACQ_DATE))
rm(modis)