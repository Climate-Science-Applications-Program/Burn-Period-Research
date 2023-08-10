# process USGS wildfire perims
# https://doi.org/10.5066/P9Z2VVRT
# MAC 07/17/23

library(raster)

perim <- rgdal::readOGR(dsn = "./data/USGSperims/Shapefile", layer = "US_Wildfires_1878_2019") 
perim<-spTransform(perim, CRS("+proj=longlat +datum=WGS84"))

perimsCrop<-crop(perim,extent(-115, -102, 31, 37))

# save data file
save(perimsCrop, file="./data/AZNM_USGS_perims_1878_2019.RData")


# alternate sf version
# library(sf)
# sf_use_s2(FALSE)
# # mtbs perims
# perims<-st_read("./data/USGSperims/Shapefile/US_Wildfires_1878_2019.shp")
# #st_geometry_type(noaa20)
# # reproject data to WGS lat/lon
# st_crs(perims)
# perims<-st_transform(perims, 4326)
# # crop to AZ/NM
# perimsCrop<-st_intersection(perims, st_set_crs(st_as_sf(as(raster::extent(-115, -102, 31, 37), "SpatialPolygons")), st_crs(perims)))
# #noaa20crop <- noaa20crop %>% st_drop_geometry()
# perimsCrop<-as(perimsCrop, 'Spatial')
# perims<-as(perims, 'Spatial')




