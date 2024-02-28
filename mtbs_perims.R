# read and process mtbs data from https://www.mtbs.gov/direct-download
# MAC 07/16/23

library(sf)
library(raster)

sf_use_s2(FALSE)

# mtbs perims, updated dataset loaded on 11/29/23
perims<-st_read("~/RProjects/BurnPeriodResearch/data/mtbs/us_perim/mtbs_perims_DD.shp")
#st_geometry_type(noaa20)
st_crs(perims)
perimsCrop<-st_intersection(perims, st_set_crs(st_as_sf(as(raster::extent(-115, -102, 31, 37.35), "SpatialPolygons")), st_crs(perims)))
#noaa20crop <- noaa20crop %>% st_drop_geometry()
perimsCrop<-as(perimsCrop, 'Spatial')

# check counts of subsets
#test<-subset(perimsCrop, Ig_Date>="2000-01-01")
#length(which(test$BurnBndAc>=5000))

# save data file
save(perimsCrop, file="~/RProjects/BurnPeriodResearch/data/AZNM_mtbs_perims_1984_2022.RData")


# add in provisional data accessed 7/17/23
# https://burnseverity.cr.usgs.gov/products/provisionalIA/data

# build file list 
dirsName<-c("azProv","nmProv")
provList<-list()
k=1
for(j in 1:length(dirsName)){
  dirs<-list.dirs(path = paste0("./data/mtbs/",dirsName[j]), full.names = TRUE, recursive = TRUE)
  fileNames<-cbind.data.frame(dirname(list.files(path = dirs, pattern = "*bndy.shp", full.names = TRUE)),
                              basename(list.files(path = dirs, pattern = "*bndy.shp", full.names = TRUE)))
  colnames(fileNames)<-c("dirName","baseName")
  fileNames[] <- lapply(fileNames, as.character)
  
  for(i in 1:nrow(fileNames)){
    # load in shapefile
    # provList[[k]] <- rgdal::readOGR(dsn = fileNames$dirName[i], layer = tools::file_path_sans_ext(fileNames$baseName[i])) 
    temp <- rgdal::readOGR(dsn = fileNames$dirName[i], layer = tools::file_path_sans_ext(fileNames$baseName[i])) 
    provList[[k]] <- spTransform(temp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    k=k+1
    print(k)  
  }
}
# combine provisional data
provList<-do.call(bind, provList)

# check var types
compareColumns <- function(df1, df2) {
  commonNames <- names(df1)[names(df1) %in% names(df2)]
  data.frame(Column = commonNames,
             df1 = sapply(df1[,commonNames], class),
             df2 = sapply(df2[,commonNames], class)) }
compareColumns(provList@data,perimsCrop@data)
# fix var types in provList
provList$Map_ID<-as.numeric(as.character(provList$Map_ID))
provList$Ig_Date<-as.Date(provList$Ig_Date,"%Y/%m/%d")
provList$BurnBndAc<-as.numeric(as.character(provList$BurnBndAc))

# add to MTBS dataset
perimsCrop<-bind(perimsCrop,provList)

# save data file
save(perimsCrop, file="./data/AZNM_mtbs_perims_1984_2022_wProv.RData")

