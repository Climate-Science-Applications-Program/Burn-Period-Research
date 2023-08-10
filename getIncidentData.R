# pull and process incident data from GACC site
# MAC 06/02/23


library(httr)
library(XML)
library(raster)

fireDate<-"20210605"

years <- readHTMLTable(content(GET("https://ftp.wildfire.gov/public/incident_specific_data/southwest/GACC_Incidents/"), "text"))[[1]]
  years <- na.omit(years)
  years <- years[-which(years$Name=="Parent Directory"),]
  
fires <- readHTMLTable(content(GET(paste0("https://ftp.wildfire.gov/public/incident_specific_data/southwest/GACC_Incidents/",years$Name[10])), "text"))[[1]] 
  fires <- na.omit(fires)
  fires <- fires[-which(fires$Name=="Parent Directory"),]
  
  which(stringr::str_detect(fires$Name, 'Telegraph'))

gis <- readHTMLTable(content(GET(paste0("https://ftp.wildfire.gov/public/incident_specific_data/southwest/GACC_Incidents/",years$Name[10],
                                        fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))],"GIS/Data")), "text"))[[1]] 
  gis <- na.omit(gis)
  gis <- gis[-which(gis$Name=="Parent Directory"),]
  
eventData <- readHTMLTable(content(GET(paste0("https://ftp.wildfire.gov/public/incident_specific_data/southwest/GACC_Incidents/",years$Name[10],
                                          fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))],"GIS/Data/",
                                          gis$Name[which(stringr::str_detect(gis$Name, fireDate))])), "text"))[[1]] 
  eventData <- na.omit(eventData)
  eventData <- eventData[-which(eventData$Name=="Parent Directory"),]

# create data dir and download
dir.create(paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))],eventData$Name)[1])
  
download.file(paste0("https://ftp.wildfire.gov/public/incident_specific_data/southwest/GACC_Incidents/",years$Name[10],
                     fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))],"GIS/Data/",
                     gis$Name[which(stringr::str_detect(gis$Name, fireDate))],eventData$Name),
                     paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))],eventData$Name),
                     mode = "wb")

dirs<-list.dirs(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]), full.names = TRUE, recursive = TRUE)

# list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
#            pattern = "*.zip", full.names = TRUE)

unzip(list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
                 pattern = "*.zip", full.names = TRUE),
                 exdir = dirs, overwrite = TRUE)
unlink(list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
                  pattern = "*.zip", full.names = TRUE))


list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
           pattern = "*.gdb", full.names = TRUE)

rgdal::ogrListLayers(list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
                         pattern = "*.gdb", full.names = TRUE))
firePerim2<-rgdal::readOGR(list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
                               pattern = "*.gdb", full.names = TRUE), layer="IR_Polygon")

test<-subset(firePerim, IncidentName=="Telegraph")

rgdal::ogrListLayers(list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
                                pattern = "*.gdb", full.names = TRUE)[1]  )

firePerim<-rgdal::readOGR(list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
                                     pattern = "*.gdb", full.names = TRUE)[1], layer="IR_Polygon")
firePerim2<-rgdal::readOGR(list.files(path = paste0("./data/SW_GACC/",fires$Name[which(stringr::str_detect(fires$Name, 'Telegraph'))]),
                                     pattern = "*.gdb", full.names = TRUE)[2], layer="IR_Polygon")


