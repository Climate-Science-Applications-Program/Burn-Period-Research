# calculate fire progression from active fire pixels MODIS/VIIRS
# use data from mtbs perims
# adapted from fireProgression_loop.R
# MAC 07/18/23

library(raster)
library(dplyr)
library(gstat)

# functions
RMSE <- function(residuals){
  sqrt(sum((residuals)^2)/length(residuals))
}

# turn off dplyr warnings
options(dplyr.summarise.inform = FALSE)

#####
# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")
# work with  FOD dataframe only
fc<-fc@data
# remove factors
fc[,] <- lapply(fc, function(x) type.convert(as.character(x), as.is = TRUE))
# adjust dates on fc DF  
fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,"%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,"%m/%d/%Y")
#####

##### load MODIS FRP data -- produced by frpAnalysis.R
load("~/RProjects/BurnPeriodResearch/data/M-C61_FRP_MODIS_SW.RData")
load("~/RProjects/BurnPeriodResearch/data/SV-C2_FRP_MODIS_SW.RData")
#####

# combine VIIRS and MODIS df
modiscrop<-rbind(modiscrop,snppcrop)
rm(snppcrop)

# adjust to local time
modiscrop$DateTimeUTC<-as.POSIXct(paste0(modiscrop$ACQ_DATE," ",modiscrop$ACQ_TIME), format="%Y-%m-%d %H%M")
modiscrop$DateTimeAZ7=as.POSIXct(format(modiscrop$DateTimeUTC,tz="America/Phoenix"))
modiscrop$DateAZ<-as.Date(format(modiscrop$DateTimeAZ7, "%Y-%m-%d"))
modiscrop$DateAZ_doy<-as.numeric(format(modiscrop$DateAZ,"%j"))

##### extract MTBS data - only do once ----
# dirs<-list.dirs(path = "./data/mtbs/nm", full.names = TRUE, recursive = TRUE)
# for(i in 2:length(dirs)){
#   zipF <- list.files(path = dirs[i], pattern = "*.zip", full.names = TRUE)
#   # unzip all your files
#   plyr::ldply(.data = zipF, .fun = unzip, exdir = dirs[i])
# }
######

# # build file list
# dirs<-list.dirs(path = "./data/mtbs", full.names = TRUE, recursive = TRUE)
# fileNames<-cbind.data.frame(dirname(list.files(path = dirs, pattern = "*bndy.shp", full.names = TRUE)),
#                        basename(list.files(path = dirs, pattern = "*bndy.shp", full.names = TRUE)))
# colnames(fileNames)<-c("dirName","baseName")
#   fileNames[] <- lapply(fileNames, as.character)
# # leave out any fires?
# fileNames<-fileNames[-c(13),] # telegraph not in MTBS format

# load/filter mtbs perims, mtbs_perims.R ----
load("~/RProjects/BurnPeriodResearch/data/AZNM_mtbs_perims_1984_2022_wProv.RData")
  # remove factors
  perimsCrop$BurnBndAc<-as.numeric(as.character(perimsCrop$BurnBndAc))
  perimsCrop$Incid_Name<-as.character(perimsCrop$Incid_Name)
  perimsCrop$Ig_Date<-as.Date(perimsCrop$Ig_Date, "%Y-%m-%d")
  perimsCrop$year<-as.numeric(format(as.Date(perimsCrop$Ig_Date, format="%Y/%m/%d"),"%Y"))
  #min(fc$DISCOVERY_DATE); max(fc$CONT_DATE)
  #min(modiscrop$ACQ_DATE); max(modiscrop$ACQ_DATE)
  perimsCrop<-subset(perimsCrop, Ig_Date>=min(modiscrop$ACQ_DATE))
  perimsCrop<-subset(perimsCrop, BurnBndAc>=10000)
  perimsCrop<-subset(perimsCrop, Incid_Type=="Wildfire")
  #perimsCrop<-subset(perimsCrop, Incid_Type=="Prescribed Fire")
  
#####


# put results in list
fireProgList<-list()

# Loop here...
for(i in 1:nrow(perimsCrop)){
  
  # load in shapefile
  # perim <- rgdal::readOGR(dsn = fileNames$dirName[i], layer = tools::file_path_sans_ext(fileNames$baseName[i])) 
  # perim<-spTransform(perim, CRS("+proj=longlat +datum=WGS84"))
  
  perim<-perimsCrop[i,]
  
  fc_idx<-min(which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name & 
                      fc$FIRE_YEAR==perim@data$year))
  
  # if("PolygonDat" %in% names(perim)){
  #   perim$year<-as.numeric(format(as.Date(perim$PolygonDat, format="%Y/%m/%d"),"%Y"))
  # } else {
  #   perim$year<-as.numeric(format(as.Date(perim$Ig_Date, format="%Y/%m/%d"),"%Y"))
  # }
  
  # get discovery and containment dates
  discDate<-min(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name & 
                           fc$FIRE_YEAR==perim@data$year),"DISCOVERY_DATE"])
  contDate<-max(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name &
                           fc$FIRE_YEAR==perim@data$year),"CONT_DATE"])
  
  # if fc data is missing
  if(is.na(as.character(discDate))){
    discDate<-as.Date(perim$Ig_Date[1],"%Y/%m/%d")
  }else{
  }
  
  # give 30 day window if 
  if(is.na(as.character(contDate))){
    contDate<-discDate+30
    contDateUnknown<-"yes"
  }else{
    contDateUnknown<-"no"
  }
  
  fcFireSize<-sum(fc[which(fc$MTBS_FIRE_NAME==perim@data$Incid_Name &
                             fc$FIRE_YEAR==perim@data$year),"FIRE_SIZE"])
  
  if(fcFireSize==0){
    fcFireSize<-as.numeric(as.character(perim$BurnBndAc[1]))
  }else{}

  mtbsFireSize<-perim$BurnBndAc
    
  #contDate<-as.Date("2002-07-01")
  # set manually
  #discDate<-as.Date("2021-06-04")
  #contDate<-as.Date("2021-07-03")
  
  # subset modis FRP in event dates
  #modisFRP<-subset(modiscrop,ACQ_DATE>=discDate & ACQ_DATE<=contDate )
  modisFRP<-subset(modiscrop,DateAZ>=discDate & DateAZ<=contDate )
  
  if(nrow(modisFRP)!=0){
  
      colnames(modisFRP)[1:2]<-c("Latitude","Longitude")
      
      # look for identical points
      modisFRP$latlon<-paste0(modisFRP$Latitude,"_",modisFRP$Longitude)
      nonDup<-modisFRP[!duplicated(modisFRP$latlon),]
      setdiff(modisFRP$latlon,nonDup$latlon)
      # trim duplicates
      which(!duplicated(modisFRP$latlon)==FALSE)
      modisFRP<-modisFRP[!duplicated(modisFRP$latlon),]
      
      # look at UTC vs local date
      #modisFRP$dateDiff<- as.numeric(modisFRP$ACQ_DATE-modisFRP$DateAZ)
      
      # convert frp to spatial points
      coordinates(modisFRP)<- ~ Longitude + Latitude
      proj4string(modisFRP) <- proj4string(perim)
      
      # ADD IN 1km buffer from Parks 2014
      perim_proj <- spTransform(perim, CRS("+init=epsg:26912"))
      perim_buff <- rgeos::gBuffer(perim_proj, width=1000, quadsegs=100)
      perim_buff <- spTransform(perim_buff, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      # Extract polygon ID's
      ( pid <- sapply(slot(perim_buff, "polygons"), function(x) slot(x, "ID")) )
      # Create dataframe with correct rownames
      ( perim_buff.df <- data.frame( ID=1:length(perim_buff), row.names = pid) ) 
      perim_buff <- SpatialPolygonsDataFrame(perim_buff,perim_buff.df)
      ####
      
      # subset points to perim
      crs(modisFRP)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      temp<-sp::over(modisFRP, perim_buff) # or change to perim for no buffer
      modisFRP<-modisFRP[-(which(is.na(temp$ID)==TRUE)),] # change temp$Event_ID for perim 
      # create FRP time stamps
      # modisFRP$DateTimeUTC<-as.POSIXct(paste0(modisFRP$ACQ_DATE," ",modisFRP$ACQ_TIME), format="%Y-%m-%d %H%M")
      # modisFRP$DateTimeAZ7=as.POSIXct(format(modisFRP$DateTimeUTC,tz="America/Phoenix"))
      # modisFRP$DateAZ<-as.Date(format(modisFRP$DateTimeAZ7, "%Y-%m-%d"))
      # modisFRP$DateAZ_doy<-as.numeric(format(modisFRP$DateAZ,"%j"))
      prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      crs(modisFRP)<-prj_dd
      #plot(perim)
      #plot(modisFRP, add=TRUE)
     
      # if FRP points are empty
      
      if(nrow(modisFRP)!=0){
       
          # create raster for fire progression IDW, adapted from IDW_AllNetworks.R
          #prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
          r<-raster(extent(perim),res=0.0025) # 0.01
          crs(r)<-prj_dd
          progGrid<- mask(r, perim)
          
          ##### set IDW with cross validation ----
          # set idw parameters
          beta<-2
          neighb<-nrow(modisFRP)-1
          # IDW using gstat
          idwFRP <- gstat(formula=DateAZ_doy~1, locations=modisFRP, nmax=neighb, set=list(idp = beta))
          idwRas <- interpolate(r, idwFRP)
          # plot(idwRas)
          # plot(perim, add=TRUE)
          # get cross val info
            #crossval <- gstat.cv(idwFRP)
            #crossval_rmse <- RMSE(crossval$residual)
          #####
          
          # mask data to perim and calc areas
          fireProg<-round(mask(idwRas,perim))
          fireDays<-as.data.frame(table(values(fireProg)))
          fireDays$km2<-fireDays$Freq*(1/16) # resolution correction, pixels in km2
          fireDays$acres<-fireDays$km2*247.105 # 1km2=247.105 ac
          sum(fireDays$acres)
          fireDays$Var1<-as.numeric(as.character(fireDays$Var1))
          
          # join back to modis table
          fireDays<-merge(modisFRP@data,fireDays, by.x="DateAZ_doy", by.y="Var1")
          
          # get daily fire progression stats
          # get daily summed FRP
          dailyFireProg<-fireDays %>% 
            group_by(DateAZ) %>%
            summarize(sumFRP=sum(FRP),
                      maxFRP=max(FRP),
                      countFRP=n(),
                      nightSumFRP=sum(FRP[DAYNIGHT=="N"]),
                      nightMaxFRP=max(FRP[DAYNIGHT=="N"]),
                      nightCountFRP=sum(DAYNIGHT=="N"),
                      acres=first(acres))
          dailyFireProg$cumSum<-cumsum(dailyFireProg$acres)
          dailyFireProg$FireName<-as.character(perim$Incid_Name[1])
      
      }else{
          dailyFireProg<-cbind.data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
          colnames(dailyFireProg)<-c("DateAZ","sumFRP","maxFRP","countFRP","nightSumFRP","nightMaxFRP","nightCountFRP","acres",
                                     "cumSum","FireName")
          dailyFireProg$FireName<-as.character(perim$Incid_Name[1])
          fireProg<-NA
      }
  }else{
    dailyFireProg<-cbind.data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    colnames(dailyFireProg)<-c("DateAZ","sumFRP","maxFRP","countFRP","nightSumFRP","nightMaxFRP","nightCountFRP","acres",
                               "cumSum","FireName")
    dailyFireProg$FireName<-as.character(perim$Incid_Name[1])
    fireProg<-NA
  }
  # put results in list
  #fireProgList[[i]]<-list(dailyFireProg,modisFRP,perim,fireProg,crossval_rmse,fcFireSize,contDateUnknown)
  fireProgList[[i]]<-list(dailyFireProg,modisFRP,perim,fireProg,fcFireSize,contDateUnknown, mtbsFireSize)
  
  print(i)
}

save(fireProgList, file="./data/fireProgression_Stats_mtbs_perims_gt10K.RData")




