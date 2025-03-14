library(basemaps)
data(ext)
# or use draw_ext() to interactively draw an extent yourself

# view all available maps
get_maptypes()

# set defaults for the basemap
set_defaults(map_service = "osm", map_type = "topographic")

# load and return basemap map as class of choice, e.g. as image using magick:
basemap_magick(ext)
#> Loading basemap 'topographic' from map service 'osm'..

ext<-sf::st_as_sfc(sf::st_bbox(raster::extent(c(-115,-102.8,31,37.1)),st_crs(4326)),crs=st_crs(4326))

ext<-sf::st_bbox(raster::extent(c(-115,-102.8,31,37.1)),st_crs(4326))


ext<-sf::st_transform(ext,crs=st_crs(3857))

extSW<-draw_ext()
basemap_magick(extSW)

basemap_magick(extSW, map_service = "esri", map_type = "world_topo_map")
basemap_magick(extSW, map_service = "esri", map_type = "delorme_world_base_map")
basemap_magick(extSW, map_service = "esri", map_type = "world_terrain_base")
basemap_magick(extSW, map_service = "esri", map_type = "natgeo_world_map")
basemap_magick(extSW, map_service = "esri", map_type = "world_shaded_relief")


basemap_magick(extSW, map_service = "esri", map_type = "natgeo_world_map")
basemap_magick(extSW, map_service = "esri", map_type = "usa_topo_maps") # add overlays

basemap_magick(extSW, map_service = "osm", map_type = "topographic") # might be good ****
basemap_magick(extSW, map_service = "osm_stamen", map_type = "terrain_bg") # add overlays

# https://maps-for-free.com/
get_maptypes()


# try plotting own topo map
# https://milospopovic.net/crisp-topography-map-with-r/

library(elevatr)
library(tidyverse)
library(raster)
library(marmap)

# load layers
states <- map_data("state")
# psa zones
psa<-rgdal::readOGR(dsn="/home/crimmins/RProjects/BurnPeriodTracker/shapes", layer="National_PSA_Current")
sw_psa<-subset(psa, GACCName=="Southwest Coordination Center")
# get psa centroids for factor order
sw_psaDF<- cbind.data.frame(sw_psa, rgeos::gCentroid(sw_psa,byid=TRUE))
sw_psa_df<-fortify(sw_psa)

# SW extent
#ext<-raster::extent(c(-115,-102.8,31,37.1))
#ext<-(raster::extent(sw_psa))*1.2

# Turn the matrix into a raster
rast <- raster::raster(matrix(rnorm(400),1000,1000))
# Give it lat/lon coords for 36-37°E, 3-2°S
raster::extent(rast) <- (raster::extent(sw_psa))*1.2

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
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = sw_psa_df, aes(x = long, y = lat, group = group), fill=NA, color="black", alpha=0.8)  + # get the state border back on top
  #coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")+
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  theme_minimal() +
  theme(text = element_text(family = "georg", color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "white", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
        plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
        plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank())
  # labs(x = "", 
  #      y = NULL, 
  #      title = "Topographic map of SW", 
  #      subtitle = "", 
  #      caption = "MAC")

ggsave(filename="SW_topo_map_z8.png", width=7, height=4, dpi = 600, device='png', elev_map)







