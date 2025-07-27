#Pseudonaja map
#Author: Lachlan Bourke
#2024-08-15

### geodata ####
#https://cran.r-project.org/web/packages/geodata/geodata.pdf
library(geodata)
codes<-country_codes(query=NULL)

# Download map data and plot other data over the map
#Use geodata package. 

#Country with province borders
AUS<- geodata::gadm(country = "AUS", level = 1, path = "Map_data", version = "latest", resolution = 1, type = "sf")
plot(AUS)

#### Make Simple plot ####
library(tmap) #tmap book https://r-tmap.github.io/tmap-book/layout.html
library(sf)
#use below code to convert spatvector to simple features (sf) (can then map in tmap)
AUS <- sf::st_as_sf(AUS)

#Need to simplify as has polygons and multipolygons. If use above will get
#duplicate text values for the provinces.
#https://stackoverflow.com/questions/69947457/tm-text-produces-duplicate-text-in-tmap
library(dplyr)
AUS <- AUS %>%  
  st_cast()

#View names
AUS$NAME_1
#Change to abbreviations
AUS$NAME_1[AUS$NAME_1=="Ashmore and Cartier Islands"] <- ""
AUS$NAME_1[AUS$NAME_1=="Coral Sea Islands Territory"] <- ""
AUS$NAME_1[AUS$NAME_1=="New South Wales"] <- "NSW"
AUS$NAME_1[AUS$NAME_1=="Queensland"] <- "QLD"
AUS$NAME_1[AUS$NAME_1=="Tasmania"] <- "TAS"
AUS$NAME_1[AUS$NAME_1=="Western Australia"] <- ""
AUS$NAME_1[AUS$NAME_1=="Australian Capital Territory"] <- "ACT"
AUS$NAME_1[AUS$NAME_1=="Jervis Bay Territory"] <- ""
AUS$NAME_1[AUS$NAME_1=="Northern Territory"] <- "NT"
AUS$NAME_1[AUS$NAME_1=="South Australia"] <- "SA"
AUS$NAME_1[AUS$NAME_1=="Victoria"] <- "VIC"

#Area to map (use http://bboxfinder.com)
bbox_AUS <- st_bbox(c(xmin = 129.5, xmax = 154, ymax = -10, ymin = -44), crs = st_crs(4326))

#Test map
AUS_map <- tm_shape(AUS, bbox = bbox_AUS)+
  tm_polygons(alpha=0)+ ##what AUS consists of
  tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)
AUS_map


####Add samples to map#####
# sites
#venom sample location data frame
dat <- read.csv("Pseudonaja_localities_forR.csv", header=TRUE)

#Convert data frame to simple features (sf).
sites <- sf::st_as_sf(dat, coords = c("Longitude", "Latitude"))

#draft1
AUS_map <- tm_shape(AUS, bbox = bbox_AUS)+
  tm_polygons(alpha=0)+ ##what AUS consists of
  tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=3,alpha = 0.5, col = "Clade", #?pch to see symbols and corresponding numbers
             title.col = "Clade", palette = c("red","blue"))+
  tm_layout(legend.position = c(0.02,0.05),legend.width = 10,
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 1, legend.text.fontfamily = "Times",
            frame = F) #Frame T is useful to orientate things
AUS_map

tmap_save(AUS_map)


#Country with elevation data
AUS.elevation<-elevation_30s("AUS",path = "Altitude_map_data", mask = T)

AUS_map <- tm_shape(AUS.elevation, bbox = bbox_AUS)+
  tm_raster(title = "Elevation (m)",n=8, 
            palette = c("#A7DFD2","#75AE18","#A6B40C","#CFB938","#F3BE65",
                        "#FFC393","#FFCBC0","#F1F1F1"))+
  tm_shape(AUS, bbox = bbox_AUS)+
  tm_polygons(alpha=0)+ ##what AUS consists of
  tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=3,alpha = 0.5, col = "Clade", #?pch to see symbols and corresponding numbers
             title.col = "Clade", palette = c("red","blue"))+
  tm_layout(legend.position = c(0.02,0.05),legend.width = 10,
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 1, legend.text.fontfamily = "Times",
            frame = F) #Frame T is useful to orientate things
AUS_map



#draft2
AUS_map <- tm_shape(AUS, bbox = bbox_AUS)+
  tm_polygons()+ ##what AUS consists of
  tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=0.1,alpha = 0.5, col = "Clade", #?pch to see symbols and corresponding numbers
             title.col = "Clade",
             palette = c("red","blue"))+
  tm_text("Locality2",size = 0.5)+
  tm_layout(legend.position = c(0.02,0.05),legend.width = 10,
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 1, legend.text.fontfamily = "Times",
            frame = F) #Frame T is useful to orientate things
AUS_map

tmap_save(AUS_map)

#### Rain #####

rain<-worldclim_country(country="AUS",var="prec",path="worldclim")

#need to convert raster to stars object
library(stars)
library(raster)
rain<-st_as_stars(raster(rain), proxy=FALSE)

#raster.downsample = FALSE makes raster image more detailed
AUS_map <- tm_shape(rain, bbox = bbox_AUS, raster.downsample = FALSE)+
  tm_raster(title="rainfall")+ ##what AUS consists of
  tm_shape(AUS)+
  tm_polygons(alpha=0)+
  tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=3,alpha = 0.5, col = "Clade", #?pch to see symbols and corresponding numbers
             title.col = "Clade", palette = c("red","blue"))+
  tm_layout(legend.position = c(0.02,0.05),legend.width = 10,
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 1, legend.text.fontfamily = "Times",
            frame = F) #Frame T is useful to orientate things
AUS_map

tmap_save(AUS_map)


#### Ways to save map ####

#https://r-tmap.github.io/tmap-book/save.html
tmap_save(AUS_map)#png
tmap_save(AUS_map,filename = "tmap.tiff", dpi = 600)

#Vector map
tmap_save(AUS_map,"tmap.svg")

#Interactive map
tmap_save(AUS_map,"interactive_tmap.html")
require(svglite)
svglite("AUS_map.svg", width = 10,height = 10)
print(AUS_map)
dev.off()

