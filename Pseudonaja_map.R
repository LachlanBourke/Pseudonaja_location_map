#Pseudonaja map
#Author: Lachlan Bourke
#2024-08-15

### Get map file ####
#https://cran.r-project.org/web/packages/geodata/geodata.pdf
library(geodata)

#Country with province borders
AUS<- geodata::gadm(country = "AUS", level = 1, path = "Map_data", version = "latest", resolution = 1, type = "sf")
plot(AUS)

library(sf)
#use below code to convert spatvector to simple features (sf) (can then map in tmap)
AUS <- sf::st_as_sf(AUS)

#Need to simplify as has polygons and multipolygons. If use above will get
#duplicate text values for the states and territories.
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
#bbox_AUS <- st_bbox(c(xmin = 129.5, xmax = 154, ymax = -10, ymin = -44), crs = st_crs(4326))

bbox_AUS <- st_bbox(c(xmin = 129.5, xmax = 155, ymax = -10, ymin = -47), crs = st_crs(4326))

#Test map
library(tmap)
AUS_map <- tm_shape(AUS, bbox = bbox_AUS)+
  tm_polygons(alpha=0)
AUS_map

####Sites#####
#venom site location data frame
dat <- read.csv("Pseudonaja_localities_forR.csv", header=TRUE)

#Convert data frame to simple features (sf).
sites <- sf::st_as_sf(dat, coords = c("Longitude", "Latitude"))

#### worldclim ####
temp<-worldclim_country(country="AUS",var="tavg",path="worldclim")

#can't plot. Get error.
#https://github.com/r-spatial/mapview/issues/305
#SOLUTION: need to convert spatraster to stars object. 
#Just like needed to convert spatvector to sf.
#tmap likes using sf and stars objects.
library(stars)
library(raster)

temp<-st_as_stars(raster(temp), proxy=FALSE)

#### Make Map ####

#Raster.downsample = TRUE will make better quality raster, but slower

AUS_map <- tm_shape(temp, bbox = bbox_AUS, raster.downsample = TRUE)+
  tm_raster(title="Average Temp °C")+ ##what AUS consists of
  tm_shape(AUS)+
  tm_polygons(alpha=0)+
  tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.02, 1), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=3,alpha = 0.5, shape = "Clade", #?pch to see symbols and corresponding numbers
             title.shape = "Clade", shapes = c(21,24), #?pch
             col = "Clade", palette = c("purple","blue"), 
             legend.shape.show = FALSE, # remove legend for shape, so just keep legend for colour
             shapes.legend = c(21,24))+ #makes the colour legend symbols the right shape
  tm_layout(legend.position = c(0.02,0),
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 1, legend.text.fontfamily = "Times",
            frame = F) #Frame T is useful to orientate things
AUS_map

tmap_save(AUS_map)
tmap_save(AUS_map,filename = "Ptex_samples_temp.tiff", dpi = 600)
