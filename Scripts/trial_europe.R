###############################################################---
    #Extract points from Europe polygon and explore graphically----
###############################################################---
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
library(sf)
library(tidyverse)
library(units)

#read data to avoid running all again
d <- read.csv("Data/gbif_data.csv")

#Load worldmap
world <- map_data("world")

#Separate by continent
europe <- d %>% filter(Continent %in% c("Europe"))

#Plot north america spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region), color = "white", 
                    fill = "lightgray", size = 0.1) +
    geom_point(data = europe,aes(decimalLongitude, decimalLatitude),
               alpha = 0.1, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-15, 40), ylim = c(30, 75), expand = FALSE)


#Now try to find polygons 


#Extract Europe map from giscoR
#install.packages("giscoR")
library(giscoR)
library(sf)
library(dplyr)

#gisco_countrycode
eu2016 <- c("UK", gisco_countrycode[gisco_countrycode$eu, ]$CNTR_CODE)

#Select data
europe_map <- gisco_get_nuts(
    year = "2016",
    epsg = "3035",
    resolution = "3",
    nuts_level = "2",
    country = eu2016) %>%
    sf::st_transform(crs = 4326) 


# Borders
borders <- gisco_get_countries(
    epsg = "3035",
    year = "2016",
    resolution = "3",
    country = eu2016) %>%
    sf::st_transform(crs = 4326) 

ggplot() + geom_map(data = europe_map, map = europe_map,
                    aes(long, lat, map_id = region), color = "white", 
                    fill = "lightgray", size = 0.1)
ggplot(europe_map) +
    geom_sf(aes(fill = CNTR_CODE), color = NA, alpha = 0.3) +
    geom_sf(data = borders, fill = NA, size = 0.1, col = "black") +
    geom_point(data = europe,aes(decimalLongitude, decimalLatitude),
               alpha = 0.1, size = 0.025) + ylim(30,70)+xlim(-10,40)+
    guides(fill=FALSE)


# EU members plus UK

eu2016 <- c("UK", gisco_countrycode[gisco_countrycode$eu, ]$CNTR_CODE)

nuts2 <- gisco_get_nuts(
    year = "2016",
    epsg = "3035",
    resolution = "3",
    nuts_level = "2",
    country = eu2016
)

# Borders
borders <- gisco_get_countries(
    epsg = "3035",
    year = "2016",
    resolution = "3",
    country = eu2016
)

# Eurostat data - Purchase parity power
pps <- giscoR::tgs00026
pps <- pps[pps$time == 2016, ]


nuts2.sf <- merge(nuts2,
                  pps,
                  by.x = "NUTS_ID",
                  by.y = "geo",
                  all.x = TRUE
)

nuts2.sf <- nuts2.sf %>%  sf::st_transform(crs = 4326) 


#Select metropolitan area region of New York state
vars <- c("BE",  "NL", "LU","UK", "DK", "SE")
#Extract counties of interest
euro_map <- filter(nuts2.sf, CNTR_CODE %in% vars)

#Exlcude wales, ireland and scotlad (select England)
vars <- c("Northern Ireland", "East Wales","Eastern Scotland","Highlands and Islands","North Eastern Scotland",
          "West Central Scotland","West Wales and The Valleys","Southern Scotland")
#Extract counties of interest
euro_map_1 <- filter(euro_map, !NUTS_NAME %in% vars)

#Now filter sweeden (select just south)
vars <- c("Mellersta Norrland", "Övre Norrland")
euro_map_2 <- filter(euro_map_1, !NUTS_NAME %in% vars)

#Plot and check
ggplot(euro_map_2) +
geom_sf(aes(fill = CNTR_CODE, group=CNTR_CODE), color = NA, alpha = 0.3)+ guides(fill=FALSE)
#Seems ok

#Now extract the points that fall in the polygon
polygon <- euro_map_2 %>% summarise(geometry = st_combine(geometry)) %>% 
st_cast("POLYGON")  %>% sf::st_as_sf(crs = 4326) 




#to speed up the process I'm going to filter manually some lat longs
europe %>% europe$decimalLatitude


b <- europe[390:450,] %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% st_set_crs(4326)%>%
    sf::st_as_sf(crs = 4326)

europe_high_density <- st_intersection(polygon,b)


