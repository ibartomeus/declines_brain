###############################################################---
#Extract points from Europe polygon and explore graphically----
###############################################################---
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
library(sf) #gis in r
library(tidyverse)
library(units)
library(maps)
library(giscoR) #map of europe
library(sf)
library(ggspatial) #to plot arrow
library(patchwork)
library(ggforce)
    
#read data to avoid running all again
library(data.table)
d <- data.frame(fread("Data/gbif_data_final.csv.gz"))

#Load worldmap
world <- map_data("world")

#Separate by continent
europe <- d %>% filter(Continent %in% c("Europe"))
str(europe)
#Plot north america spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region), color = "white", 
                    fill = "lightgray", size = 0.1) +
    geom_point(data = europe,aes(decimalLongitude, decimalLatitude),
               alpha = 0.1, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-15, 40), ylim = c(30, 75), expand = FALSE)


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

#Plot map
ggplot(europe_map) +
    geom_sf(aes(fill = CNTR_CODE), color = NA, alpha = 0.3) +
    geom_sf(data = borders, fill = NA, size = 0.1, col = "black") +
    geom_point(data = europe,aes(decimalLongitude, decimalLatitude),
    alpha = 0.1, size = 0.025) + ylim(30,70)+xlim(-10,40)+
    guides(fill=FALSE)


#EU members plus UK
eu2016 <- c("UK","CH", gisco_countrycode[gisco_countrycode$eu, ]$CNTR_CODE)

nuts2 <- gisco_get_nuts(
    year = "2016",
    epsg = "3035",
    resolution = "3",
    nuts_level = "2",
    country = eu2016)

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
                  all.x = TRUE)

nuts2.sf <- nuts2.sf %>%  sf::st_transform(crs = 4326) 

#Select metropolitan area region of New York state
vars <- c("BE",  "NL", "LU","UK", "DK", "DE")
#Extract counties of interest
euro_map <- filter(nuts2.sf, CNTR_CODE %in% vars)

#Exlcude wales, ireland and scotlad (select England)
vars <- c("Northern Ireland", "East Wales","Eastern Scotland","Highlands and Islands","North Eastern Scotland",
          "West Central Scotland","West Wales and The Valleys","Southern Scotland")
#Extract counties of interest
euro_map_1 <- filter(euro_map, !NUTS_NAME %in% vars)

#Plot and check
ggplot(euro_map_1) +
geom_sf(aes(fill = CNTR_CODE, group=CNTR_CODE), color = NA, alpha = 0.3)+ guides(fill=FALSE)

#Add country and extract points by country
europe$Country <-  map.where(database="world", europe$decimalLongitude, europe$decimalLatitude)
levels(factor(europe$Country))

vars <- c("Germany","Germany:Fehmarn", "Germany:Rugen", "Belgium",
          "Denmark", "Denmark:2", "Denmark:8", "Denmark:Als",
          "Denmark:Bornholm", "Denmark:Fyn", "Denmark:Laeso",
          "Denmark:Langeland", "Denmark:Lolland", "Denmark:Mon",
          "Denmark:Samso", "Denmark:Sjaelland","Luxembourg",
          "Netherlands", "Netherlands:Ameland", "Netherlands:Schouwen-Duiveland",
          "Netherlands:South", "Netherlands:Texel")

europe_filtered <- filter(europe, Country %in% vars)

#Plot and check
ggplot(euro_map_1) +
geom_sf(aes(fill = CNTR_CODE, group=CNTR_CODE), color = NA,
alpha = 0.3)+ guides(fill=FALSE)+geom_point(data = europe_filtered,
aes(decimalLongitude, decimalLatitude),size = 0.15, stroke = 0, shape = 16) 

#Now create a polygon of England and extract the points that fall on it
#check levels
levels(factor(euro_map_1$CNTR_CODE))

polygon <- euro_map_1 %>% filter(CNTR_CODE=="UK") %>% summarise(geometry = st_combine(geometry)) %>% 
    st_cast("POLYGON")  %>% sf::st_as_sf(crs = 4326) 

#Select points that fall in UK
great_britain_points <- europe %>% filter(Country=="UK:Great Britain"|Country=="UK:Isle of Wight") %>%    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% st_set_crs(4326)%>%
    sf::st_as_sf(crs = 4326)

england <- st_intersection(polygon,great_britain_points)

#Now convert 
europe_other <- europe_filtered %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% st_set_crs(4326)%>%
    sf::st_as_sf(crs = 4326)

all <- rbind(england,europe_other)

#separate geometry to long lat
all_long_lat <- all %>% 
    dplyr::mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])

#Calculate area
area_eu <- euro_map_1 %>%  mutate(area = st_area(euro_map_1))
set_units(sum(area_eu$area) , km^2)

#Save data
write.csv(all_long_lat, file=gzfile("Data/Europe_data/urope_all_long_lat.csv.gz"),row.names=FALSE)
st_write(euro_map_1, "Data/Europe_data/euro_map.shp")
st_write(nuts2.sf, "Data/Europe_data/euro_nuts2.sf.shp")
#world <- map_data("world")

