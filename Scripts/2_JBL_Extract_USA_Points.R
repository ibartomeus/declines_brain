###############################################################---
#Extract coordinates from polygon and explore graphically----
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
#Plot spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region),
                    color = "white", fill = "lightgray", size = 0.05) +
    geom_point(data = d,aes(decimalLongitude, decimalLatitude),
               alpha = 0.7, size = 0.1) 


#Separate by continent
namerica <- d %>% filter(Continent %in% c("North America"))

#Plot north america spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region), color = "white", 
                    fill = "lightgray", size = 0.1) +
    geom_point(data = namerica,aes(decimalLongitude, decimalLatitude),
               alpha = 0.1, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-120, -50), ylim = c(20, 70), expand = FALSE)

#East coast seems to be the area with highest point density

###
#Select states interest----
###

#Closer look
library(spData) #to load us_states data

#Create USA polygon
polygon <- us_states %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")  %>% sf::st_as_sf(crs = 4326) %>%
    sf::st_transform(crs = 4326) 

#set same coordinate system
usa <- namerica %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% 
    st_set_crs(4326)

#Select points within USA
usa_points <- st_intersection(polygon,usa)

#separate geometry to long lat
separated_coord <- usa_points %>% 
dplyr::mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])

#select points within USA
separated_coord <- as.data.frame(separated_coord)

states <- map_data("state")
#Plot north america spatial data
ggplot() + geom_map(data = states, map = states,
                    aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
                    size = 0.1) +guides(fill=FALSE)+
    geom_point(data = separated_coord,aes(long, lat),
               alpha = 0.3, size = 0.05) + 
    coord_sf(xlim = c(-100, -60), ylim = c(25, 50), expand = FALSE)

#States from east coast with highest point density
vars <- c("Vermont", "New Hampshire", "Massachusetts", "Rhode Island","Connecticut",
          "New Jersey","Delaware", "Maryland")
usa_states <- filter(us_states, NAME %in% vars)

#Dataset of usa points
polygon <- usa_states %>% summarise(geometry = st_combine(geometry)) %>% 
st_cast("POLYGON")  %>% sf::st_as_sf(crs = 4326) %>% sf::st_transform(crs = 4326)

area_states <- polygon %>%  mutate(area = st_area(polygon))

b <- separated_coord %>% 
    st_as_sf(coords = c("long", "lat"), dim = "XY") %>% st_set_crs(4326)%>%
    sf::st_as_sf(crs = 4326)

#Select points
east_coast <- st_intersection(polygon,b)

#separate geometry to long lat
east_coast_long_lat <- east_coast %>% 
    dplyr::mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])


###
#Select now counties of interest of NY----
###

#Fisrt I load county map of USA
#(I'm assuming that I can convert this to a multypolygon but maybe this is an issue)
counties <-map_data("county")
#Then I subset by New York
ny_county <- subset(counties, region=="new york")
#Select metropolitan area region of New York state
vars_ny <- c("orange",  "putnam", "rockland",
             "westchester", "bronx", "new york","kings", "richmond", "queens", "nassau","suffolk")
#Extract counties of interest
ny_metropolitan <- filter(ny_county, subregion %in% vars_ny)

#Now I have to create a multipolygon from this dataframe
#First create an sf object  
ny_area <- ny_metropolitan %>% st_as_sf(coords = c("long", "lat"), dim = "XY") %>% 
    st_set_crs(4326) 

#Now convert to polygon
sf <- sfheaders::sf_polygon(
    obj = ny_metropolitan
    , x = "long"
    , y = "lat"
    , polygon_id = "subregion"
)

sf::st_crs( sf ) <- 4326

area_ny <- sf %>%  mutate(area = st_area(sf))

#Same as before but wrote here to remember what it is
b <- separated_coord %>% 
    st_as_sf(coords = c("long", "lat"), dim = "XY") %>% st_set_crs(4326)%>%
    sf::st_as_sf(crs = 4326)

#calculate points that fall in polygon
east_coast_ny <- st_intersection(sf,b)

#separate geometry to long lat
east_coast_ny_long_lat <- east_coast_ny %>% 
    dplyr::mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])

#Plot north america spatial data
ggplot() + geom_map(data = ny_metropolitan, map = ny_metropolitan,
                    aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
                    size = 0.1) +guides(fill=FALSE)+
    geom_point(data = east_coast_ny_long_lat,aes(long, lat),
               alpha = 0.3, size = 0.05) 
#Seems ok, now go for all!

#Load for states of interest for plotting
vars <- c("vermont", "new hampshire", "massachusetts", "rhode island","connecticut",
          "new jersey","delaware", "maryland")
usa_states <- filter(states, region %in% vars)

#rbind both spatial points of NY area and the 8 different states
east_coast_long_lat$subregion <- NA
all_east_coast <- rbind(east_coast_long_lat, east_coast_ny_long_lat)

#Plot north america spatial data
ggplot() + geom_map(data = usa_states, map = usa_states,
aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
size = 0.1) + geom_map(data = ny_metropolitan, 
map = ny_metropolitan,aes(long, lat, map_id = region, fill=region, group = group), 
size = 0.1) +guides(fill=FALSE) + geom_point(data = all_east_coast,aes(long, lat),
alpha = 0.3, size = 0.05)
    
#Save data now, the code takes some time
#Point of interests
all_east_coast <- all_east_coast %>% select(-c(X.1, X))
write.csv(all_east_coast, "Data/all_east_coast.csv")


#Calculate area in km2
set_units(sum(area_states$area) + sum(area_ny$area), km^2)
#146180 [km^2]
