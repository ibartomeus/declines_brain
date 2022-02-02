###############################################################---
#Extract coordinates from polygon and explore graphically----
###############################################################---
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
library(sf)
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
europe <- d %>% filter(Continent %in% c("Europe"))

#Plot north america spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region), color = "white", 
                    fill = "lightgray", size = 0.1) +
    geom_point(data = namerica,aes(decimalLongitude, decimalLatitude),
               alpha = 0.1, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-120, -50), ylim = c(20, 70), expand = FALSE)

#Plot europe spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region), color = "white", 
                    fill = "lightgray", size = 0.1) +
    geom_point(data = europe,aes(decimalLongitude, decimalLatitude),
               alpha = 0.7, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-15, 40), ylim = c(30, 75), expand = FALSE)


bb <-
    c(
        "xmin" = 4.005,
        "xmax" = 13.005,
        "ymin" = 49.005,
        "ymax" = 52.005
    ) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf(crs = 4326) %>%
    sf::st_transform(crs = 4326) 


df <- data.frame(
    lon = c(2.5, 7.005,5.505),
    lat = c(51.005, 54.0,49.500)
)

polygon <- df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
polygon

pnts <- df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
polygon <- concaveman(pnts)

plot(polygon, reset = FALSE)
plot(pnts, add = TRUE)

#Calculate are in km^2
units::set_units(bb %>% st_area(), km^2)

europe_land_sf <- europe %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% 
    st_set_crs(4326)

bb_extraction <- st_intersection(bb, europe_land_sf)

ggplot() +
    geom_map(
        data = world,
        map = world,
        aes(long, lat, map_id = region),
        color = "white",
        fill = "lightgray",
        size = 0.1
    ) +
    geom_point(data = europe,
               aes(decimalLongitude, decimalLatitude),
              alpha = 0.7,
              size = 0.05) + ylim(0, 70) +
    geom_sf(data = polygon, colour = "red", fill = NA) +
    coord_sf(xlim = c(0, 20),
             ylim = c(40, 60),
             expand = FALSE)



#Trying to plot USA
library(ggplot2)
library(fiftystater)
library(tidyverse)
library(spData)

states <- map_data("state")

polygon <- us_states %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")  %>% sf::st_as_sf(crs = 4326) %>%
    sf::st_transform(crs = 4326) 

usa <- namerica %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% 
    st_set_crs(4326)

usa_points <- st_intersection(polygon,usa)

library(tidyverse)

separated_coord <- usa_points %>%
    mutate(long = unlist(map(usa_points$geometry,1)),
           lat = unlist(map(usa_points$geometry,2)))

separated_coord <- as.data.frame(separated_coord)


library(dplyr)
vars <- c("vermont", "new hampshire", "massachusetts", "rhode island","connecticut",
          "new jersey","delaware", "maryland")
usa_states <- filter(states, region %in% vars)


ggplot() + geom_map(data = usa_states, map = usa_states,
                    aes(long, lat, map_id = region, fill=region, group = group), 
                    size = 0.1) +guides(fill=FALSE)+
    geom_polygon(aes(x=long, y=lat, group=group),data=ny_metropolitan,fill="grey")


ny_base <- ggplot(data=ny_df, mapping=aes(x=long, y=lat, group=group))+
    coord_fixed(1.3) + 
    geom_polygon(color="black", fill="gray")
ny_base+theme_nothing()

usa <- map_data("usa")
states <- map_data("state")
ny_df <- subset(states, region=="new york" & lat<42)

counties <-map_data("county")

ny_county <- subset(counties, region=="new york")

levels(factor(ny_county$subregion))
#Select metropolitan area region of New York state
vars_ny <- c("orange",  "putnam", "rockland",
             "westchester", "bronx", "new york","kings", "richmond", "queens", "nassau","suffolk")
ny_metropolitan <- filter(ny_county, subregion %in% vars_ny)

ny_base + theme_nothing() + 
    geom_polygon(data=ny_metropolitan,fill=NA, color="white")+
    geom_polygon(color="black", fill=NA)


#Plot north america spatial data
ggplot() + geom_map(data = states, map = states,
                    aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
                size = 0.1) +guides(fill=FALSE)+
    geom_point(data = separated_coord,aes(long, lat),
               alpha = 0.3, size = 0.05) + 
    coord_sf(xlim = c(-100, -60), ylim = c(25, 50), expand = FALSE)
    

