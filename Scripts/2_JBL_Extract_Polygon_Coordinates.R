###############################################################---
#Extract coordinates from polygon and explore graphically----
###############################################################---
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
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
               alpha = 0.7, size = 0.05) + ylim(0,70) +
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
    geom_sf(data = bb, colour = "red", fill = NA) +
    coord_sf(xlim = c(-15, 45),
             ylim = c(30, 75),
             expand = FALSE)

