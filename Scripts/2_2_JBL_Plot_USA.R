
#Load libraries
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
library(sf)
library(tidyverse)
library(units)
library(patchwork)
library(spData) #to loas us_states data

#Read data
all_east_coast <- data.frame(fread("Data/Usa_data/usa_all_long_lat.csv.gz"))
#Fix colname position
names(all_east_coast)[1:(ncol(all_east_coast)-1)] <- names(all_east_coast)[2:ncol(all_east_coast)]
all_east_coast[, ncol(all_east_coast)] <- NULL
usa_states_1 <- read.csv("Data/Usa_data/usa_states.csv")
ny_metropolitan_1 <- read.csv("Data/Usa_data/ny_metropolitan.csv")

#Load worldmap
world <- map_data("world")

zoom <-
    c(
        "xmin" = -70,
        "xmax" = -80,
        "ymin" = 38,
        "ymax" = 47
    ) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf(crs = 4326)


p1 <- ggplot() + geom_map(data = world, map = world,aes(long, lat, map_id = region), color = "white", 
fill = "lightgray", size = 0.1) + geom_map(data = usa_states_1, map = usa_states_1,
aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
size = 0.1) + geom_map(data = ny_metropolitan_1, 
map = ny_metropolitan_1,aes(long, lat, map_id = region, fill=region, group = group), 
size = 0.1) +guides(fill=FALSE) + geom_point(data = all_east_coast,aes(long, lat),
size = 0.15, stroke = 0, shape = 16)+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1)) +
ylab("Latitude")+xlab("Longitude") +   geom_sf(data = zoom, colour = "black", fill = NA)+
coord_sf(xlim = c(-105, -68), ylim = c(23, 52)) 

p1


p2 <- ggplot() + geom_map(data = world, map = world,aes(long, lat, map_id = region), color = "white", 
fill = "lightgray", size = 0.1) +geom_map(data = usa_states_1, map = usa_states_1,
aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
size = 0.1) + geom_map(data = ny_metropolitan_1, 
map = ny_metropolitan_1,aes(long, lat, map_id = region, fill=region, group = group), 
size = 0.1) +guides(fill=FALSE) + geom_point(data = all_east_coast,aes(long, lat),
size = 0.2, stroke = 0, shape = 16)+
theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1)) +
coord_sf(xlim = c(-80, -69), ylim = c(38, 45.5)) + ylab("Latitude")+
xlab("Longitude")

p2
#Combine plots
p1 + p2 +  plot_layout(widths = 1)


