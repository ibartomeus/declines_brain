
#Load libraries
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
library(sf) #gis in r
library(tidyverse)
library(maps)
library(giscoR) #map of europe
library(sf)
library(ggspatial) #to plot arrow
library(patchwork)
library(ggforce)

#Read data 
library(data.table)
all_long_lat <- data.frame(fread("Data/Europe_data/urope_all_long_lat.csv.gz"))
#Fix colname position
names(all_long_lat)[1:(ncol(all_long_lat)-1)] <- names(all_long_lat)[2:ncol(all_long_lat)]
all_long_lat[, ncol(all_long_lat)] <- NULL
euro_map <- st_read("Data/Europe_data/euro_map.shp")
nuts2.sf <- st_read("Data/Europe_data/euro_nuts2.sf.shp")
#Load worldmap
world <- map_data("world")


#Plot and check
p1 <- ggplot(euro_map) +
    geom_sf(aes(fill = CNTR_CODE, group=CNTR_CODE), color = NA, alpha = 1)+ 
    guides(fill="none") +
    geom_point(data = all_long_lat,aes(long, lat),size = 0.15, stroke = 0, shape = 16) +
    geom_sf(fill = "transparent", color = "gray20", size = 0.25, 
            data = . %>% group_by(CNTR_CODE) %>% summarise()) +
    ylab("Latitude") + xlab("Longitude")+
    geom_sf(data= nuts2.sf,aes(fill = NA, group=CNTR_CODE), color = NA, alpha = 0.3)+
    coord_sf(xlim = c(-5, 20), ylim = c(46, 60)) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))

#Plot EUROPE spatial data

zoom <-
    c(
        "xmin" = -5,
        "xmax" = 16,
        "ymin" = 47,
        "ymax" = 58
    ) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf(crs = 4326)

p2 <- ggplot() + geom_map(data = world, map = world,
                          aes(long, lat, map_id = region), color = "white", 
                          fill = "lightgray", size = 0.1)  + ylim(0,70) +
    geom_sf(data=euro_map,aes(fill = CNTR_CODE, group=CNTR_CODE), color = NA, alpha = 0.3)+ 
    guides(fill="none") +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))+
    geom_sf(data = zoom, colour = "black", fill = NA) +
    coord_sf(xlim = c(-15, 40), ylim = c(30, 75), expand = FALSE)+
    ylab("Latitude") + xlab("Longitude") 

#Combine plots
p2  + p1 +  plot_layout(widths = 1)
