#Filter MA thesis Data

#Read data
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
library(sf)
library(tidyverse)
library(units)
library(patchwork)
library(spData) #to loas us_states data
library(data.table)

data = read_csv("Data/Usa_data/thesis_chapter1_data_ma.csv") %>% 
select(gen_sp, latitude, longitude, country, state, county, city, site, habitat.extracted,
year1, month1, day1) %>% 
rename(long = longitude, lat = latitude, year=year1, month=month1, day=day1)

all_east_coast <- data.frame(fread("Data/Usa_data/usa_all_long_lat.csv.gz"))
#Fix colname position
names(all_east_coast)[1:(ncol(all_east_coast)-1)] <- names(all_east_coast)[2:ncol(all_east_coast)]
all_east_coast[, ncol(all_east_coast)] <- NULL
usa_states_1 <- read.csv("Data/Usa_data/usa_states.csv")
ny_metropolitan_1 <- read.csv("Data/Usa_data/ny_metropolitan.csv")

world <- map_data("world")
i
#Now create for loop for the different species 
ggplot() + geom_map(data = world, map = world,aes(long, lat, map_id = region), color = "white", 
fill = "lightgray", size = 0.1) + geom_map(data = usa_states_1, map = usa_states_1,
aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
size = 0.1) + geom_map(data = ny_metropolitan_1, 
map = ny_metropolitan_1,aes(long, lat, map_id = region, fill=region, group = group), 
size = 0.1) +  scale_fill_manual(values =c("#85BEDC", "#DEB478", "#CCB62F", "#115896",
"#A1B654", "#CD8862", "#CABEE9","#D04E59","#C582B2"))+guides(fill=FALSE) +
theme(panel.grid.major = element_line(color = gray(0.5), 
linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1)) +
coord_sf(xlim = c(-80, -69), ylim = c(38, 45.5))  + ylab("Latitude")+xlab("Longitude")+
geom_point(data = data,aes(long, lat),
size = 0.5, stroke = 0, shape = 16)

colnames(data)
#States from east coast with highest point density
vars <- c("Vermont", "New Hampshire", "Massachusetts", "Rhode Island","Connecticut",
          "New Jersey","Delaware", "Maryland")

data_filtered <- filter(data, state %in% vars) %>% 
filter(!long < -80)

#There are some points off, fix later on

ggplot() + geom_map(data = world, map = world,aes(long, lat, map_id = region), color = "white", 
fill = "lightgray", size = 0.1) + geom_map(data = usa_states_1, map = usa_states_1,
aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
size = 0.1) + geom_map(data = ny_metropolitan_1, 
map = ny_metropolitan_1,aes(long, lat, map_id = region, fill=region, group = group), 
size = 0.1) +  scale_fill_manual(values =c("#85BEDC", "#DEB478", "#CCB62F", "#115896",
"#A1B654", "#CD8862", "#CABEE9","#D04E59","#C582B2"))+guides(fill=FALSE) +
theme(panel.grid.major = element_line(color = gray(0.5), 
linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1)) +
coord_sf(xlim = c(-80, -69), ylim = c(38, 45.5))  + ylab("Latitude")+xlab("Longitude")+
geom_point(data = data_filtered,aes(long, lat),
size = 0.5, stroke = 0, shape = 16)

#Save data
write_csv(data_filtered, "Data/Usa_data/usa_thesis_ma_filtered.csv")








