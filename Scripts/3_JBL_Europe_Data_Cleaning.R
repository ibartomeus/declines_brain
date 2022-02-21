######-
#In this script we prepare Europe data for analysis
#We take 5 key decisions in data cleaning
#1)Filter records above 1985 (this is decided based on the GIS data available)
#2)Filter by unique capture event
#3)Filter by minimum of 3 decimals on coordinates
#4)Filter number of levels per species (minimum N=50)
#5)Filter by wide geographical distribution
######-

#Load libraries
library(data.table)
library(tidyverse)

#Filter Europe data
#Read data 
all_long_lat <- data.frame(fread("Data/Europe_data/urope_all_long_lat.csv.gz"))
#Fix colname position
names(all_long_lat)[1:(ncol(all_long_lat)-1)] <- names(all_long_lat)[2:ncol(all_long_lat)]
all_long_lat[, ncol(all_long_lat)] <- NULL

all_long_lat$long <- gsub(")", "", all_long_lat$long)

colnames(all_long_lat)

all <- all_long_lat %>% select("scientificName", "family", "genus", "year", "month", "day","recordedBy",
                        "identifiedBy","sex","stateProvince", "locality", "Continent",
                        "Country", "long", "lat")

#############-
#FIRST FILTER----
#############-
#First filter all records above 1985 
all_above_1985 <- all %>% filter(year>1985)

#############-
#SECOND FILTER----
#############-
#Second filter unique capture event based on: year, month, day, locality
locality_na <-  all_above_1985 %>% filter(is.na(locality))
locality_non_na <- all_above_1985 %>% filter(!is.na(locality))
#Filter by unique locality when present and if not by unique coordinate
locality_na_filtered <- locality_na %>% distinct(scientificName, year, month,day, long, lat,  .keep_all = T)
locality_non_na_filtered <- locality_non_na %>% distinct(scientificName, year, month,day,locality,  .keep_all = T)
#bind both
all_unique_event <- rbind(locality_na_filtered, locality_non_na_filtered)

#############-
#THIRD FILTER----
#############-
#Third filter lat/long with high precision (3 decimals?)
#Function from stackoverflow to count decimal positions
#https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
count_decimals = function(x) {
    #length zero input
    if (length(x) == 0) return(numeric())
    
    #count decimals
    x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
    x_int = floor(x) %>% abs() %>% nchar()
    x_nchr = x_nchr - 1 - x_int
    x_nchr[x_nchr < 0] = 0
    
    x_nchr
}

#Create a col with number of decimals fro lat and long
all_unique_event$long_decimals <- as.numeric(all_unique_event$long) %>% count_decimals()
all_unique_event$lat_decimals <- as.numeric(all_unique_event$lat) %>% count_decimals()
#Now filter by minimum 3 decimals
all_unique_event_3_decimals <- all_unique_event %>% filter(long_decimals > 2 & lat_decimals > 2)
#Seems to work fine

#############-
#FOURTH FILTER----
#############-
#This filter is done in function of the number of levels per species
#First explore levels
s <- data.frame(all_unique_event_3_decimals %>% 
    group_by(scientificName) %>%
    summarise(no_rows = length(scientificName)))
#The dataframe needs a bit of cleaning

#Delete this cases without species names
all_unique_event_3_decimals <- all_unique_event_3_decimals %>%
    filter(!str_detect(scientificName, regex("\\BOLD", ignore_case = TRUE)))

#Rename species, gbif is giving the synonim
all_unique_event_3_decimals$scientificName[all_unique_event_3_decimals$scientificName=="Andrena sabulosa (Scopoli, 1763)"] <- "Andrena carantonica Pérez, 1902"
#Select above species with above 50 records
all_above_50 <- all_unique_event_3_decimals %>% 
    group_by(scientificName) %>% filter(n() >= 50) %>% ungroup()

#Create a col with Species name that will match the original name
all_above_50$Species_name <- paste(word(all_above_50$scientificName, 1), word(all_above_50$scientificName, 2))
#Filter out now apis from this column
all_above_50 <- all_above_50 %>% filter(!Species_name=="Apis mellifera")

#############-
#FITH FILTER----
#############-
#Wide geograhical distribution
#I'm going to explore this graphically
#We are avoiding "endemic" species
#At the moment there are 32 species
#Load libraries to create map of interest
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

#Convert long to numeric
all_above_50$long <- as.numeric(all_above_50$long)
str(all_above_50)

#Create for loop and store all plots on a folder
spp <- unique(all_above_50$Species_name)

#for(i in spp){

#temp_plot <- ggplot(euro_map) +
#geom_sf(aes(fill = CNTR_CODE, group=CNTR_CODE), color = NA, alpha = 1)+ 
#guides(fill="none") +
#geom_point(data = all_long_lat,aes(long, lat),size = 0.15, stroke = 0, shape = 16) +
#geom_sf(fill = "transparent", color = "gray20", size = 0.25, 
#data = . %>% group_by(CNTR_CODE) %>% summarise()) +
#ylab("Latitude") + xlab("Longitude")+
#geom_sf(data= nuts2.sf,aes(fill = NA, group=CNTR_CODE), color = NA, alpha = 0.3)+
#coord_sf(xlim = c(-5, 20), ylim = c(46, 60)) +
#theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
#size = 0.5), panel.background = element_rect(fill = "aliceblue"),
#panel.border = element_rect(colour = "black", fill=NA, size=1)) +
#geom_point(data = all_above_50 %>% filter(Species_name==i),aes(lat, long),
#size = 1, stroke = 0, shape = 16)+ggtitle(i)
#
#ggsave(temp_plot, file=paste0("Image_bee_distribution/europe/plot_", i,".png"), width = 14, height = 10, units = "cm")

#}

#All species have approximately an homogeneous distribution

#############-
#SAVE DATA----
#############-

colnames(all_above_50)
#Select cols of interest
all_above_50_europe <- all_above_50 %>% select(c("Species_name", "recordedBy", "identifiedBy", "sex","long", "lat","day", "month", "year", 
"locality", "Country", "Continent"))  


