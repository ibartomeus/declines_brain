######-
#In this script we prepare USA data for analysis
#We take 5 key decisions in data cleaning
#1)Filter records above 1987 (this is decided based on the GIS data available)
#2)Filter by unique capture event
#3)Filter by minimum of 3 decimals on coordinates
#4)Filter number of levels per species (minimum N=50)
#5)Filter by wide geographical distribution
######-

#Load libraries
library(data.table)
library(tidyverse)

#Read data
all_east_coast <- data.frame(fread("Data/Usa_data/usa_all_long_lat.csv.gz"))
#Fix colname position
names(all_east_coast)[1:(ncol(all_east_coast)-1)] <- names(all_east_coast)[2:ncol(all_east_coast)]
all_east_coast[, ncol(all_east_coast)] <- NULL
all <- all_east_coast

#############-
#FIRST FILTER----
#############-
#First filter all records above 1987
all_above_1987 <- all %>% filter(year>1987)

#############-
#SECOND FILTER----
#############-
#Second filter unique capture event based on: year, month, day, locality
all_unique_event <- all_above_1987 %>% distinct(scientificName, year, month,day,stateProvince,locality,  .keep_all = T)

#Second filter unique capture event based on: year, month, day, locality
locality_na <-  all_above_1987 %>% filter(is.na(locality))
locality_non_na <- all_above_1987 %>% filter(!is.na(locality))
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

#Create a col with Species name that will match the original name
all_unique_event_3_decimals$Species_name <- paste(word(all_unique_event_3_decimals$scientificName, 1), word(all_unique_event_3_decimals$scientificName, 2))

#Checking quickly if there are more synonyms on the dataset
#d <- read.csv("Data/Especies_para_buscar.csv", row.names = 1)
#b <- data.frame(raw_spp=unique(factor(d$x)))
#a <- data.frame(gbif_spp=unique(factor(all_unique_event_3_decimals$Species_name)),gbif_spp=unique(factor(all_unique_event_3_decimals$Species_name)))
#joined_df <- merge(a, b, by.x = "gbif_spp", by.y = "raw_spp", all = T)

#Select above species with above 50 records
all_above_50 <- all_unique_event_3_decimals %>% 
    group_by(scientificName) %>% filter(n() >= 50) %>% ungroup()

#Filter out now apis from this column
all_above_50 <- all_above_50 %>% filter(!Species_name=="Apis mellifera")


#############-
#FITH FILTER----
#############-
#Wide geograhical distribution
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

#Create for loop and store all plots on a folder
spp <- unique(all_above_50$Species_name)

for(i in spp){

#Now create for loop for the different species 
temp_plot <- ggplot() + geom_map(data = world, map = world,aes(long, lat, map_id = region), color = "white", 
fill = "lightgray", size = 0.1) +geom_map(data = usa_states_1, map = usa_states_1,
aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
size = 0.1) + geom_map(data = ny_metropolitan_1, 
map = ny_metropolitan_1,aes(long, lat, map_id = region, fill=region, group = group), 
size = 0.1) +  scale_fill_manual(values =c("#85BEDC", "#DEB478", "#CCB62F", "#115896",
"#A1B654", "#CD8862", "#CABEE9","#D04E59","#C582B2"))+guides(fill=FALSE) +
theme(panel.grid.major = element_line(color = gray(0.5), 
linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"),
panel.border = element_rect(colour = "black", fill=NA, size=1)) +
coord_sf(xlim = c(-80, -69), ylim = c(38, 45.5)) + ylab("Latitude")+xlab("Longitude")+
geom_point(data = all_above_50 %>% filter(Species_name==i),aes(long, lat),
size = 1, stroke = 0, shape = 16)+ggtitle(i)

ggsave(temp_plot, file=paste0("Image_bee_distribution/usa/plot_", i,".png"), width = 14, height = 10, units = "cm")

}

#All species have approximately an homogeneous distribution

#############-
#SAVE DATA----
#############-

#Save
write.csv(all_above_50, file=gzfile("Data/Usa_data/all_above_50_usa.csv.gz"),row.names=FALSE)


