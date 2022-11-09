######-
#In this script we prepare Europe data for analysis
#We take 5 key decisions in data cleaning
#1)Filter records above 1985 (this is decided based on the GIS data available)
#2)Filter by unique capture event (locality/coordinate)
#3)Filter by minimum of 3 decimals on coordinates
#4)Filter number of levels per species (minimum N=100)
#5)Filter by wide geographical distribution
######-

#Load libraries
library(data.table)
library(tidyverse)

#Read data 
all_long_lat <- data.frame(fread("Data/Europe_data/urope_all_long_lat.csv.gz"))
#Fix colname position
names(all_long_lat)[1:(ncol(all_long_lat)-1)] <- names(all_long_lat)[2:ncol(all_long_lat)]
all_long_lat[, ncol(all_long_lat)] <- NULL

#The coordinates are a bit messy and the fix is not elegant... lat and long are swapped
all = all_long_lat %>% 
rename(lat1 = lat) %>% 
rename(long1 = long) %>% 
rename(lat = long1) %>% 
rename(long = lat1) %>% 
mutate(lat = gsub(")", "", lat)) 

#############-
#FIRST FILTER----
#############-
#First filter all records above 1985 
all_above_1985 <- all %>% filter(year>1985)

#############-
#SECOND FILTER----
#############-
#Filter by unique coordinate and locality 
coordinate <- all_above_1985 %>% distinct(scientificName, year, month,day, long, lat,  .keep_all = T)
all_unique_event <- coordinate %>% distinct(scientificName, year, month,day,locality,  .keep_all = T)

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
#Now filter by minimum 2 decimals
all_unique_event_3_decimals <- all_unique_event %>% filter(long_decimals > 1 & lat_decimals > 1)
#Seems to work fine

#############-
#FOURTH FILTER----
#############-
#This filter is done in function of the number of levels per species
#First explore levels
s <- data.frame(all_unique_event_3_decimals %>% 
    group_by(species) %>%
    summarise(no_rows = length(species)))
#The dataframe needs a bit of cleaning
#Select species exclusively from our dataset
d <- read.csv("Data/Processing/Especies_para_buscar.csv", row.names = 1)
#spp
spp = d$x
#Species
eu_species=all_unique_event_3_decimals %>% 
    filter(species %in% spp)


#Select above species with above 50 records
all_above_50 <- eu_species %>% 
    group_by(species) %>% filter(n() >= 50) %>% ungroup()

#Check levels again
s <- data.frame(all_above_50 %>% 
                    group_by(species) %>%
                    summarise(no_rows = length(species)))

#############-
#FITH FILTER----
#############-
#Wide geograhical distribution
#I'm going to explore this graphically
#We are avoiding "endemic" species
#At the moment there are 32 species
#Load libraries to create map of interest
#library(ggplot2) #plotting v.3.3.5
#library(dplyr) #data cleaning v.1.0.7
#library(geosphere) #Calculate area within polygon
#library(sf) #gis in r
#library(tidyverse)
#library(maps)
#library(giscoR) #map of europe
#library(sf)
#library(ggspatial) #to plot arrow
#library(patchwork)
#library(ggforce)
##Read data 
#library(data.table)
#all_long_lat <- data.frame(fread("Data/Europe_data/urope_all_long_lat.csv.gz"))
##Fix colname position
#names(all_long_lat)[1:(ncol(all_long_lat)-1)] <- names(all_long_lat)[2:ncol(all_long_lat)]
#all_long_lat[, ncol(all_long_lat)] <- NULL
#euro_map <- st_read("Data/Europe_data/euro_map.shp")
#nuts2.sf <- st_read("Data/Europe_data/euro_nuts2.sf.shp")
##Load worldmap
#world <- map_data("world")
#
##Convert long to numeric
#all_above_50$long <- as.numeric(all_above_50$long)
#
##Create for loop and store all plots on a folder
#spp <- unique(all_above_50$species)
#
#for(i in spp){
#
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
#geom_point(data = all_above_50 %>% filter(species==i),aes(lat, long),
#size = 1, stroke = 0, shape = 16)+ggtitle(i)
#
#ggsave(temp_plot, file=paste0("Data/Image_bee_distribution/europe/plot_", i,".png"), width = 14, height = 10, units = "cm")
#
#}

#All species have approximately an homogeneous distribution

#############-
#SAVE DATA----
#############-

#58 records
unique(as.factor(all_above_50$species))

#Save
write.csv(all_above_50, file=gzfile("Data/Europe_data/all_above_50_europe.csv.gz"),row.names=FALSE)

