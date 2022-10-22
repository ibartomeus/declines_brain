######-
#In this script we prepare USA data for analysis
#We take 5 key decisions in data cleaning
#1)Filter records above 1987 (this is decided based on the GIS data available)
#2)Filter by unique capture event
#3)Filter by minimum of 2 decimals on coordinates #Because MA dataset has only 2
#4)Filter number of levels per species (minimum N=100)
#5)Filter by wide geographical distribution
######-

#Note we also add the data used in the thesis of MA Collado.
#This data is processed in the folder Collado_data

#Load libraries
library(data.table)
library(tidyverse)

#Read data
all_east_coast <- data.frame(fread("Data/Usa_data/usa_all_long_lat.csv.gz"))
#Fix colname position
names(all_east_coast)[1:(ncol(all_east_coast)-1)] <- names(all_east_coast)[2:ncol(all_east_coast)]
all_east_coast[, ncol(all_east_coast)] <- NULL
all <- all_east_coast %>% select(!geometry)
#Read MA tehsis data
all_east_coast_thesis <- data.frame(fread("Data/Usa_data/usa_all_long_lat_thesis.csv.gz"))
#Fix colname position
names(all_east_coast_thesis)[1:(ncol(all_east_coast_thesis)-1)] <- names(all_east_coast_thesis)[2:ncol(all_east_coast_thesis)]
all_east_coast_thesis[, ncol(all_east_coast_thesis)] <- NULL
all_thesis <- all_east_coast_thesis 

colnames(all_thesis)

#Select columns of interest
all_thesis = all_thesis %>% 
select(gen_sp, state, lat, long, country, year, month, day, site) %>% 
rename(species = gen_sp, Country = country, stateProvince = state, locality=site) 

#Now mege both datasets
colnames(all_thesis)
colnames(all)

#Rename to original name
all = bind_rows(all, all_thesis)

#############-
#FIRST FILTER----
#############-
#First filter all records above 1987
all_above_1987 <- all %>% filter(year>1987)

#############-
#SECOND FILTER----
#############-
#Filter by unique coordinate and locality 
coordinate <- all_above_1987 %>% distinct(species, year, month,day, long, lat,  .keep_all = T)
all_unique_event <- coordinate %>% distinct(species, year, month,day,locality,  .keep_all = T)

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
all_unique_event_3_decimals <- all_unique_event %>% filter(long_decimals > 1 & lat_decimals > 1)
#Seems to work fine

#############-
#FOURTH FILTER----
#############-
#This filter is done in function of the number of levels per species
#Filter just spp records with same names in our dataset
d <- read.csv("Data/Processing/Especies_para_buscar.csv", row.names = 1)
#spp
spp = d$x
#Species
usa_species=all_unique_event_3_decimals %>% 
filter(species %in% spp)

#Select above species with above 50 records
all_above_50 <- usa_species %>% 
    group_by(species) %>% filter(n() >= 100) %>% ungroup()

#check levels again
s <- data.frame(all_above_50 %>% 
                    group_by(species) %>%
                    summarise(no_rows = length(species)))

colnames(all_above_50)
#Rename lat/lon cols
#all_above_50 = all_above_50 %>% 
#rename(longitude = long) %>% 
#rename(latitude = lat)


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
spp <- unique(all_above_50$species)

world <- map_data("world")

colnames(world)

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
geom_point(data = all_above_50 %>% filter(species==i),aes(long, lat),
size = 1, stroke = 0, shape = 16)+ggtitle(i)

ggsave(temp_plot, file=paste0("Data/Image_bee_distribution/usa/plot_", i,".png"), width = 14, height = 10, units = "cm")

}

#All species have approximately an homogeneous distribution

#############-
#SAVE DATA----
#############-

#Save
write.csv(all_above_50, file=gzfile("Data/Usa_data/all_above_50_usa.csv.gz"),row.names=FALSE)

