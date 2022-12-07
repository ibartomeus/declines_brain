#In this script we process the data downloaded from GBIF: https://doi.org/10.15468/dl.5s5kuf
#We select cols of interest with georreferenced information

#Notes: 
#1)this is a very large file that can be downloaded from the DOI above
#As a consequence, this file is not saved at the repo
#2) The data has been downloaded by GBIF with the help of occ_dwonload()
#However, we also have downloaded the data with occ_search()
#Because, the second option takes very long to download 
#we have decided that the 1st one is the way to go

#Load libraries
library(dplyr) 
library(rworldmap) 
library(data.table) 
library(tidyr)
library(sp)
library(ggplot2)

#Read data, it takes a while
d = fread("Data/occurrence.txt")
#Check colnames
colnames(d)
#Select cols of interest
main_cols = d %>% select("scientificName", "family", "genus", "species","speciesKey",
             "recordedBy", "recordedByID", "identifiedBy", "year", "month", "day",
             "decimalLatitude", "decimalLongitude", "stateProvince", 
             "locality","countryCode", "references", "datasetKey", "institutionCode",
             "bibliographicCitation")

main_cols_georeferenced = main_cols %>% 
drop_na("decimalLatitude", "decimalLongitude") %>% 
filter(year>1984)


coords2continent = function(points) 
{  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    #indices$continent   # returns the continent (6 continent model)
    indices$REGION   # returns the continent (7 continent model)
    #indices$ADMIN  #returns country name
    #indices$ISO3 # returns the ISO3 code 
}

points = cbind(main_cols_georeferenced$decimalLongitude, main_cols_georeferenced$decimalLatitude)
#Include column of continent on the dataset
main_cols_georeferenced$Continent <- coords2continent(points)

#Filter out by continent
all = main_cols_georeferenced %>% filter(Continent %in% c("Europe", "North America"))
#Check cols and if species have NA's
colnames(all)
sum(is.na(all$species))

#Now it seems to download more levels (species)
#Filter just the scientific names of interest 
#Read data with species 
data <- read.csv("Data/Processing/Especies_para_buscar.csv", row.names = 1)
#Species data ready to extract species names and filter
spp =  data$x
all_subset = all %>% filter(species %in% spp)

#Check levels
info <- all_subset %>% 
    group_by(species) %>%
    summarise(no_rows = length(species))
#Seems ok!
#Save data
write.csv(all_subset, file=gzfile("Data/gbif_data_final.csv.gz"),row.names=FALSE)
