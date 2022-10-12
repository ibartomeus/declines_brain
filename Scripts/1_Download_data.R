#In this script we download data from GBIF----

#Load libraries
library(stringr) #data cleaning v.1.4.0
library(dplyr) #data cleaning v.1.0.7
library(rgbif) #online database v.3.6.0
library(ggplot2) #plotting v.3.3.5
library(sp) #manipulate coordinates v.1.4-6
library(rworldmap) #plot worldmap v.1.3-6
library(sf) #scale worldmap v 1.0-5

#Read data
d <- read.csv("Data/Processing/Especies_para_buscar.csv", row.names = 1)
#Rename cols
colnames(d) <- c("Species")
#Check levels
levels(unique(factor(d$Species)))
#Fix one species name
d$Species[d$Species=="Lasioglossum dialictus spp"] <- "Lasioglossum dialictus" 
#Delete Species with sp. 
d <- filter(d, !grepl(" sp.",Species))
#Some more typos
d$Species[d$Species=="Agaposemon sericeus"] <- "Agapostemon sericeus"
d$Species[d$Species=="Rhodantidium sticticum"] <- "Rhodanthidium sticticum"
d$Species[d$Species=="Anthopora plumipes"] <- "Anthophora plumipes"
#Filter out Apis mellifera
d <- d %>% filter(!Species=="Apis mellifera")
#For doing trials
#lev_trial <- levels(factor(d$Species))[6:15]
#d <- d %>% filter(Species %in% lev_trial)


########################---
#Download data from GBIF----
########################---

#First create species key (some long numbers that are needed to download the data) 
#For loop to do it for all species
i <- NULL
gbif_id <- list()

for(i in d$Species){
    
    j <- gsub(" ", "_", i)
    gbif_id[[j]] <- name_backbone(name=i, rank = "species")$usageKey
}

#convert list to dataframe
gbif_id <- data.frame(unlist(gbif_id))
#rename col
colnames(gbif_id) <- "key_number"

#Download data
temp <- NULL

#Add check to add missing vars
library(cleanR)
check <-  data.frame(scientificName = NA, decimalLatitude = NA,
                     decimalLongitude = NA,
                     family = NA, genus = NA, species = NA,
                     year = NA, month = NA, day = NA, recordedBy = NA,
                     identifiedBy = NA, sex = NA,  stateProvince = NA,
                     locality = NA,Country= NA, references=NA, datasetKey=NA)

check <- define_template(check, NA)

cols <-  c("scientificName", "decimalLatitude",
                   "decimalLongitude",
                   "family", "genus", "species" ,
                   "year", "month", "day", "recordedBy",
                   "identifiedBy",  "stateProvince",
                   "locality","Country", "references", "datasetKey")

dat <-  data.frame(scientificName = NA, decimalLatitude = NA,
                     decimalLongitude = NA,
                     family = NA, genus = NA, species = NA,
                     year = NA, month = NA, day = NA, recordedBy = NA,
                     identifiedBy = NA, sex = NA,  stateProvince = NA,
                     locality = NA, Country=NA, references=NA, datasetKey=NA)


#Countries of interest with ISO codes
countries <- c("US", "DE", "GB", "LU", "BE", "NL", "DK") 

for(i in gbif_id$key_number){
    temp <- occ_data(taxonKey= i, 
                       year="1985, 2022",
                       hasCoordinate=TRUE,
                       hasGeospatialIssue=FALSE,
                       limit=100000, 
                       country = c("US", "DE", "GB", "LU", "BE", "NL", "DK"))
    
    #Convert to dataframe and add cols if necessary
    temp_US <- as.data.frame(temp$US$data) 
    temp_US = temp_US %>% select(any_of(cols))
    temp_DE <- as.data.frame(temp$DE$data)
    temp_DE = temp_DE %>% select(any_of(cols)) 
    temp_GB <- as.data.frame(temp$GB$data) 
    temp_GB = temp_GB %>% select(any_of(cols))
    temp_LU <- as.data.frame(temp$LU$data) 
    temp_LU = temp_LU %>% select(any_of(cols))
    temp_BE <- as.data.frame(temp$BE$data) 
    temp_BE = temp_BE %>% select(any_of(cols))
    temp_NL <- as.data.frame(temp$NL$data) 
    temp_NL = temp_NL %>% select(any_of(cols))
    temp_DK <- as.data.frame(temp$DK$data) 
    temp_DK = temp_DK %>% select(any_of(cols))
    
    temp_US <- add_missing_variables(check, temp_US)
    temp_DE <- add_missing_variables(check, temp_DE)
    temp_GB <- add_missing_variables(check, temp_GB)
    temp_LU <- add_missing_variables(check, temp_LU)
    temp_BE <- add_missing_variables(check, temp_BE)
    temp_NL <- add_missing_variables(check, temp_NL)
    temp_DK <- add_missing_variables(check, temp_DK)
    
    dat <- rbind(dat, temp_US, temp_DE, temp_GB, temp_LU,
                 temp_BE, temp_NL, temp_DK)
    
    
}

#Example of citation trhough dataset key
#gbif_citation(x='50c9509d-22c7-4a22-a47d-8c48425ef4a7')

write.csv(dat, file=gzfile("Data/gbif_data_1.csv.gz"),row.names=FALSE)

#delete first row with NA on scientific name
all <- dat %>% filter(!is.na(dat$scientificName))
#There are some genus level info, delete
all <- all %>% filter(!is.na(all$species))

#Check number of levels per species
info <- all %>% 
    group_by(species) %>%
    summarise(no_rows = length(species))

#Add continent manually gbif seems to have many NA's on this field
#Get continents
# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees

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

points = cbind(all$decimalLongitude, all$decimalLatitude)
#Include column of continent on the dataset
all$Continent <- coords2continent(points)

#Filter out by continent
all <- all %>% filter(Continent %in% c("Europe", "North America"))
levels(factor(all$Continent))


#Merge to have the raw species names in the dataset (just in case)
gbif_id$Raw_spp <- rownames(gbif_id)
gbif_id$Raw_spp <- gsub("_", " ", gbif_id$Raw_spp)
gbif_id$Raw_spp_1 <- rownames(gbif_id)
gbif_id$Raw_spp_1 <- gsub("_", " ", gbif_id$Raw_spp_1)

#1st
gbif_id$Raw_spp[gbif_id$Raw_spp=="Psithyrus vestalis"] <- "Bombus vestalis"
gbif_id$Raw_spp_1[gbif_id$Raw_spp_1=="Psithyrus vestalis"] <- "Bombus vestalis"
#2nd
gbif_id$Raw_spp[gbif_id$Raw_spp=="Chelostoma philadephi"] <- "Chelostoma philadelphi"
gbif_id$Raw_spp_1[gbif_id$Raw_spp_1=="Chelostoma philadephi"] <- "Chelostoma philadelphi"
#3rd
gbif_id$Raw_spp[gbif_id$Raw_spp=="Heriades carinatus"] <- "Heriades carinata"
gbif_id$Raw_spp_1[gbif_id$Raw_spp_1=="Heriades carinatus"] <- "Heriades carinata"
#4th
gbif_id$Raw_spp[gbif_id$Raw_spp=="Heriades rubicola"] <- "Heriades rubicolus"
gbif_id$Raw_spp_1[gbif_id$Raw_spp_1=="Heriades rubicola"] <- "Heriades rubicolus"
#5th
gbif_id$Raw_spp[gbif_id$Raw_spp=="Heriades rubicola"] <- "Heriades rubicolus"
gbif_id$Raw_spp_1[gbif_id$Raw_spp_1=="Heriades rubicola"] <- "Heriades rubicolus"
#There other levels but don't think are in our species list

#Merege in order to have raw names
all_1 <- merge(all, gbif_id, by.x="species", by.y="Raw_spp_1", all.x=TRUE)

#Exclude the ones that we don´t have
all_1 <- all_1 %>% filter(!is.na(Raw_spp))

#Check levels
info <- all_1 %>% 
    group_by(Raw_spp) %>%
    summarise(no_rows = length(Raw_spp))
#Seems ok!
#Save data
write.csv(all_1, file=gzfile("Data/gbif_data_final.csv.gz"),row.names=FALSE)



