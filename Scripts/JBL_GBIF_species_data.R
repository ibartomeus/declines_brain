#In this script we download data from GBIF for 133 bee species----

#Load libraries
library(stringr)
library(dplyr)
library(rgbif)

#Read data
d <- read.csv("Data/Especies_para_buscar.csv", row.names = 1)
#Rename cols
colnames(d) <- c("Species")

#Check levels
levels(unique(factor(d$Species)))

#Fix one species name
d$Species[d$Species=="Lasioglossum dialictus spp"] <- "Lasioglossum dialictus" 
#Delete Species with sp. 
d <- filter(d, !grepl(" sp.",Species))

d <- filter(d, !Species==)

#Some more typos
d$Species[d$Species=="Agaposemon sericeus"] <- "Agapostemon sericeus"
d$Species[d$Species=="Rhodantidium sticticum"] <- "Rhodanthidium sticticum"
d$Species[d$Species=="Anthopora plumipes"] <- "Anthophora plumipes"

#Filter out Apis mellifera
d <- d %>% filter(!Species=="Apis mellifera")

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

#Create template to store results from loop
dat <-  data.frame(scientificName = NA, decimalLatitude = NA,
                   decimalLongitude = NA,
                   family = NA, genus = NA, species = NA,
                   year = NA, month = NA, day = NA, recordedBy = NA,
                   identifiedBy = NA, sex = NA, stateProvince = NA,
                   locality = NA, continent= NA)
#Download data
temp <- NULL
for(i in gbif_id$key_number){
    temp <- occ_search(taxonKey= i, 
                       return='data', 
                       hasCoordinate=TRUE,
                       hasGeospatialIssue=FALSE,
                       start=500,
                       limit=10000, #safe threshold based on rounding up counts above
                       #country = c(spain_code, portugal_code),
                       fields = c('scientificName','name', 'decimalLatitude',
                                  'decimalLongitude', 
                                  'family','genus', 'species',
                                  'year', 'month', 'day', 'recordedBy',
                                  'identifiedBy', 'sex', 'stateProvince', 
                                  'locality', 'continent'))
    
   # temp$data <- temp$data[,c('scientificName','decimalLatitude',
   #                       'decimalLongitude', 
   #                       'family','genus', 'species',
   #                       'year', 'month', 'day', 'recordedBy',
   #                       'identifiedBy', 'sex',  'stateProvince', 
   #                       'locality')]
    
    dat <- rbind(dat, as.data.frame(temp$data))
    
    
}

#Delete first row with NA's that was created to add the data after the loop
dat <- dat[!is.na(dat$species),]

#Check number of levels per species
info <- dat %>% 
    group_by(species) %>%
    summarise(no_rows = length(species))

#At the moment there is a maximum of 7000 records per species
#change it?

#Save data
#write.csv(dat, "Data/gbif_data.csv")

d <- read.csv("Data/gbif_data.csv")

library(ggplot2)

world <- map_data("world")
str(d)
ggplot() +
    geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "white", fill = "lightgray", size = 0.1) +
    geom_point(
        data = d,
        aes(decimalLongitude, decimalLatitude),
        alpha = 0.7, size = 0.1) 



MainStates <- map_data("state")
ggplot() + 
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
 color="black", fill="lightblue")+   geom_point(
     data = d,
     aes(decimalLongitude, decimalLatitude),
     alpha = 0.7, size = 0.1) + xlim(-120,0) +ylim(0,60)
