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
d <- read.csv("Data/Especies_para_buscar.csv", row.names = 1)
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
dat <- NULL

for(i in gbif_id$key_number){
    temp <- occ_search(taxonKey= i, 
                       return='data', 
                       hasCoordinate=TRUE,
                       hasGeospatialIssue=FALSE,
                       limit=10000, #safe threshold based on rounding up counts above
                       #country = c(spain_code, portugal_code),
                       fields = c('scientificName', 'decimalLatitude',
                                  'decimalLongitude', 
                                  'family','genus', 'species',
                                  'year', 'month', 'day', 'recordedBy',
                                  'identifiedBy', 'sex', 'stateProvince', 
                                  'locality'))
    
    
    dat <- rbind(dat, as.data.frame(temp$data))
    
    
}

colnames(temp$data)

#Delete first row with NA's that was created to add the data after the loop
dat <- dat[!is.na(dat$species),]
#Check number of levels per species
info <- dat %>% 
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

points = cbind(dat$decimalLongitude, dat$decimalLatitude)
#Include column of continent on the dataset
dat$Continent <- coords2continent(points)

#Filter out by continent
dat <- dat %>% filter(Continent %in% c("Europe", "North America"))
levels(factor(dat$Continent))

#Save data
write.csv(dat, "Data/gbif_data.csv")

#####################---
#Explore graphically----
#####################---
#read data to avoid running all again
d <- read.csv("Data/gbif_data.csv")

#Load worldmap
world <- map_data("world")
#Plot spatial data
ggplot() + geom_map(data = world, map = world,
        aes(long, lat, map_id = region),
        color = "white", fill = "lightgray", size = 0.05) +
        geom_point(data = d,aes(decimalLongitude, decimalLatitude),
        alpha = 0.7, size = 0.1) 


#Separate by continent
namerica <- d %>% filter(Continent %in% c("North America"))
europe <- d %>% filter(Continent %in% c("Europe"))

#Plot north america spatial data
ggplot() + geom_map(data = world, map = world,
    aes(long, lat, map_id = region), color = "white", 
    fill = "lightgray", size = 0.1) +
    geom_point(data = namerica,aes(decimalLongitude, decimalLatitude),
    alpha = 0.7, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-120, -50), ylim = c(0, 70), expand = FALSE)

#Plot europe spatial data
ggplot() + geom_map(data = world, map = world,
   aes(long, lat, map_id = region), color = "white", 
   fill = "lightgray", size = 0.1) +
    geom_point(data = europe,aes(decimalLongitude, decimalLatitude),
    alpha = 0.7, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-15, 45), ylim = c(30, 75), expand = FALSE)


