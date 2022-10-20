#In this script we download data from GBIF----
#The download is done with the help of rgbif and the function occ_download
#Quite fast but we need a gbif account!
#To reproduce our analysis, 
#Skip this part and use directly the downloaded data from the DOI
#https://doi.org/10.15468/dl.re5wwt

#Load libraries
library(stringr) 
library(dplyr) 
library(rgbif) 
library(sp) 
library(rworldmap) 
library(sf) 
library(openssl)
library(usethis) 

#Here we need to add our credentials to download the data
usethis::edit_r_environ()

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
#lev_trial <- levels(factor(d$Species))[1:1]
#d <- d %>% filter(Species %in% lev_trial)

#Save species list

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

#Countries of interest with ISO codes
countries <- c("US", "DE", "GB", "LU", "BE", "NL", "DK") 

#Download data
occ_download(pred_in("taxonKey",c(gbif_id$key_number)),  pred_in("country", c("US", "DE", "GB", "LU", "BE", "NL", "DK")))

#The data is downloaded through my gbif account
#Here is the final data: https://doi.org/10.15468/dl.re5wwt 
#Last accesed 10/20/2022 