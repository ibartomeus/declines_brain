
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

#Now try to download data from GBIF
#First start with an example without looping

Agaposemon_sericeus <- name_backbone(name="Agaposemon sericeus", rank = "species")$usageKey

#fetch data
dat <-  data.frame(scientificName = NA, decimalLatitude = NA,
                   decimalLongitude = NA, scientificName = NA,
                   family = NA, genus = NA, species = NA,
                   year = NA, month = NA, day = NA, recordedBy = NA,
                   identifiedBy = NA, sex = NA, stateProvince = NA,
                   locality = NA, coordinatePrecision = NA)

for(i in c(Agaposemon_sericeus)){
    temp <- occ_search(taxonKey= i, 
                       return='data', 
                       hasCoordinate=TRUE,
                       hasGeospatialIssue=FALSE,
                       limit=7000, #safe threshold based on rounding up counts above
                       #country = c(spain_code, portugal_code),
                       fields = c('scientificName','name', 'decimalLatitude',
                                  'decimalLongitude', 'scientificName',
                                  'family','genus', 'species',
                                  'year', 'month', 'day', 'recordedBy',
                                  'identifiedBy', 'sex', 'stateProvince', 
                                  'locality', 'coordinatePrecision'))
    
    }


unique(temp$data$stateProvince)


