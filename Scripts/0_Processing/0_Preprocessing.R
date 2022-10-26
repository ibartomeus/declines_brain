#Preprocessing----
#In this script we fix typos and rename species when neccesary to their synonyms
#The data was gathered and measured by MA Collado during his PhD thesis
library(tidyverse)
#Load data
brains.it <- read.csv("Raw_data/brains.it.csv", dec = ",")

#Reshaping data and some cleaning
species.brains = brains.it %>% 
filter(!ID == "F33") %>%  #Dasypoda visnaga's brain was conserved in ethanol, not formol
rename(Brain.weight = Brain.Weight..mg.) %>% #Rename var
mutate(Brain.IT = (Brain.weight/IT)) %>% #create new col
dplyr::select(Species, IT, Brain.weight, Brain.IT, Sex) %>% #select cols of interest
na.omit() #filter out na's

#Check levels
s = species.brains %>% distinct(Species)

#Fix species names
species.brains$Species[species.brains$Species=="Lasioglossum dialictus spp"] <- "Lasioglossum dialictus"
species.brains$Species[species.brains$Species=="Andena vicina"] <- "Andrena vicina"
species.brains$Species[species.brains$Species=="Chelostoma philadephi"] <- "Chelostoma philadelphi"
#Delete Species with sp. 
species.brains <- filter(species.brains, !grepl(" sp.",Species))
#Some more typos
species.brains$Species[species.brains$Species=="Agaposemon sericeus"] <- "Agapostemon sericeus"
species.brains$Species[species.brains$Species=="Rhodantidium sticticum"] <- "Rhodanthidium sticticum"
species.brains$Species[species.brains$Species=="Anthopora plumipes"] <- "Anthophora plumipes"
species.brains$Species[species.brains$Species=="Anthopora dispar"] <- "Anthophora dispar"

#Two species are best to rename it with synonym
species.brains$Species[species.brains$Species=="Flavipanurgus venustus"] <- "Panurgus venustus"
species.brains$Species[species.brains$Species=="Psithyrus vestalis"] <- "Bombus vestalis"

#Filter out Apis mellifera
species.brains <- species.brains %>% filter(!Species=="Apis mellifera")    

#Check and clean non-sense brain weights
#Andrena barbilabris
species.brains <- subset(species.brains, 
subset = !((Species == "Andrena barbilabris") & (Brain.weight==0.296)))
#Andrena dunningi
species.brains <- subset(species.brains, 
subset = !((Species == "Andrena dunningi") & (Brain.weight==0.600)))
#Augochlorella aurata
species.brains <- subset(species.brains, 
subset = !((Species == "Augochlorella aurata") & (Brain.weight==0.705)))
#Bombus muscorum
species.brains <- subset(species.brains, 
subset = !((Species == "Bombus muscorum") & (Brain.weight==3.585)))

#Xylocopa cantabrita
species.brains <- subset(species.brains, 
subset = !((Species == "Xylocopa cantabrita") & (Brain.weight==5.938)))

#Bombus pascuorum
species.brains <- subset(species.brains, 
subset = !((Species == "Bombus pascuorum") & (Brain.weight==4.937)))
species.brains <- subset(species.brains, 
subset = !((Species == "Bombus pascuorum") & (Brain.weight==3.932)))
species.brains <- subset(species.brains, 
subset = !((Species == "Bombus pascuorum")&(Brain.weight==3.871)))
#Bombus pratorum
species.brains<-subset(species.brains, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==3.626)))
species.brains<-subset(species.brains, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==3.278)))
species.brains<-subset(species.brains, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==3.121)))
species.brains<-subset(species.brains, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==1.070)))
species.brains<-subset(species.brains, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==1.300)))
#Bombus terrestris
species.brains <- subset(species.brains, 
subset = !((Species == "Bombus terrestris") & (Brain.weight==5.838)))
species.brains <- subset(species.brains, 
subset = !((Species == "Bombus terrestris") & (Brain.weight==5.564)))
species.brains <- subset(species.brains, 
subset = !((Species == "Bombus terrestris") & (Brain.weight==6.370)))
#Bombus hortorum
species.brains <- subset(species.brains, subset = !((Species == "Bombus hortorum")))
#Bombus lapidarius
species.brains <- subset(species.brains, subset = !((Species == "Bombus lapidarius")))
#Bombus bimaculatus
species.brains <- subset(species.brains, subset = !((Species == "Bombus bimaculatus")))
#Halictus ligatus
species.brains <- subset(species.brains, 
subset = !((Species == "Halictus ligatus") & (Brain.weight==0.395)))
#Osmia caerulescens
species.brains <- subset(species.brains, 
subset = !((Species == "Osmia caerulescens") & (Brain.weight==1.530)))
#Xylocopa virginica
species.brains <-subset(species.brains, 
subset = !((Species == "Xylocopa virginica") & (Brain.weight==6.613)))
species.brains <-subset(species.brains, 
subset = !((Species == "Xylocopa virginica") & (Brain.weight==5.787)))


#Filter out non-species
species.brains<-subset(species.brains, subset = !((Species == "Andrena sp 3")))
species.brains<-subset(species.brains, subset = !((Species == "Andrena sp.")))
species.brains<-subset(species.brains, subset = !((Species == "Megascolia sp.")))
species.brains<-subset(species.brains, subset = !((Species == "Halictus sp.")))
species.brains<-subset(species.brains, subset = !((Species == "Eucera sp.")))
species.brains<-subset(species.brains, subset = !((Species == "Dasypoda sp.")))
species.brains<-subset(species.brains, subset = !((Species == "Coelioxys sp.")))
species.brains<-subset(species.brains, subset = !((Species == "Andrena (Chlorandrena)")))
species.brains<-subset(species.brains, subset = !((Species == "Andrena (Chrysandrena)")))

species.brains = species.brains %>%  filter(Sex == "Female") %>% dplyr::select(!Sex)

#Create dataframe with average measurements of IT and brain weight
weights.mean <- data.frame(aggregate(Brain.weight ~ Species, data = species.brains, FUN = mean))
IT.mean <- data.frame(aggregate(IT ~ Species, data = species.brains, FUN = mean))
wit.mean <- merge(weights.mean, IT.mean)

saveRDS(wit.mean, "Data/Processing/wit.mean.rds")


