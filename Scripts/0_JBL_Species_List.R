#Genereate species list

#Load libraries----
library(tidyverse)

#Data Preparation----
#Load data
brains.it <- read.csv("Raw_data/brains.it.csv", dec = ",")

#Clean data
species.brains1 = brains.it %>% 
filter(!ID == "F33") %>%  #Dasypoda visnaga's brain was conserved in ethanol, not formol
rename(Brain.weight = Brain.Weight..mg.) %>% #Rename var
mutate(Brain.IT = (Brain.weight/IT)) %>% #create new col
dplyr::select(Species, IT, Brain.weight, Brain.IT) %>% #select cols of interest
na.omit() #filter out na's

#Check levels
s = species.brains1 %>% distinct(Species)

#Fix one species name
species.brains1$Species[species.brains1$Species=="Lasioglossum dialictus spp"] <- "Lasioglossum dialictus" 
#Delete Species with sp. 
species.brains1 <- filter(species.brains1, !grepl(" sp.",Species))
#Some more typos
species.brains1$Species[species.brains1$Species=="Agaposemon sericeus"] <- "Agapostemon sericeus"
species.brains1$Species[species.brains1$Species=="Rhodantidium sticticum"] <- "Rhodanthidium sticticum"
species.brains1$Species[species.brains1$Species=="Anthopora plumipes"] <- "Anthophora plumipes"
#Filter out Apis mellifera
species.brains1 <- species.brains1 %>% filter(!Species=="Apis mellifera")    
    
#Check and clean outliers
#Andrena barbilabris
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Andrena barbilabris") & (Brain.weight==0.296)))
#Andrena dunningi
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Andrena dunningi") & (Brain.weight==0.600)))
#Augochlorella aurata
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Augochlorella aurata") & (Brain.weight==0.705)))
#Bombus muscorum
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Bombus muscorum") & (Brain.weight==3.585)))

#Xylocopa cantabrita
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Xylocopa cantabrita") & (Brain.weight==5.938)))

#Bombus pascuorum
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Bombus pascuorum") & (Brain.weight==4.937)))
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Bombus pascuorum") & (Brain.weight==3.932)))
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Bombus pascuorum")&(Brain.weight==3.871)))
#Bombus pratorum
species.brains1<-subset(species.brains1, 
                        subset = !((Species == "Bombus pratorum") & (Brain.weight==3.626)))
species.brains1<-subset(species.brains1, 
                        subset = !((Species == "Bombus pratorum") & (Brain.weight==3.278)))
species.brains1<-subset(species.brains1, 
                        subset = !((Species == "Bombus pratorum") & (Brain.weight==3.121)))
species.brains1<-subset(species.brains1, 
                        subset = !((Species == "Bombus pratorum") & (Brain.weight==1.070)))
species.brains1<-subset(species.brains1, 
                        subset = !((Species == "Bombus pratorum") & (Brain.weight==1.300)))
#Bombus terrestris
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Bombus terrestris") & (Brain.weight==5.838)))
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Bombus terrestris") & (Brain.weight==5.564)))
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Bombus terrestris") & (Brain.weight==6.370)))
#Bombus hortorum
species.brains1 <- subset(species.brains1, subset = !((Species == "Bombus hortorum")))
#Bombus lapidarius
species.brains1 <- subset(species.brains1, subset = !((Species == "Bombus lapidarius")))
#Bombus bimaculatus
species.brains1 <- subset(species.brains1, subset = !((Species == "Bombus bimaculatus")))
#Halictus ligatus
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Halictus ligatus") & (Brain.weight==0.395)))
#Osmia caerulescens
species.brains1 <- subset(species.brains1, 
                          subset = !((Species == "Osmia caerulescens") & (Brain.weight==1.530)))
#Xylocopa virginica
species.brains1 <-subset(species.brains1, 
                         subset = !((Species == "Xylocopa virginica") & (Brain.weight==6.613)))
species.brains1 <-subset(species.brains1, 
                         subset = !((Species == "Xylocopa virginica") & (Brain.weight==5.787)))

#Create dataframe with average measurements of IT and brain weight
weights.mean <- data.frame(aggregate(Brain.weight ~ Species, data = species.brains1, FUN = mean))
IT.mean <- data.frame(aggregate(IT ~ Species, data = species.brains1, FUN = mean))
wit.mean <- merge(weights.mean, IT.mean)

#Linearizamos y añadimos la variable de residuales general de cada especie
#now is one value per species
wit.mean = wit.mean %>% 
    mutate(log_brain_weight = log(Brain.weight)) %>% 
    mutate(log_it = log(IT))    
#Check scatter
ggplot(wit.mean, aes(x= IT, y = Brain.weight)) + geom_point()
ggplot(wit.mean, aes(x= log_it, y = log_brain_weight)) + geom_point()
#Model with phylo to extract residuals
m1 <-  brms::brm(log_brain_weight ~ log_it , data = wit.mean, family=gaussian(), iter=3000, 
                 warmup = 1000)
pp_check(m1)
#Extract residuals (estimates) and overwrite old ones
wit.mean$residuals = residuals(m1)[,1]

#Check also residuals with lm 
wit.mean_trial = wit.mean
m2 <- lm(log_brain_weight ~ log_it , data = wit.mean_trial)
wit.mean_trial$residuals2 <- residuals(m2)
cor.test(wit.mean_trial$residuals,wit.mean_trial$residuals2)

#Filter out non-species
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena sp 3")))
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Megascolia sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Halictus sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Eucera sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Dasypoda sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Coelioxys sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena (Chlorandrena)")))
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena (Chrysandrena)")))

#Check species
wit.mean$Species
#Save species to search for their occurrence data
write.csv(wit.mean$Species, "Data/Especies_para_buscar.csv")
#Save data to merge in scripts number 8 for analysis
write_csv(wit.mean, "Data/brain_weight_data.csv")

