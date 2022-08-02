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
    select(Species, IT, Brain.weight, Brain.IT) %>% #select cols of interest
    na.omit() #filter out na's

#Check outliers
boxplot(Brain.weight~Species,data = species.brains1, las= 2,cex.axis = 0.6)

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
plot(Brain.weight ~ IT, data=wit.mean)
abline(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(log(Brain.weight) ~ log(IT), data=wit.mean))
lm.data.means<-lm(log(Brain.weight) ~ log(IT), data=wit.mean)
lm.data.means$residuals
wit.mean$residuals <- lm.data.means$residuals

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
