#LIBRARIES----
#We are going to need a lot of R packages
library(reshape2)
library(googlesheets)
library(rredlist)
library(MCMCglmm)
library(brms)
library(data.tree)
library('ctv') 
library(ape)
library(stringi)
library(phytools)
library(ggplot2)
library(lme4)
library(sjstats)
library(visreg) 

#DATA LOAD----

getwd()

#This is raw data with just brain weights and intertegular distance
brains.it <- read.csv("Raw_data/brains.it.csv")

#Dasypoda visnaga's brain was conserved in ethanol, not formol, ask Oscar Aguado why, we remove
brains.it<-subset(brains.it, subset = !(brains.it$ID)=="F33")
which(brains.it$ID=="F33")

#first We need a dataframe of species with brain weight and IT
#We create a "encephalization" variable, just in case, i.e. controlling brain allometry by bee size
brains.it$Encephalization<-brains.it$Brain.Weight..mg./brains.it$IT

#We remove individuals without brain size or IT, we need both
brains.t<-subset(brains.it, subset = (is.na(brains.it$Encephalization)==FALSE))

list.of.species<-as.data.frame(unique(brains.t$Species))
list.of.species



brains.it$Species
brains.it$IT
brains.it$Brain.Weight..mg.
#Creamos un dataframe chiquito para trabajar
species.brains1<-data.frame(brains.it$Species,
                            brains.it$IT,
                            brains.it$Brain.Weight..mg.)
species.brains1<-na.omit(species.brains1)
colnames(species.brains1)<-c("Species", "IT", "Brain.weight")

plot(species.brains1$Brain.weight ~ species.brains1$IT, data = species.brains1)
abline(lm(species.brains1$Brain.weight ~ species.brains1$IT,data = species.brains1), col="pink")
summary(lm(species.brains1$Brain.weight ~ species.brains1$IT,data = species.brains1))

#Linearizing
#Linearizamos, aplicamos log en ambos lados, y extraemos residuales, que es lo que usamos como cerebro
plot(log(species.brains1$Brain.weight) ~ log(species.brains1$IT), data = species.brains1)
abline(lm(log(species.brains1$Brain.weight) ~ log(species.brains1$IT),data = species.brains1), col="pink")
summary(lm(log(species.brains1$Brain.weight) ~ log(species.brains1$IT),data = species.brains1))
text(log(species.brains1$Brain.weight) ~ log(species.brains1$IT), labels=row.names(species.brains1), cex= 0.7, pos=3)
res.extract1<-lm(log(species.brains1$Brain.weight) ~ log(species.brains1$IT), data = species.brains1)
res.extract1$residuals

#Add residuals and encephalization to the dataframe
species.brains1$residuals<-res.extract1$residuals
species.brains1$Brain.IT<-species.brains1$Brain.weight/species.brains1$IT

#We load habitat preference (This is a small list from collado et al 201X, we need more)

habpref <- read.csv("Raw_data/habitat_preference_simplify.csv")





#################
habpref$Species
species.brains1$Brain.weight
species.brains1$IT
species.brains1$residuals
habpref$Habitats.used
habpref$Pasture.and.Crops
habpref$Forests
habpref$Urban

XXX<-merge(habpref,species.brains1)

#consigue este formato
habitat.brain


##################

#DATA FORMATING----
#PHYLOGENETIC TREE CONSTRUCTION---------
#ANALYSIS-----
#FIGURES----