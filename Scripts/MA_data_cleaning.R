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
#DATA CONSTRUCTION------
getwd()
brains.it <- read.csv("Raw_data/brains.it.csv")

#Dasypoda visnaga's brain was conserved in ethanol, not formol
brains.it<-brains.it[-175,]
#List of species with brain weight and IT-----

brains.it$Encephalization<-brains.it$Brain.Weight..mg./brains.it$IT
brains.t<-subset(brains.it, subset = (is.na(brains.it$Encephalization)==FALSE))
list.of.species<-as.data.frame(unique(brains.t$Species))
spain.bees<-subset(brains.t, subset = (brains.t$Country=="Spain"))
list.of.spanish.bees<-unique(spain.bees$Species)
list.of.spanish.bees<-as.data.frame(list.of.spanish.bees)


#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/Raw_data")
#write.csv(list.of.spanish.bees, "list.of.spanish.bees.csv")
#write.csv(list.of.species, "list.of.species.csv")
#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/")
list.of.spanish.bees
list.of.species

#List of innovators-----
# load package
Traits <- read.csv("Data/Traits.csv")

Traits$species
summary(as.factor(Traits$value), maxsum = 1000)
list.of.innovators.f<-subset(Traits, subset = (Traits$value == "Brick" | 
                                                   Traits$value == "Plastic" |
                                                   Traits$value == "Small mammal house" |
                                                   Traits$value == "Birdhouse" |
                                                   Traits$value == "PVC" |
                                                   Traits$value == "Table" |
                                                   Traits$value == "Rockwool" |
                                                   Traits$value == "Plastic table"|
                                                   Traits$value == "Plastic straw" |
                                                   Traits$value == "Polycarbonate" |
                                                   Traits$value == "Paper" |
                                                   Traits$value == "Metal" |
                                                   Traits$value == "latch hole" |
                                                   Traits$value == "Fluff" |
                                                   Traits$value == "Dried paint" |
                                                   Traits$value == "Cardboard"))

list.of.innovators<-unique(list.of.innovators.f$species)
list.of.innovators

#Import scheper trends----

scheper_trends <- read.csv("Raw_data/scheper_trends.csv")
# 1 means not change along time
scheper_trends

#Import red list----

#You need an API token to use this
IUCN_REDLIST_KEY="084775ffa81321631f2583dcd416a81af6fbd73efb60a0eab38e0df10082fe07"


list.of.species

#for (n in 1:nrow(list.of.species)) {
    #tryCatch({temp<-rl_search(as.character(list.of.species[n,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
    #print(temp)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#}

rl_search(as.character(list.of.species[19,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[20,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[26,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[59,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[66,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[71,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[113,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[114,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[121,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[137,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[144,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category

species.temp<-(c(as.character(list.of.species[19,1]),
as.character(list.of.species[20,1]),
as.character(list.of.species[26,1]),
as.character(list.of.species[59,1]),
as.character(list.of.species[66,1]),
as.character(list.of.species[71,1]),
as.character(list.of.species[113,1]),
as.character(list.of.species[114,1]),
as.character(list.of.species[121,1]),
as.character(list.of.species[137,1]),
as.character(list.of.species[144,1])))

category.temp<-c(rl_search(as.character(list.of.species[19,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[20,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[26,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[59,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[66,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[71,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[113,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[114,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[121,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[137,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[144,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category)
species.iucn<-as.data.frame(species.temp)          
category.iucn<-as.data.frame(category.temp)          
iucn.trends<-(cbind(species.iucn,category.iucn))

#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/Raw_data")
#write.csv(iucn.trends, "iucn_trends.csv")
#setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain")

#US habitats----
habitat_preference_US <- read.csv("Raw_data/habitat_preference_US.csv")
habitat_preference_US
m.habitat_preference_US<-melt(habitat_preference_US)
m.habitat_preference_US<-as.data.frame(m.habitat_preference_US)
#We order
m.habitat_preference_US<-m.habitat_preference_US[order(m.habitat_preference_US$Species),]
m.habitat_preference_US<-subset(m.habitat_preference_US, subset = (!(m.habitat_preference_US$variable == "Habitats.used")))
m.habitat_preference_US$habitat.preference<-rep(NA,nrow(m.habitat_preference_US))

for (n in 1:nrow(m.habitat_preference_US)) {
    if(m.habitat_preference_US$value[n]>0.94){m.habitat_preference_US$habitat.preference[n] <- "Yes"}else{
        if(m.habitat_preference_US$value[n]<0.06){
            m.habitat_preference_US$habitat.preference[n] <- "No"    
        }}}
m.habitat_preference_US
na.omit(m.habitat_preference_US)

habitat_preference_US<-na.omit(m.habitat_preference_US)

habitat_preference_US

#Species trends data--------
scheper_trends <- read.csv("Raw_data/scheper_trends.csv")
iucn_trends <- read.csv("Raw_data/iucn_trends.csv")
bartomeus_trends <- read.csv("Raw_data/bartomeus_trends.csv", sep = ";")

colnames(scheper_trends)
colnames(iucn_trends) <-c("X","Species","category.temp")
colnames(bartomeus_trends)

colnames(list.of.species)<-c("Species")

merger1<-merge(list.of.species,scheper_trends)
merger2<-data.frame(merger1$Species, merger1$Change.index)
colnames(merger2)<-c("Species","Scheper_trends")
merger3<-merge(list.of.species, iucn_trends)
merger4<-data.frame(merger3$Species,merger3$category.temp)
colnames(merger4)<-c("Species","IUCN_trends")

merger5<-merge(list.of.species, bartomeus_trends)
merger6<-data.frame(merger5$Species, merger5$Status)
colnames(merger6)<-c("Species","Bartomeus_trends")

merger2$source<-rep("Scheper",nrow(merger2))
merger4$source<-rep("IUCN",nrow(merger4))
merger6$source<-rep("Bartomeus",nrow(merger6))

#Let's standarize
merger2

f.trend2<-vector()
for (n in 1:nrow(merger2)) {
if (merger2$Scheper_trends[n]<1){
    f.trend2[n] = "declining"
}else{
if (merger2$Scheper_trends[n]>1) {
f.trend2[n] = "increasing"    
}else{
f.trend2[n] = "stable"        
}}}
merger2$f.trend<-f.trend2


merger4
f.trend4<-vector()
for (n in 1:nrow(merger4)) {
if (merger4$IUCN_trends[n] == "DD") {
f.trend4[n]<-"unknown"    
}else{
if (merger4$IUCN_trends[n] == "LC") {
f.trend4[n]<-"stable"    
}else{print("Check IUCN_trends")}   
}    
}
merger4$f.trend<-f.trend4

merger6$f.trend<-merger6$Bartomeus_trends

colnames(merger2)<-c("Species","original.trends","source","f.trend")
colnames(merger4)<-c("Species","original.trends","source","f.trend")
colnames(merger6)<-c("Species","original.trends","source","f.trend")

species.trends<-rbind(merger2,merger4,merger6)

species.trends[order(species.trends$Species),]

subset(species.trends, subset = (species.trends$Species == ("Andrena barbilabris"|
                                                                 "Andrena fulva"|
                                                             "Andrena semilaevis"|
                                                             "Bombus bimaculatus"|
                                                                 "Bombus griseocollis"|
                                                                 "Bombus impatiens"|
                                                                 "Bombus jonellus"|
                                                             "Bombus ternarius"|
                                                                 "Bombus vagans"|
                                                                 "Halictus confusus"|
                                                             "Halictus rubicundus"|
                                                                 "Megachile centuncularis"
)))


repeated.trends<-subset(species.trends, subset = (species.trends$Species == "Andrena barbilabris" 
                                 | species.trends$Species == "Andrena fulva"
                                 | species.trends$Species == "Andrena semilaevis"
                                 | species.trends$Species == "Bombus bimaculatus"
                                 | species.trends$Species == "Bombus griseocollis"
                                 | species.trends$Species == "Bombus impatiens"
                                 | species.trends$Species == "Bombus jonellus"
                                 | species.trends$Species == "Bombus ternarius"
                                 | species.trends$Species == "Bombus vagans"
                                 | species.trends$Species == "Halictus confusus"
                                 | species.trends$Species == "Halictus rubicundus"
                                 | species.trends$Species == "Megachile centuncularis"
                                 ))

repeated.trends[order(repeated.trends$Species),]

#Identify the procedence of the repeated species

brains.it[which(brains.it$Species == "Andrena barbilabris"),] #Pick Scheper
brains.it[which(brains.it$Species == "Andrena fulva"),] #Pick Scheper
brains.it[which(brains.it$Species == "Andrena semilaevis"),] #Pick Scheper
brains.it[which(brains.it$Species == "Bombus bimaculatus"),] #Bartomeus
brains.it[which(brains.it$Species == "Bombus griseocollis"),] #Bartomeus
brains.it[which(brains.it$Species == "Bombus impatiens"),] #Bartomeus
brains.it[which(brains.it$Species == "Bombus jonellus"),] #Pick Scheper
brains.it[which(brains.it$Species == "Bombus ternarius"),] #Pick Bartomeus
brains.it[which(brains.it$Species == "Bombus vagans"),] #Pick Bartomeus
brains.it[which(brains.it$Species == "Halictus confusus"),] #Pick anything
brains.it[which(brains.it$Species == "Megachile centuncularis"),] #Pick Scheper
brains.it[which(brains.it$Species == "Halictus rubicundus"),] #Pick Scheper

#Remove the repeated species

sp.t<-subset(species.trends, subset = (!(species.trends$Species == "Andrena barbilabris" & species.trends$source == "Bartomeus")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Andrena fulva" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Andrena semilaevis" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Bombus bimaculatus" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Bombus griseocollis" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Bombus impatiens" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Bombus jonellus" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Bombus ternarius" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Bombus vagans" & sp.t$source == "IUCN")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Halictus confusus" & sp.t$source == "Bartomeus")))
sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Halictus rubicundus" & sp.t$source == "Bartomeus")))

sp.t<-subset(sp.t, subset = (!(sp.t$Species == "Megachile centuncularis" & sp.t$source == "Bartomeus")))
duplicated(sp.t$Species)

sp.t

# write.csv(repeated.trends, "repeated.trends.csv")

#We add brain sizes and ITs
#Dasypoda visnaga's brain was conserved in ethanol, not formol
brains.it<-brains.it[-175,]
brains.it$Species
brains.it$IT
brains.it$Brain.Weight..mg.

species.brains1<-data.frame(brains.it$Species,
                            brains.it$IT,
                            brains.it$Brain.Weight..mg.)
species.brains1<-na.omit(species.brains1)
colnames(species.brains1)<-c("Species", "IT", "Brain.weight")

plot(species.brains1$Brain.weight ~ species.brains1$IT, data = species.brains1)
abline(lm(species.brains1$Brain.weight ~ species.brains1$IT,data = species.brains1), col="pink")
summary(lm(species.brains1$Brain.weight ~ species.brains1$IT,data = species.brains1))

#Linearizing
plot(log(species.brains1$Brain.weight) ~ log(species.brains1$IT), data = species.brains1)
abline(lm(log(species.brains1$Brain.weight) ~ log(species.brains1$IT),data = species.brains1), col="pink")
summary(lm(log(species.brains1$Brain.weight) ~ log(species.brains1$IT),data = species.brains1))
text(log(species.brains1$Brain.weight) ~ log(species.brains1$IT), labels=row.names(species.brains1), cex= 0.7, pos=3)
res.extract1<-lm(log(species.brains1$Brain.weight) ~ log(species.brains1$IT), data = species.brains1)
res.extract1$residuals






#Add residuals and encephalization to the dataframe
species.brains1$residuals<-res.extract1$residuals
species.brains1$Brain.IT<-species.brains1$Brain.weight/species.brains1$IT

colnames(species.trends)
colnames(species.brains1)
brain.it.trends<-merge(species.brains1, sp.t)

bartomeus.numeric.trends<-data.frame(bartomeus_trends$Species,
bartomeus_trends$Estimate,
bartomeus_trends$SE)

colnames(bartomeus.numeric.trends)<-c("Species","Bartomeus.Estimate","Bartomeus.SE")
#RE-DO ALL FROM HERE----------
BTT<-merge(brain.it.trends, bartomeus.numeric.trends, by.x = "Species", all.x = TRUE)

write.csv(BTT, "data/data_to_play_with.csv")
#BY INDIVIDUAL-----
#GLM and GRAPHS----
data1 <- read.csv("data/data_to_play_with.csv")
data1
data.scheper<-subset(data1, subset=(data1$source == "Scheper"))
data.nacho<-subset(data1, subset=(data1$source == "Bartomeus"))

#Encephalization
boxplot(data1$Brain.IT~data1$f.trend, notch=FALSE,data = data1, ylab= "Brain/IT", xlab="Population trends",main="All data")
aggregate(Brain.IT~f.trend, FUN = mean, data = data1)
b.aov<-(aov(Brain.IT~f.trend, data = data1))
TukeyHSD(b.aov)

boxplot(data.scheper$Brain.IT~data.scheper$f.trend, notch=TRUE,data = data.scheper, ylab= "Brain/IT", xlab="Population trends",main="Scheper data")
aggregate(Brain.IT~f.trend, FUN = mean, data = data.scheper)
sch.aov<-(aov(Brain.IT~f.trend, data = data.scheper))
TukeyHSD(sch.aov)
table(data.scheper$f.trend)

str(data.scheper)
data.scheper$original.trends<-as.numeric(as.character(data.scheper$original.trends))
hist(data.scheper$original.trends)
plot(original.trends ~ Brain.IT,data = data.scheper, main= "Scheper data")
abline(glm(original.trends ~ Brain.IT,data = data.scheper, family = gaussian))
summary(glm(original.trends ~ Brain.IT,data = data.scheper, family = gaussian))


str(data.nacho)

boxplot(data.nacho$Brain.IT~data.nacho$f.trend, notch=FALSE,data = data.nacho, ylab= "Brain/IT", xlab="Population trends",main="Bartomeus data")
aggregate(Brain.IT~f.trend, FUN = mean, data = data.scheper)
sch.aov<-(aov(Brain.IT~f.trend, data = data.scheper))
TukeyHSD(sch.aov)
table(data.nacho$f.trend)
hist(data.nacho$Bartomeus.Estimate)
plot(Bartomeus.Estimate ~ Brain.IT,data = data.nacho, main="Bartomeus data") 
abline(glm(Bartomeus.Estimate ~ Brain.IT,data = data.nacho))
summary(glm(Bartomeus.Estimate ~ Brain.IT,data = data.nacho))



#DECLINING / INCRESING ONLY
incdec.data<-subset(data1, subset = (data1$f.trend == "declining" | data1$f.trend == "increasing"))
droplevels(incdec.data$f.trend)
incdec.data$Species<-droplevels(incdec.data$Species)
boxplot(incdec.data$Brain.IT ~ incdec.data$f.trend)


incdec.data$numeric.trend <- NULL
for (n in 1:nrow(incdec.data)) {
    if (incdec.data$f.trend[n] == "declining") {
        incdec.data$numeric.trend[n]= 0
    }else{
        if (incdec.data$f.trend[n] == "increasing") {       
            incdec.data$numeric.trend[n]= 1
        }else{ print("not increasing not declining")}
    }}

incdec.data.scheper<-subset(incdec.data, subset = (incdec.data$source == "Scheper"))
incdec.data.nacho<-subset(incdec.data, subset = (incdec.data$source == "Bartomeus"))

plot(numeric.trend ~ Brain.IT, data = incdec.data, main="", xlab="Brain/IT", ylab = "Declining population/ Increasing population", yaxt='n')
xweight <- seq(0, 2, 0.01)
fit <- glm(f.trend ~ Brain.IT, family = binomial, data = incdec.data)
yweight <- predict(fit, list(Brain.IT = xweight), type="response")
lines(xweight, yweight)

trend.brainit<- glm(f.trend ~ Brain.IT, family = binomial, data = incdec.data)
summary(trend.brainit)


#Residuals
plot(numeric.trend ~ residuals, data = incdec.data, main="", xlab="Brain - Body size residuals", ylab = "Declining population/ Increasing population", yaxt='n')
xweight <- seq(-2, 2, 0.01)
fit <- glm(f.trend ~ residuals, family = binomial, data = incdec.data)
yweight <- predict(fit, list(residuals = xweight), type="response")
lines(xweight, yweight)
summary(fit)

boxplot(brain.it.trends$residuals~brain.it.trends$f.trend, notch=TRUE,data = brain.it.trends, ylab= "Brain/IT residuals", xlab="Population trends")
incdec.data$f.trend<-droplevels(incdec.data$f.trend)
boxplot(incdec.data$residuals~incdec.data$f.trend, notch=TRUE,data = incdec.data, ylab= "Brain/IT residuals", xlab="Population trends")

d.aov<-(aov(residuals~f.trend, data = incdec.data))
TukeyHSD(d.aov)

plot(original.trends ~ residuals,data = data.scheper, main= "Scheper data")
abline(glm(original.trends ~ residuals,data = data.scheper, family = gaussian))
summary(glm(original.trends ~ residuals,data = data.scheper, family = gaussian))

plot(Bartomeus.Estimate ~ residuals,data = data.nacho) 
abline(glm(Bartomeus.Estimate ~ residuals,data = data.nacho))
summary(glm(Bartomeus.Estimate ~ residuals,data = data.nacho))

#Absolute brain
boxplot(Brain.weight ~ f.trend,data = incdec.data, main= "")

plot(original.trends ~ Brain.weight,data = data.scheper, main= "Scheper data")
abline(glm(original.trends ~ Brain.weight,data = data.scheper, family = gaussian))

plot(Bartomeus.Estimate ~ Brain.weight,data = data.nacho, main= "Bartomeus data")
abline(glm(Bartomeus.Estimate ~ Brain.weight,data = data.nacho, family = gaussian))



# PHYLOGENETICS-----
# scheper-----
data.scheper$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data2<-data.scheper
unique(data2$Species)


species=c("Andrena_barbilabris", "Andrena_bicolor", "Andrena_carantonica", "Andrena_chrysosceles", 
          "Andrena_cineraria", "Andrena_dorsata", "Andrena_flavipes","Andrena_fucata", 
          "Andrena_fulva", "Andrena_fulvida", "Andrena_gravida", "Andrena_labiata", 
          "Andrena_nigriceps", "Andrena_nigroaenea", "Andrena_nitida", "Andrena_ovatula", 
          "Andrena_pilipes", "Andrena_semilaevis", "Andrena_subopaca", "Andrena_tibialis",
          "Anthidium_manicatum", "Anthophora_plumipes", "Anthophora_quadrimaculata", "Anthophora_retusa",
          "Bombus_hortorum", "Bombus_jonellus", "Bombus_lapidarius", "Bombus_pascuorum", 
          "Bombus_pratorum", "Bombus_terrestris", "Halictus_confusus", "Halictus_rubicundus", 
          "Lasioglossum_calceatum", "Lasioglossum_fulvicorne", "Lasioglossum_leucozonium", "Lasioglossum_punctatissimum",
          "Lasioglossum_sexnotatum", "Lasioglossum_sexstrigatum", "Lasioglossum_zonulum", "Megachile_centuncularis",
          "Megachile_willughbiella", "Osmia_bicornis", "Osmia_caerulescens")
#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree2=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                    "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                    "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                    "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                    "Ctenocolletes", "Alocandrena", "Megandrena",      
                                    "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                    "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                    "Perdita", "Clavipanurgus", "Panurginus",        
                                    "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                    "Arhysosage", "Callonychium", "Cerceris",        
                                    "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                    "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                    "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                    "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                    "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                    "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                    "Samba", "Capicola", "Hesperapis",      
                                    "Eremaphanta", "Melitta", "Redivivoides",    
                                    "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                    "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                    "Sphecodopsis", "Pasites", "Oreopasites",     
                                    "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                    "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                    "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                    "Hexepeolus", "Neolarra", "Biastes",         
                                    "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                    "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                    "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                    "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                    "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                    "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                    "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                    "Peponapis", "Xenoglossa", "Tetraloniella",   
                                    "Tetralonia", "Svastra", "Martinapis",      
                                    "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                    "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                    "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                    "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                    "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                    "Aglae", "Eulaema", "Eufriesea",            
                                    "Tetragonilla", "Tetragonula", "Platytrigona",    
                                    "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                    "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                    "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                    "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                    "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                    "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                    "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                    "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                    "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                    "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                    "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                    "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                    "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                    "Ctenoplectra", "Macrogalea", "Allodapula",      
                                    "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                    "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                    "Allodape", "Fideliopsis", "Fidelia",         
                                    "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                    "Dioxys", "Noteriades", "Radoszkowskiana", 
                                    "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                    "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                    "Haetosmia", "Wainia", "Hoplosmia",           
                                    "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                    "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                    "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                    "Dianthidium", "Anthidiellum", "Paranthidium",  
                                    "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                    "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                    "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                    "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                    "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                    "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                    "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                    "Xenochlora", "Megaloptidia",    
                                    "Dinagapostemon", "Rhinetula",       
                                    "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                    "Eupetersia", "Mexalictus", "Patellapis",      
                                    "Thrincohalictus", "Homalictus",   
                                    "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                    "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                    "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                    "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                    "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                    "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                    "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                    "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                    "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                    "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                    "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                    "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                    "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                    "Hemicotelles", "Mourecotelles", 
                                    "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                    "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                    "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                    "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                    "Eucera", "Chilicola", "Duckeanthidium",
                                    "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                    "Melissodes", "Panurgus","Sphecodes","Ceratina","Xylocopa","Nomada",
                                    "Calliopsis","Agapostemon","Augochloropsis","Augochlorella","Augochlora","Colletes","Hylaeus"
))


plot(bee.tree2)
nodelabels()
tiplabels()
bee.tree2$tip.label == "Andrena"
unique(data2$Species)


#add dummy species labels
bee.tree2$tip.label<-paste(bee.tree2$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree2<-add.species.to.genus(bee.tree2,species[i],
                                   where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree2$tip.label)
bee.tree2<-drop.tip(bee.tree2,bee.tree2$tip.label[ii])
#Our tree
plot(bee.tree2, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree2$tip.label)

#Remove node labels, or the model will fail
bee.tree2$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree2, nodes = "TIPS", scale = TRUE)
A2 <- solve(inv.phylo$Ainv)
rownames(A2) <- rownames(inv.phylo$Ainv)
isSymmetric(A2, check.attributes = FALSE)





dataformcmc2=data2
dataformcmc2$Species
#Let's fix it
dataformcmc2$Species<-stri_replace_first_regex(dataformcmc2$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A2),dataformcmc2$Species)



#All set



#Brain.IT brms scheper
brm.scheper.brainit<-brm(original.trends ~ Brain.IT, data = dataformcmc2,
                cores=4,
                family = gaussian, cov_ranef = list("Species" = A2),
                control = list(adapt_delta = 0.99,max_treedepth=15))


brm.scheper.brainit
brm.scheper.brainit=add_criterion(brm.scheper.brainit,ic=c("waic"))
pp_check(brm.scheper.brainit,nsamples=1000)
bayes_R2(brm.scheper.brainit)
icc(brm.scheper.brainit, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.scheper.brainit)


#Residuals brms
brm.scheper.residuals<-brm(original.trends ~ residuals, data = dataformcmc2,
                cores=4,
                family = gaussian, cov_ranef = list("Species" = A2),
                control = list(adapt_delta = 0.99,max_treedepth=15))

brm.scheper.residuals
marginal_effects(brm.scheper.residuals)

pp_check(brm.scheper.residuals,nsamples=1000)
bayes_R2(brm.scheper.residuals)
icc(brm.residuals, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

#absolute brain size brms
brm.scheper.brain<-brm(original.trends ~ Brain.weight, data = dataformcmc2,
                           cores=4,
                           family = gaussian, cov_ranef = list("Species" = A2),
                           control = list(adapt_delta = 0.99,max_treedepth=15))
brm.scheper.brain
pp_check(brm.scheper.brain,nsamples=1000)
marginal_effects(brm.scheper.brain)

#Bartomeus data-----
data.nacho$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data3<-data.nacho
unique(data3$Species)


species=c("Agapostemon_sericeus","Agapostemon_virescens","Andrena_carlini","Andrena_crataegi",        
 "Andrena_dunningi","Andrena_fragilis","Andrena_frigida","Andrena_hippotes","Andrena_hirticincta",
 "Andrena_milwaukeensis","Andrena_miserabilis","Andrena_nasonii","Andrena_nubecula","Andrena_pruni",
 "Andrena_rugosa", "Andrena_simplex", "Andrena_vicina", "Augochlora_pura", "Augochlorella_aurata",
 "Augochloropsis_metallica", "Bombus_bimaculatus", "Bombus_griseocollis", "Bombus_impatiens","Bombus_ternarius",
 "Bombus_vagans", "Calliopsis_andreniformis", "Ceratina_calcarata", "Ceratina_strenua", "Colletes_thoracicus",
 "Halictus_ligatus", "Hylaeus_mesillae", "Hylaeus_modestus", "Lasioglossum_coriaceum", "Megachile_campanulae",
 "Megachile_gemula", "Megachile_mendica", "Megachile_pugnata", "Megachile_texana", "Melissodes_bimaculata",
 "Nomada_cressonii", "Nomada_luteoloides", "Osmia_atriventris", "Osmia_bucephala", "Osmia_lignaria",
 "Osmia_pumila", "Sphecodes_ranunculi","Xylocopa_virginica")



#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree3=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                     "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                     "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                     "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                     "Ctenocolletes", "Alocandrena", "Megandrena",      
                                     "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                     "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                     "Perdita", "Clavipanurgus", "Panurginus",        
                                     "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                     "Arhysosage", "Callonychium", "Cerceris",        
                                     "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                     "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                     "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                     "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                     "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                     "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                     "Samba", "Capicola", "Hesperapis",      
                                     "Eremaphanta", "Melitta", "Redivivoides",    
                                     "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                     "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                     "Sphecodopsis", "Pasites", "Oreopasites",     
                                     "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                     "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                     "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                     "Hexepeolus", "Neolarra", "Biastes",         
                                     "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                     "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                     "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                     "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                     "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                     "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                     "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                     "Peponapis", "Xenoglossa", "Tetraloniella",   
                                     "Tetralonia", "Svastra", "Martinapis",      
                                     "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                     "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                     "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                     "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                     "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                     "Aglae", "Eulaema", "Eufriesea",            
                                     "Tetragonilla", "Tetragonula", "Platytrigona",    
                                     "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                     "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                     "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                     "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                     "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                     "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                     "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                     "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                     "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                     "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                     "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                     "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                     "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                     "Ctenoplectra", "Macrogalea", "Allodapula",      
                                     "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                     "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                     "Allodape", "Fideliopsis", "Fidelia",         
                                     "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                     "Dioxys", "Noteriades", "Radoszkowskiana", 
                                     "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                     "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                     "Haetosmia", "Wainia", "Hoplosmia",           
                                     "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                     "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                     "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                     "Dianthidium", "Anthidiellum", "Paranthidium",  
                                     "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                     "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                     "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                     "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                     "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                     "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                     "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                     "Xenochlora", "Megaloptidia",    
                                     "Dinagapostemon", "Rhinetula",       
                                     "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                     "Eupetersia", "Mexalictus", "Patellapis",      
                                     "Thrincohalictus", "Homalictus",   
                                     "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                     "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                     "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                     "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                     "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                     "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                     "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                     "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                     "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                     "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                     "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                     "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                     "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                     "Hemicotelles", "Mourecotelles", 
                                     "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                     "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                     "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                     "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                     "Eucera", "Chilicola", "Duckeanthidium",
                                     "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                      "Panurgus","Anthidium","Anthophora"
))


plot(bee.tree3)
nodelabels()
tiplabels()
bee.tree3$tip.label == "Anthidium"
unique(data3$Species)


#add dummy species labels
bee.tree3$tip.label<-paste(bee.tree3$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree3<-add.species.to.genus(bee.tree3,species[i],
                                    where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree3$tip.label)
bee.tree3<-drop.tip(bee.tree3,bee.tree3$tip.label[ii])
#Our tree
plot(bee.tree3, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree3$tip.label)

#Remove node labels, or the model will fail
bee.tree3$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree3, nodes = "TIPS", scale = TRUE)
A3 <- solve(inv.phylo$Ainv)
rownames(A3) <- rownames(inv.phylo$Ainv)
isSymmetric(A3, check.attributes = FALSE)





dataformcmc3=data3
dataformcmc3$Species
#Let's fix it
dataformcmc3$Species<-stri_replace_first_regex(dataformcmc3$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A3),dataformcmc3$Species)
#All set
#Brain.IT brms scheper
brm.bartomeus.brainit<-brm(Bartomeus.Estimate ~ Brain.IT, data = dataformcmc3,
                         cores=4,
                         family = gaussian, cov_ranef = list("Species" = A3),
                         control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeus.brainit
brm.bartomeus.brainit=add_criterion(brm.bartomeus.brainit,ic=c("waic"))
pp_check(brm.bartomeus.brainit,nsamples=1000)
bayes_R2(brm.scheper.brainit)
icc(brm.bartomeus.brainit, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeus.brainit)


#Residuals brms
brm.bartomeus.residuals<-brm(Bartomeus.Estimate ~ residuals, data = dataformcmc3,
                           cores=4,
                           family = gaussian, cov_ranef = list("Species" = A2),
                           control = list(adapt_delta = 0.99,max_treedepth=15))

brm.bartomeus.residuals
marginal_effects(brm.bartomeus.residuals)

pp_check(brm.bartomeus.residuals,nsamples=1000)
bayes_R2(brm.scheper.residuals)
icc(brm.residuals, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

#absolute brain size brms
brm.bartomeus.brain<-brm(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc3,
                       cores=4,
                       family = gaussian, cov_ranef = list("Species" = A2),
                       control = list(adapt_delta = 0.99,max_treedepth=15))
brm.bartomeus.brain
pp_check(brm.bartomeus.brain,nsamples=1000)
marginal_effects(brm.bartomeus.brain)



#####increasing decreasing populations brms------
incdec.data$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data<-incdec.data
unique(data$Species)

summary(incdec.data$source)

species=c("Agapostemon_sericeus", "Agapostemon_virescens", "Andrena_barbilabris", "Andrena_bicolor",            
          "Andrena_carantonica", "Andrena_carlini", "Andrena_chrysosceles", "Andrena_cineraria",          
          "Andrena_crataegi", "Andrena_dorsata", "Andrena_flavipes","Andrena_fucata", 
          "Andrena_fulva", "Andrena_fulvida", "Andrena_gravida", "Andrena_labiata", 
          "Andrena_nigriceps", "Andrena_nigroaenea", "Andrena_nitida", "Andrena_nubecula",           
          "Andrena_ovatula", "Andrena_pilipes", "Andrena_pruni", "Andrena_semilaevis", 
          "Andrena_subopaca", "Andrena_tibialis","Anthidium_manicatum", "Anthophora_plumipes", 
          "Anthophora_quadrimaculata", "Anthophora_retusa", "Augochlora_pura", "Augochlorella_aurata", 
          "Augochloropsis_metallica", "Bombus_bimaculatus", "Bombus_griseocollis", "Bombus_hortorum", 
          "Bombus_impatiens", "Bombus_jonellus", "Bombus_lapidarius", "Bombus_pascuorum", 
          "Bombus_pratorum", "Bombus_ternarius", "Bombus_terrestris", "Bombus_vagans", 
          "Calliopsis_andreniformis", "Ceratina_calcarata", "Ceratina_strenua", "Colletes_thoracicus", 
          "Halictus_confusus", "Halictus_ligatus", "Halictus_rubicundus", "Hylaeus_modestus", 
          "Lasioglossum_calceatum", "Lasioglossum_fulvicorne", "Lasioglossum_leucozonium", "Lasioglossum_punctatissimum",
          "Lasioglossum_sexnotatum", "Lasioglossum_sexstrigatum", "Lasioglossum_zonulum", "Megachile_campanulae", 
          "Megachile_centuncularis", "Megachile_willughbiella", "Nomada_cressonii", "Nomada_luteoloides",         
          "Osmia_atriventris", "Osmia_bicornis", "Osmia_caerulescens", "Osmia_lignaria", "Xylocopa_virginica")
#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                    "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                    "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                    "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                    "Ctenocolletes", "Alocandrena", "Megandrena",      
                                    "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                    "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                    "Perdita", "Clavipanurgus", "Panurginus",        
                                    "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                    "Arhysosage", "Callonychium", "Cerceris",        
                                    "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                    "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                    "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                    "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                    "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                    "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                    "Samba", "Capicola", "Hesperapis",      
                                    "Eremaphanta", "Melitta", "Redivivoides",    
                                    "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                    "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                    "Sphecodopsis", "Pasites", "Oreopasites",     
                                    "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                    "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                    "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                    "Hexepeolus", "Neolarra", "Biastes",         
                                    "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                    "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                    "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                    "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                    "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                    "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                    "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                    "Peponapis", "Xenoglossa", "Tetraloniella",   
                                    "Tetralonia", "Svastra", "Martinapis",      
                                    "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                    "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                    "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                    "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                    "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                    "Aglae", "Eulaema", "Eufriesea",            
                                    "Tetragonilla", "Tetragonula", "Platytrigona",    
                                    "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                    "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                    "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                    "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                    "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                    "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                    "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                    "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                    "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                    "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                    "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                    "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                    "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                    "Ctenoplectra", "Macrogalea", "Allodapula",      
                                    "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                    "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                    "Allodape", "Fideliopsis", "Fidelia",         
                                    "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                    "Dioxys", "Noteriades", "Radoszkowskiana", 
                                    "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                    "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                    "Haetosmia", "Wainia", "Hoplosmia",           
                                    "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                    "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                    "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                    "Dianthidium", "Anthidiellum", "Paranthidium",  
                                    "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                    "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                    "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                    "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                    "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                    "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                    "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                    "Xenochlora", "Megaloptidia",    
                                    "Dinagapostemon", "Rhinetula",       
                                    "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                    "Eupetersia", "Mexalictus", "Patellapis",      
                                    "Thrincohalictus", "Homalictus",   
                                    "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                    "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                    "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                    "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                    "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                    "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                    "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                    "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                    "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                    "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                    "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                    "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                    "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                    "Hemicotelles", "Mourecotelles", 
                                    "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                    "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                    "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                    "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                    "Eucera", "Chilicola", "Duckeanthidium",
                                    "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                    "Melissodes", "Panurgus","Sphecodes" 
))


plot(bee.tree)
nodelabels()
tiplabels()
bee.tree$tip.label == "Andrena"
unique(data$Species)


#add dummy species labels
bee.tree$tip.label<-paste(bee.tree$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree<-add.species.to.genus(bee.tree,species[i],
                                   where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree$tip.label)
bee.tree<-drop.tip(bee.tree,bee.tree$tip.label[ii])
#Our tree
plot(bee.tree, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree$tip.label)

#Remove node labels, or the model will fail
bee.tree$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
isSymmetric(A, check.attributes = FALSE)





dataformcmc=data
dataformcmc$Species
#Let's fix it
dataformcmc$Species<-stri_replace_first_regex(dataformcmc$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A),dataformcmc$Species)



#if we compare A with our species column, they have different names
A
dataformcmc=data
dataformcmc$Species
#Let's fix it
dataformcmc$Species<-stri_replace_first_regex(dataformcmc$Species,pattern = " ", replacement = "_")
dataformcmc[dataformcmc$Species%in%"Psithyrus_vestalis","Species"]=c("Bombus_vestalis")
dataformcmc.success<-dataformcmc
dataformcmc.success<-subset(dataformcmc, subset = (dataformcmc$Success.test == 1))

#no differences. good
setdiff(rownames(A),dataformcmc$Species)

#All set




#This is the formula of the first model I want to do
#I changed binomial to bernoulli as it is more efficient for data with just 0's and 1's
brm.prueba<-brm(f.trend ~ Brain.IT, data = dataformcmc,
                cores=4,
                family = bernoulli, cov_ranef = list("Species" = A),
                control = list(adapt_delta = 0.99,max_treedepth=15))


brm.prueba
brm.succ8res=add_ic(brm.succ8res,ic=c("waic"))
pp_check(brm.succ8res,nsamples=1000)
bayes_R2(brm.succ8res)
icc(brm.succ8res, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.prueba)


#Residuals brms
brm.residuals<-brm(f.trend ~ residuals, data = dataformcmc,
                   cores=4,
                   family = bernoulli, cov_ranef = list("Species" = A),
                   control = list(adapt_delta = 0.99,max_treedepth=15))

brm.residuals$data
marginal_effects(brm.residuals)

brm.succ8res=add_ic(brm.succ8res,ic=c("waic"))
pp_check(brm.residuals,nsamples=1000)
bayes_R2(brm.residuals)
icc(brm.residuals, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

#Brain weight brms
brm.brainweight<-brm(f.trend ~ Brain.weight, data = dataformcmc,
                   cores=4,
                   family = bernoulli, cov_ranef = list("Species" = A),
                   control = list(adapt_delta = 0.99,max_treedepth=15))
marginal_effects(brm.brainweight)
bayes_R2(brm.brainweight)
pp_check(brm.brainweight,nsamples=1000)

#Does Queen have bigger brains?------
summary(brains.it$Sex)

boxplot(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, ylab="Brain/IT")

aggregate(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, FUN = mean)
aov.queens<-aov(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex)
TukeyHSD(aov.queens)


#Tree for full data.frame, with unknown and stable populations-----
brain.it.trends$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data<-brain.it.trends
unique(data$Species)


species=c("Agapostemon_sericeus", "Agapostemon_virescens", "Andrena_barbilabris", "Andrena_bicolor",            
          "Andrena_carantonica", "Andrena_carlini", "Andrena_chrysosceles", "Andrena_cineraria",          
          "Andrena_crataegi", "Andrena_dorsata", "Andrena_dunningi", "Andrena_flavipes",           
          "Andrena_fragilis", "Andrena_frigida", "Andrena_fucata", "Andrena_fulva",              
          "Andrena_fulvida", "Andrena_gravida", "Andrena_hippotes", "Andrena_hirticincta",        
          "Andrena_labiata", "Andrena_milwaukeensis", "Andrena_miserabilis", "Andrena_nasonii",            
          "Andrena_nigriceps", "Andrena_nigroaenea", "Andrena_nitida", "Andrena_nubecula",           
          "Andrena_ovatula", "Andrena_pilipes", "Andrena_pruni", "Andrena_rugosa",             
          "Andrena_semilaevis", "Andrena_simplex", "Andrena_subopaca", "Andrena_tibialis",           
          "Andrena_vicina", "Anthidium_manicatum", "Anthophora_plumipes", "Anthophora_quadrimaculata",  
          "Anthophora_retusa", "Augochlora_pura", "Augochlorella_aurata", "Augochloropsis_metallica",   
          "Bombus_bimaculatus", "Bombus_griseocollis", "Bombus_hortorum", "Bombus_impatiens",           
          "Bombus_jonellus", "Bombus_lapidarius", "Bombus_pascuorum", "Bombus_pratorum",            
          "Bombus_ternarius", "Bombus_terrestris", "Bombus_vagans", "Calliopsis_andreniformis",   
          "Ceratina_calcarata", "Ceratina_strenua", "Colletes_thoracicus", "Dasypoda_iberica",           
          "Flavipanurgus_venustus", "Halictus_confusus", "Halictus_ligatus", "Halictus_rubicundus",        
          "Hylaeus_mesillae", "Hylaeus_modestus", "Lasioglossum_calceatum", "Lasioglossum_coriaceum",     
          "Lasioglossum_fulvicorne", "Lasioglossum_leucozonium", "Lasioglossum_punctatissimum", "Lasioglossum_sexnotatum",    
          "Lasioglossum_sexstrigatum", "Lasioglossum_zonulum", "Megachile_campanulae", "Megachile_centuncularis",    
          "Megachile_gemula", "Megachile_mendica", "Megachile_pugnata", "Megachile_texana",           
          "Megachile_willughbiella", "Melissodes_bimaculata", "Nomada_cressonii", "Nomada_luteoloides",         
          "Nomada_merceti", "Osmia_atriventris", "Osmia_bicornis", "Osmia_bucephala",            
          "Osmia_caerulescens", "Osmia_lignaria", "Osmia_pumila", "Sphecodes_ranunculi",        
          "Xylocopa_virginica")

#BY MEANS OF SPECIES------
#We identify outliers and filter them
data.filtered<-data1
boxplot(Brain.weight~Species,data = data1, las= 2,cex.axis = 0.6)
boxplot(Brain.weight~Species,data = data.filtered, las= 2,cex.axis = 0.6)
subset(brains.it, subset = (brains.it$Sex == "Queen"))

subset(data1, subset = (Species == "Andrena barbilabris"))
mean(subset(data1, subset = (Species == "Andrena barbilabris"))$Brain.weight)
sd(subset(data1, subset = (Species == "Andrena barbilabris"))$Brain.weight)
data.filtered<-subset(data1, subset = !((Species == "Andrena barbilabris")&(X==7)))

subset(data.filtered, subset = (Species == "Andrena dunningi"))
subset(data1, subset = (Species == "Andrena dunningi"))

mean(subset(data1, subset = (Species == "Andrena dunningi"))$Brain.weight)
sd(subset(data1, subset = (Species == "Andrena dunningi"))$Brain.weight)


data.filtered<-subset(data.filtered, subset = !((Species == "Andrena dunningi")&(X==38)))

subset(data.filtered, subset = (Species == "Andrena fragilis"))

subset(data.filtered, subset = (Species == "Augochlorella aurata"))
subset(data1, subset = (Species == "Augochlorella aurata"))

mean(subset(data1, subset = (Species == "Augochlorella aurata"))$Brain.weight)
sd(subset(data1, subset = (Species == "Augochlorella aurata"))$Brain.weight)




data.filtered<-subset(data.filtered, subset = !((Species == "Augochlorella aurata")&(X==147)))


subset(data.filtered, subset = (Species == "Bombus impatiens"))

subset(data.filtered, subset = (Species == "Bombus pascuorum"))
subset(data1, subset = (Species == "Bombus pascuorum"))
mean(subset(data1, subset = (Species == "Bombus pascuorum"))$Brain.weight)
sd(subset(data1, subset = (Species == "Bombus pascuorum"))$Brain.weight)
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pascuorum")&(X==178)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pascuorum")&(X==187)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pascuorum")&(X==177)))


subset(data.filtered, subset = (Species == "Bombus pratorum"))

subset(data1, subset = (Species == "Bombus pratorum"))
mean(subset(data1, subset = (Species == "Bombus pratorum"))$Brain.weight)
sd(subset(data1, subset = (Species == "Bombus pratorum"))$Brain.weight)


data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pratorum")&(X==197)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pratorum")&(X==204)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pratorum")&(X==202)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pratorum")&(X==195)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus pratorum")&(X==196)))

subset(data.filtered, subset = (Species == "Bombus terrestris"))
subset(data1, subset = (Species == "Bombus terrestris"))
mean(subset(data1, subset = (Species == "Bombus terrestris"))$Brain.weight)
sd(subset(data1, subset = (Species == "Bombus terrestris"))$Brain.weight)


data.filtered<-subset(data.filtered, subset = !((Species == "Bombus terrestris")&(X==244)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus terrestris")&(X==235)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus terrestris")&(X==229)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus terrestris")&(X==255)))


subset(data.filtered, subset = (Species == "Bombus hortorum"))
subset(data1, subset = (Species == "Bombus hortorum"))

data.filtered<-subset(data.filtered, subset = !((Species == "Bombus hortorum")&(X==159)))





subset(data.filtered, subset = (Species == "Bombus lapidarius"))
subset(data1, subset = (Species == "Bombus lapidarius"))
subset(data1, subset = (Species == "Bombus lapidarius"))$Brain.weight


data.filtered<-subset(data.filtered, subset = !((Species == "Bombus lapidarius")&(X==175)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus lapidarius")&(X==174)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus lapidarius")&(X==175)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus lapidarius")&(X==176)))



subset(data.filtered, subset = (Species == "Bombus bimaculatus"))
subset(data1, subset = (Species == "Bombus bimaculatus"))


data.filtered<-subset(data.filtered, subset = !((Species == "Bombus bimaculatus")&(X==154)))
data.filtered<-subset(data.filtered, subset = !((Species == "Bombus bimaculatus")&(X==155)))

subset(data.filtered, subset = (Species == "Halictus ligatus"))
subset(data1, subset = (Species == "Halictus ligatus"))
mean(subset(data1, subset = (Species == "Halictus ligatus"))$Brain.weight)
sd(subset(data1, subset = (Species == "Halictus ligatus"))$Brain.weight)
data.filtered<-subset(data.filtered, subset = !((Species == "Halictus ligatus")&(X==295)))


subset(data.filtered, subset = (Species == "Lasioglossum calceatum"))
subset(data.filtered, subset = (Species == "Osmia caerulescens"))
subset(data1, subset = (Species == "Osmia caerulescens"))
mean(subset(data1, subset = (Species == "Osmia caerulescens"))$Brain.weight)
sd(subset(data1, subset = (Species == "Osmia caerulescens"))$Brain.weight)



data.filtered<-subset(data.filtered, subset = !((Species == "Osmia caerulescens")&(X==389)))

subset(data.filtered, subset = (Species == "Xylocopa virginica"))
subset(data1, subset = (Species == "Xylocopa virginica"))
mean(subset(data1, subset = (Species == "Xylocopa virginica"))$Brain.weight)
sd(subset(data1, subset = (Species == "Xylocopa virginica"))$Brain.weight)



data.filtered<-subset(data.filtered, subset = !((Species == "Xylocopa virginica")&(X==413)))
data.filtered<-subset(data.filtered, subset = !((Species == "Xylocopa virginica")&(X==414)))

boxplot(Brain.weight~Species,data = data.filtered, las= 2,cex.axis = 0.6)

boxplot(IT~Species,data = data.filtered, las= 2,cex.axis = 0.6)
boxplot(Brain.IT~Species,data = data.filtered, las= 2,cex.axis = 0.6)

#We create a new dataframe
brainses<-data.frame(aggregate(Brain.weight ~ Species, data = data.filtered, FUN = mean))
ITeses<-data.frame(aggregate(IT ~ Species, data = data.filtered, FUN = mean))
datos.a.merge<-data.frame(data.filtered$Species,
data.filtered$original.trends,
data.filtered$source,
data.filtered$f.trend,
data.filtered$Bartomeus.Estimate,
data.filtered$Bartomeus.SE)
colnames(datos.a.merge)<-c("Species","original.trends","source","f.trend","Bartomeus.Estimate","Bartomeus.SE")
datos.a.merge<-(unique(datos.a.merge))
data.means<-merge(merge(datos.a.merge, brainses),ITeses)


#We add residuals
plot(Brain.weight ~ IT, data=data.means)
abline(lm(Brain.weight ~ IT, data=data.means))
summary(lm(Brain.weight ~ IT, data=data.means))
summary(lm(log(Brain.weight) ~ log(IT), data=data.means))
lm.data.means<-lm(log(Brain.weight) ~ log(IT), data=data.means)
lm.data.means$residuals
lm.data.means$residuals
data.means$residuals<-lm.data.means$residuals


data.means.scheper<-subset(data.means, subset = (data.means$source == "Scheper"))
data.means.nacho<-subset(data.means, subset = (data.means$source == "Bartomeus"))
levels(data.means.nacho$Species)
data.means.nacho$Species<-droplevels(data.means.nacho$Species)

#Analysis
#Brain.weight
plot(Brain.weight ~ f.trend ,data = data.means, notch = FALSE, main="All data")
plot(as.numeric(f.trend) ~ Brain.weight,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing")), main="All data")
abline(glm(as.numeric(f.trend) ~ Brain.weight,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))))
summary(glm(as.numeric(f.trend) ~ Brain.weight,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))), family = "binomial")
visreg(glm(as.numeric(f.trend) ~ Brain.weight,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))))
aov.data.means<-aov(Brain.weight ~ f.trend ,data = data.means)
aggregate(Brain.weight~f.trend, FUN = mean, data = data.means)
TukeyHSD(aov.data.means)
colnames(data.means)
data.means.incdec<-subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))
glm.f.trend.brain<-glm(f.trend ~ Brain.weight, family=binomial,data = data.means.incdec)
summary(glm.f.trend.brain)
visreg(glm.f.trend.brain)
glmer.f.trend.brain<-glmer(f.trend ~ Brain.weight + (1|Species), family=binomial,data = data.means.incdec)
summary(glmer.f.trend.brain)
visreg(glmer.f.trend.brain)
#Residuals
plot(residuals ~ f.trend ,data = data.means, notch = FALSE, main="All data")
plot(as.numeric(f.trend) ~ residuals,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing")), main="All data")
abline(glm(as.numeric(f.trend) ~ residuals,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))))
summary(glm(as.numeric(f.trend) ~ residuals,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))))
visreg(glm(as.numeric(f.trend) ~ residuals,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))))
aov.data.means.res<-aov(residuals ~ f.trend ,data = data.means)
aggregate(residuals~f.trend, FUN = mean, data = data.means)
TukeyHSD(aov.data.means.res)
glm.f.trend.res<-glm(f.trend ~ residuals, family=binomial,data = data.means.incdec)
summary(glm.f.trend.res)
visreg(glm.f.trend.res)
glmer.f.trend.res<-glmer(f.trend ~ residuals + (1|Species), family=binomial,data = data.means.incdec)
summary(glmer.f.trend.res)
visreg(glmer.f.trend.res)
#IT?
plot(IT ~ f.trend ,data = data.means, notch = FALSE, main="All data")
plot(as.numeric(f.trend) ~ IT,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing")), main="All data")
abline(glm(as.numeric(f.trend) ~ IT,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))))
summary(glm(as.numeric(f.trend) ~ IT,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))), family = "binomial")
visreg(glm(as.numeric(f.trend) ~ IT,data = subset(data.means, subset=((data.means$f.trend == "declining"))|(data.means$f.trend == "increasing"))))
aov.data.means.it<-aov(IT ~ f.trend ,data = data.means)
aggregate(IT~f.trend, FUN = mean, data = data.means)
TukeyHSD(aov.data.means.it)
glmer.f.trend.IT<-glmer(f.trend ~ IT + (1|Species), family=binomial,data = data.means.incdec)
summary(glmer.f.trend.IT)
visreg(glmer.f.trend.IT)


#Bartomeus data
summary(data.means.nacho$f.trend)
plot(Brain.weight ~ f.trend ,data = data.means.nacho, notch = FALSE, main="Bartomeus data")
aov.data.nacho.means<-aov(Brain.weight ~ f.trend ,data = data.means.nacho)
aggregate(Brain.weight~f.trend, FUN = mean, data = data.means.nacho)
TukeyHSD(aov.data.nacho.means)
plot(Bartomeus.Estimate ~ Brain.weight  ,data = data.means.nacho, main="Bartomeus data")
abline(glm(Bartomeus.Estimate ~ Brain.weight, data = data.means.nacho))
visreg(glm(Bartomeus.Estimate ~ Brain.weight, data = data.means.nacho))
droplevels(data.means.nacho$Species)

glmer.bartomeus<-glm(Bartomeus.Estimate ~ Brain.weight, family = gaussian, data = data.means.nacho)
summary(glmer.bartomeus)
#error here, we can't have the same number of grouping factor as observations
lmer(Bartomeus.Estimate~Brain.weight + (1|Species),data = data.means.nacho)
summary(data.means.nacho$Species)

plot(Bartomeus.Estimate ~ residuals  ,data = data.means.nacho, main="Bartomeus data")
abline(glm(Bartomeus.Estimate ~ residuals, data = data.means.nacho))

nrow(data.means.nacho)
#Scheper data
plot(Brain.weight ~ f.trend ,data = data.means.scheper, notch = FALSE, main = "Scheper data")
aov.data.scheper.means<-aov(Brain.weight ~ f.trend ,data = data.means.scheper)
aggregate(Brain.weight~f.trend, FUN = mean, data = data.means.scheper)
TukeyHSD(aov.data.scheper.means)
data.means.scheper$original.trends<-as.numeric(as.character(data.means.scheper$original.trends))
plot(original.trends ~ Brain.weight  ,data = data.means.scheper, main="Scheper data")
abline(glm(original.trends ~ Brain.weight  ,data = data.means.scheper))
visreg(glm(original.trends ~ Brain.weight  ,data = data.means.scheper))
summary(glm(original.trends ~ Brain.weight  ,data = data.means.scheper))

plot(residuals ~ f.trend ,data = data.means.scheper, notch = FALSE, main = "Scheper data")
aov.data.scheper.means.res<-aov(residuals ~ f.trend ,data = data.means.scheper)
aggregate(Brain.weight~f.trend, FUN = mean, data = data.means.scheper)
TukeyHSD(aov.data.scheper.means.res)
data.means.scheper$original.trends<-as.numeric(as.character(data.means.scheper$original.trends))
plot(original.trends ~ residuals  ,data = data.means.scheper, main="Scheper data")
abline(glm(original.trends ~ residuals  ,data = data.means.scheper))
visreg(glm(original.trends ~ residuals  ,data = data.means.scheper))
summary(glm(original.trends ~ residuals  ,data = data.means.scheper))


#phylogenetics
unique(data2$Species)
unique(data.means.incdec$Species)


incdec.data$Species

#####increasing decreasing populations brms------
data.means.incdec$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data4<-data.means.incdec
unique(data4$Species)

summary(data.means$source)

species=c("Agapostemon_sericeus", "Agapostemon_virescens", "Andrena_barbilabris", "Andrena_bicolor",            
          "Andrena_carantonica", "Andrena_carlini", "Andrena_chrysosceles", "Andrena_cineraria",          
          "Andrena_crataegi", "Andrena_dorsata", "Andrena_flavipes","Andrena_fucata", 
          "Andrena_fulva", "Andrena_fulvida", "Andrena_gravida", "Andrena_labiata", 
          "Andrena_nigriceps", "Andrena_nigroaenea", "Andrena_nitida", "Andrena_nubecula",           
          "Andrena_ovatula", "Andrena_pilipes", "Andrena_pruni", "Andrena_semilaevis", 
          "Andrena_subopaca", "Andrena_tibialis","Anthidium_manicatum", "Anthophora_plumipes", 
          "Anthophora_quadrimaculata", "Anthophora_retusa", "Augochlora_pura", "Augochlorella_aurata", 
          "Augochloropsis_metallica", "Bombus_griseocollis", "Bombus_impatiens", "Bombus_jonellus",
          "Bombus_lapidarius", "Bombus_pascuorum", "Bombus_pratorum", "Bombus_ternarius",
          "Bombus_terrestris", "Bombus_vagans",  "Calliopsis_andreniformis", "Ceratina_calcarata",
          "Ceratina_strenua", "Colletes_thoracicus", "Halictus_confusus", "Halictus_ligatus", 
          "Halictus_rubicundus", "Hylaeus_modestus", "Lasioglossum_calceatum", "Lasioglossum_fulvicorne", 
          "Lasioglossum_leucozonium", "Lasioglossum_punctatissimum", "Lasioglossum_sexnotatum", "Lasioglossum_sexstrigatum", 
          "Lasioglossum_zonulum", "Megachile_campanulae","Megachile_centuncularis", "Megachile_willughbiella", 
          "Nomada_cressonii", "Nomada_luteoloides", "Osmia_atriventris", "Osmia_bicornis", 
          "Osmia_caerulescens", "Osmia_lignaria", "Xylocopa_virginica")
#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree4=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                    "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                    "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                    "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                    "Ctenocolletes", "Alocandrena", "Megandrena",      
                                    "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                    "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                    "Perdita", "Clavipanurgus", "Panurginus",        
                                    "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                    "Arhysosage", "Callonychium", "Cerceris",        
                                    "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                    "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                    "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                    "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                    "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                    "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                    "Samba", "Capicola", "Hesperapis",      
                                    "Eremaphanta", "Melitta", "Redivivoides",    
                                    "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                    "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                    "Sphecodopsis", "Pasites", "Oreopasites",     
                                    "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                    "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                    "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                    "Hexepeolus", "Neolarra", "Biastes",         
                                    "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                    "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                    "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                    "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                    "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                    "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                    "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                    "Peponapis", "Xenoglossa", "Tetraloniella",   
                                    "Tetralonia", "Svastra", "Martinapis",      
                                    "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                    "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                    "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                    "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                    "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                    "Aglae", "Eulaema", "Eufriesea",            
                                    "Tetragonilla", "Tetragonula", "Platytrigona",    
                                    "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                    "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                    "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                    "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                    "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                    "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                    "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                    "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                    "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                    "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                    "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                    "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                    "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                    "Ctenoplectra", "Macrogalea", "Allodapula",      
                                    "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                    "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                    "Allodape", "Fideliopsis", "Fidelia",         
                                    "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                    "Dioxys", "Noteriades", "Radoszkowskiana", 
                                    "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                    "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                    "Haetosmia", "Wainia", "Hoplosmia",           
                                    "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                    "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                    "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                    "Dianthidium", "Anthidiellum", "Paranthidium",  
                                    "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                    "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                    "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                    "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                    "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                    "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                    "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                    "Xenochlora", "Megaloptidia",    
                                    "Dinagapostemon", "Rhinetula",       
                                    "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                    "Eupetersia", "Mexalictus", "Patellapis",      
                                    "Thrincohalictus", "Homalictus",   
                                    "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                    "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                    "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                    "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                    "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                    "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                    "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                    "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                    "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                    "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                    "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                    "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                    "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                    "Hemicotelles", "Mourecotelles", 
                                    "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                    "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                    "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                    "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                    "Eucera", "Chilicola", "Duckeanthidium",
                                    "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                    "Melissodes", "Panurgus","Sphecodes" 
))


plot(bee.tree4)
nodelabels()
tiplabels()
bee.tree$tip.label == "Andrena"
unique(data4$Species)


#add dummy species labels
bee.tree4$tip.label<-paste(bee.tree4$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree4<-add.species.to.genus(bee.tree4,species[i],
                                   where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree4$tip.label)
bee.tree4<-drop.tip(bee.tree4,bee.tree4$tip.label[ii])
#Our tree
plot(bee.tree4, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree4$tip.label)

#Remove node labels, or the model will fail
bee.tree4$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree4, nodes = "TIPS", scale = TRUE)
A4 <- solve(inv.phylo$Ainv)
rownames(A4) <- rownames(inv.phylo$Ainv)
isSymmetric(A4, check.attributes = FALSE)





dataformcmc4=data
dataformcmc4$Species
#Let's fix it
dataformcmc4$Species<-stri_replace_first_regex(dataformcmc4$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A4),dataformcmc4$Species)

#if we compare A with our species column, they have different names

#no differences. good
setdiff(rownames(A4),dataformcmc4$Species)

#All set

#This is the formula of the first model I want to do
#I changed binomial to bernoulli as it is more efficient for data with just 0's and 1's
brm.brain.incdec.means<-brm(f.trend ~ Brain.weight, data = dataformcmc4,
                cores=4,
                family = bernoulli, cov_ranef = list("Species" = A4),
                control = list(adapt_delta = 0.99,max_treedepth=15))

brm.brain.incdec.means
brm.brain.incdec.means=add_ic(brm.brain.incdec.means,ic=c("waic"))
pp_check(brm.brain.incdec.means,nsamples=1000)
bayes_R2(brm.brain.incdec.means)
icc(brm.brain.incdec.means, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.brain.incdec.means)

brm.brain.incdec.means.res<-brm(f.trend ~ residuals, data = dataformcmc4,
                            cores=4,
                            family = bernoulli, cov_ranef = list("Species" = A4),
                            control = list(adapt_delta = 0.99,max_treedepth=15))

brm.brain.incdec.means.res
brm.brain.incdec.means=add_ic(brm.brain.incdec.means,ic=c("waic"))
pp_check(brm.brain.incdec.means.res,nsamples=1000)
bayes_R2(brm.brain.incdec.means.res)
icc(brm.brain.incdec.means, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.brain.incdec.means.res)


brm.brain.incdec.means.IT<-brm(f.trend ~ IT, data = dataformcmc4,
                                cores=4,
                                family = bernoulli, cov_ranef = list("Species" = A4),
                                control = list(adapt_delta = 0.99,max_treedepth=15))

brm.brain.incdec.means.IT
brm.brain.incdec.means=add_ic(brm.brain.incdec.means,ic=c("waic"))
pp_check(brm.brain.incdec.means.res,nsamples=1000)
bayes_R2(brm.brain.incdec.means.res)
icc(brm.brain.incdec.means, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.brain.incdec.means.IT)


#scheper----
data.means.scheper$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data5<-data.scheper
unique(data5$Species)


species=c("Andrena_barbilabris", "Andrena_bicolor", "Andrena_carantonica", "Andrena_chrysosceles", 
          "Andrena_cineraria", "Andrena_dorsata", "Andrena_flavipes","Andrena_fucata", 
          "Andrena_fulva", "Andrena_fulvida", "Andrena_gravida", "Andrena_labiata", 
          "Andrena_nigriceps", "Andrena_nigroaenea", "Andrena_nitida", "Andrena_ovatula", 
          "Andrena_pilipes", "Andrena_semilaevis", "Andrena_subopaca", "Andrena_tibialis",
          "Anthidium_manicatum", "Anthophora_plumipes", "Anthophora_quadrimaculata", "Anthophora_retusa",
          "Bombus_hortorum", "Bombus_jonellus", "Bombus_lapidarius", "Bombus_pascuorum", 
          "Bombus_pratorum", "Bombus_terrestris", "Halictus_confusus", "Halictus_rubicundus", 
          "Lasioglossum_calceatum", "Lasioglossum_fulvicorne", "Lasioglossum_leucozonium", "Lasioglossum_punctatissimum",
          "Lasioglossum_sexnotatum", "Lasioglossum_sexstrigatum", "Lasioglossum_zonulum", "Megachile_centuncularis",
          "Megachile_willughbiella", "Osmia_bicornis", "Osmia_caerulescens")
#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree5=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                     "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                     "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                     "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                     "Ctenocolletes", "Alocandrena", "Megandrena",      
                                     "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                     "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                     "Perdita", "Clavipanurgus", "Panurginus",        
                                     "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                     "Arhysosage", "Callonychium", "Cerceris",        
                                     "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                     "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                     "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                     "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                     "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                     "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                     "Samba", "Capicola", "Hesperapis",      
                                     "Eremaphanta", "Melitta", "Redivivoides",    
                                     "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                     "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                     "Sphecodopsis", "Pasites", "Oreopasites",     
                                     "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                     "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                     "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                     "Hexepeolus", "Neolarra", "Biastes",         
                                     "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                     "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                     "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                     "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                     "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                     "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                     "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                     "Peponapis", "Xenoglossa", "Tetraloniella",   
                                     "Tetralonia", "Svastra", "Martinapis",      
                                     "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                     "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                     "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                     "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                     "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                     "Aglae", "Eulaema", "Eufriesea",            
                                     "Tetragonilla", "Tetragonula", "Platytrigona",    
                                     "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                     "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                     "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                     "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                     "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                     "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                     "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                     "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                     "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                     "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                     "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                     "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                     "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                     "Ctenoplectra", "Macrogalea", "Allodapula",      
                                     "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                     "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                     "Allodape", "Fideliopsis", "Fidelia",         
                                     "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                     "Dioxys", "Noteriades", "Radoszkowskiana", 
                                     "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                     "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                     "Haetosmia", "Wainia", "Hoplosmia",           
                                     "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                     "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                     "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                     "Dianthidium", "Anthidiellum", "Paranthidium",  
                                     "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                     "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                     "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                     "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                     "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                     "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                     "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                     "Xenochlora", "Megaloptidia",    
                                     "Dinagapostemon", "Rhinetula",       
                                     "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                     "Eupetersia", "Mexalictus", "Patellapis",      
                                     "Thrincohalictus", "Homalictus",   
                                     "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                     "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                     "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                     "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                     "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                     "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                     "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                     "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                     "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                     "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                     "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                     "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                     "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                     "Hemicotelles", "Mourecotelles", 
                                     "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                     "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                     "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                     "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                     "Eucera", "Chilicola", "Duckeanthidium",
                                     "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                     "Melissodes", "Panurgus","Sphecodes","Ceratina","Xylocopa","Nomada",
                                     "Calliopsis","Agapostemon","Augochloropsis","Augochlorella","Augochlora","Colletes","Hylaeus"
))


plot(bee.tree5)
nodelabels()
tiplabels()
bee.tree2$tip.label == "Andrena"
unique(data5$Species)


#add dummy species labels
bee.tree5$tip.label<-paste(bee.tree5$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree5<-add.species.to.genus(bee.tree5,species[i],
                                    where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree5$tip.label)
bee.tree5<-drop.tip(bee.tree5,bee.tree5$tip.label[ii])
#Our tree
plot(bee.tree5, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree2$tip.label)

#Remove node labels, or the model will fail
bee.tree5$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree5, nodes = "TIPS", scale = TRUE)
A5 <- solve(inv.phylo$Ainv)
rownames(A5) <- rownames(inv.phylo$Ainv)
isSymmetric(A5, check.attributes = FALSE)





dataformcmc5=data5
dataformcmc5$Species
#Let's fix it
dataformcmc5$Species<-stri_replace_first_regex(dataformcmc5$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A5),dataformcmc5$Species)



#All set



#Brain.IT brms scheper
brm.scheper.means.brain<-brm(original.trends ~ Brain.weight, data = dataformcmc5,
                         cores=4,
                         family = gaussian, cov_ranef = list("Species" = A5),
                         control = list(adapt_delta = 0.99,max_treedepth=15))


brm.scheper.means.brain
brm.scheper.brainit=add_criterion(brm.scheper.brainit,ic=c("waic"))
pp_check(brm.scheper.brainit,nsamples=1000)
bayes_R2(brm.scheper.means.brain)
icc(brm.scheper.brainit, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.scheper.means.brain)



brm.scheper.means.res<-brm(original.trends ~ residuals, data = dataformcmc5,
                             cores=4,
                             family = gaussian, cov_ranef = list("Species" = A5),
                             control = list(adapt_delta = 0.99,max_treedepth=15))


brm.scheper.means.res
brm.scheper.brainit=add_criterion(brm.scheper.brainit,ic=c("waic"))
pp_check(brm.scheper.means.res,nsamples=1000)
bayes_R2(brm.scheper.means.res)
icc(brm.scheper.brainit, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.scheper.means.res)

brm.scheper.means.IT<-brm(original.trends ~ IT, data = dataformcmc5,
                           cores=4,
                           family = gaussian, cov_ranef = list("Species" = A5),
                           control = list(adapt_delta = 0.99,max_treedepth=15))


brm.scheper.means.IT
brm.scheper.brainit=add_criterion(brm.scheper.brainit,ic=c("waic"))
pp_check(brm.scheper.means.IT,nsamples=1000)
bayes_R2(brm.scheper.means.IT)
icc(brm.scheper.means.IT, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.scheper.means.IT)

#Bartomeus----
data.means.nacho$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data6<-data.means.nacho
unique(data6$Species)


species=c("Agapostemon_sericeus","Agapostemon_virescens","Andrena_carlini","Andrena_crataegi",        
          "Andrena_dunningi","Andrena_fragilis","Andrena_frigida","Andrena_hippotes",
          "Andrena_hirticincta","Andrena_milwaukeensis","Andrena_miserabilis","Andrena_nasonii",
          "Andrena_nubecula","Andrena_pruni","Andrena_rugosa", "Andrena_simplex", 
          "Andrena_vicina", "Augochlora_pura", "Augochlorella_aurata","Augochloropsis_metallica", 
          "Bombus_griseocollis", "Bombus_impatiens","Bombus_ternarius","Bombus_vagans", 
          "Calliopsis_andreniformis", "Ceratina_calcarata", "Ceratina_strenua", "Colletes_thoracicus",
          "Halictus_ligatus", "Hylaeus_mesillae", "Hylaeus_modestus", "Lasioglossum_coriaceum", 
          "Megachile_campanulae","Megachile_gemula", "Megachile_mendica", "Megachile_pugnata", 
          "Megachile_texana", "Melissodes_bimaculata","Nomada_cressonii", "Nomada_luteoloides",
          "Osmia_atriventris", "Osmia_bucephala", "Osmia_lignaria","Osmia_pumila", 
          "Sphecodes_ranunculi","Xylocopa_virginica")



#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree6=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                     "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                     "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                     "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                     "Ctenocolletes", "Alocandrena", "Megandrena",      
                                     "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                     "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                     "Perdita", "Clavipanurgus", "Panurginus",        
                                     "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                     "Arhysosage", "Callonychium", "Cerceris",        
                                     "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                     "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                     "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                     "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                     "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                     "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                     "Samba", "Capicola", "Hesperapis",      
                                     "Eremaphanta", "Melitta", "Redivivoides",    
                                     "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                     "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                     "Sphecodopsis", "Pasites", "Oreopasites",     
                                     "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                     "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                     "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                     "Hexepeolus", "Neolarra", "Biastes",         
                                     "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                     "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                     "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                     "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                     "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                     "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                     "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                     "Peponapis", "Xenoglossa", "Tetraloniella",   
                                     "Tetralonia", "Svastra", "Martinapis",      
                                     "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                     "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                     "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                     "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                     "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                     "Aglae", "Eulaema", "Eufriesea",            
                                     "Tetragonilla", "Tetragonula", "Platytrigona",    
                                     "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                     "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                     "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                     "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                     "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                     "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                     "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                     "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                     "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                     "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                     "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                     "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                     "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                     "Ctenoplectra", "Macrogalea", "Allodapula",      
                                     "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                     "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                     "Allodape", "Fideliopsis", "Fidelia",         
                                     "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                     "Dioxys", "Noteriades", "Radoszkowskiana", 
                                     "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                     "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                     "Haetosmia", "Wainia", "Hoplosmia",           
                                     "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                     "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                     "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                     "Dianthidium", "Anthidiellum", "Paranthidium",  
                                     "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                     "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                     "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                     "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                     "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                     "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                     "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                     "Xenochlora", "Megaloptidia",    
                                     "Dinagapostemon", "Rhinetula",       
                                     "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                     "Eupetersia", "Mexalictus", "Patellapis",      
                                     "Thrincohalictus", "Homalictus",   
                                     "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                     "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                     "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                     "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                     "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                     "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                     "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                     "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                     "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                     "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                     "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                     "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                     "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                     "Hemicotelles", "Mourecotelles", 
                                     "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                     "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                     "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                     "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                     "Eucera", "Chilicola", "Duckeanthidium",
                                     "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                     "Panurgus","Anthidium","Anthophora"
))


plot(bee.tree6)
nodelabels()
tiplabels()
bee.tree6$tip.label == "Anthidium"
unique(data6$Species)


#add dummy species labels
bee.tree6$tip.label<-paste(bee.tree6$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree6<-add.species.to.genus(bee.tree6,species[i],
                                    where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree6$tip.label)
bee.tree6<-drop.tip(bee.tree6,bee.tree6$tip.label[ii])
#Our tree
plot(bee.tree6, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree6$tip.label)

#Remove node labels, or the model will fail
bee.tree6$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree6, nodes = "TIPS", scale = TRUE)
A6 <- solve(inv.phylo$Ainv)
rownames(A6) <- rownames(inv.phylo$Ainv)
isSymmetric(A6, check.attributes = FALSE)





dataformcmc6=data6
dataformcmc6$Species
#Let's fix it
dataformcmc6$Species<-stri_replace_first_regex(dataformcmc6$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A6),dataformcmc6$Species)
#All set

#brain weight
brm.bartomeus.means.brains<-brm(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc6,
                           cores=4,
                           family = gaussian, cov_ranef = list("Species" = A6),
                           control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeus.means.brains
brm.bartomeus.brainit=add_criterion(brm.bartomeus.brainit,ic=c("waic"))
pp_check(brm.bartomeus.means.brains,nsamples=1000)
bayes_R2(brm.bartomeus.means.brains)
icc(brm.bartomeus.brainit, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeus.means.brains)

#residuals
brm.bartomeus.means.residuals<-brm(Bartomeus.Estimate ~ residuals, data = dataformcmc6,
                                cores=4,
                                family = gaussian, cov_ranef = list("Species" = A6),
                                control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeus.means.residuals
brm.bartomeus.brainit=add_criterion(brm.bartomeus.brainit,ic=c("waic"))
pp_check(brm.bartomeus.means.brains,nsamples=1000)
bayes_R2(brm.bartomeus.means.residuals)
icc(brm.bartomeus.brainit, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeus.means.residuals)

#IT
brm.bartomeus.means.IT<-brm(Bartomeus.Estimate ~ IT, data = dataformcmc6,
                                   cores=4,
                                   family = gaussian, cov_ranef = list("Species" = A6),
                                   control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeus.means.IT
pp_check(brm.bartomeus.means.IT,nsamples=1000)
bayes_R2(brm.bartomeus.means.IT)
icc(brm.bartomeus.brainit, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeus.means.IT)

#HABITAT PREFERENCE------
habpref <- read.csv("Raw_data/habitat_preference_simplify.csv")
habpref$Species
data1$Species
setdiff(habpref$Species,data1$Species)
data.means.pref<-merge(data.means,habpref)
plot(f.trend ~ Urban,data = data.means.pref)

#Population trends with habitat preference
plot(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref)
abline(glm(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref))
summary(glm(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref))

plot(Bartomeus.Estimate ~ Forests,data = data.means.pref)
abline(glm(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref))
summary(glm(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref))


plot(Bartomeus.Estimate ~ Urban,data = data.means.pref)
abline(glm(Bartomeus.Estimate ~ Urban,data = data.means.pref))
summary(glm(Bartomeus.Estimate ~ Urban,data = data.means.pref))
visreg(glm(Bartomeus.Estimate ~ Urban,data = data.means.pref))
nrow(data.means.pref)

#Habitat preference with brain sizes
plot(Brain.weight~IT, data = data.means.pref)
abline(lm(Brain.weight~IT, data = data.means.pref))
lm.bw<-lm(Brain.weight~IT, data = data.means.pref)
lm.bw$residuals
data.means.pref$residuals<-lm.bw$residuals

#Crops, Pasture
plot(Pasture.and.Crops ~ Brain.weight,data = data.means.pref)
abline(glm(Pasture.and.Crops ~ Brain.weight,data = data.means.pref))
visreg(glm(Pasture.and.Crops ~ Brain.weight,data = data.means.pref))
summary(glm(Pasture.and.Crops ~ Brain.weight,data = data.means.pref))

plot(Pasture.and.Crops ~ residuals,data = data.means.pref)
abline(glm(Pasture.and.Crops ~ residuals,data = data.means.pref))
visreg(glm(Pasture.and.Crops ~ residuals,data = data.means.pref))
summary(glm(Pasture.and.Crops ~ residuals,data = data.means.pref))

plot(Pasture.and.Crops ~ IT,data = data.means.pref)
abline(glm(Pasture.and.Crops ~ IT,data = data.means.pref))
visreg(glm(Pasture.and.Crops ~ IT,data = data.means.pref))
summary(glm(Pasture.and.Crops ~ IT,data = data.means.pref))


#phylogenetics
data.means.pref$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data7<-data.means.pref
unique(data7$Species)


species=c("Agapostemon_virescens","Andrena_carlini","Andrena_nasonii", "Augochlora_pura", 
          "Augochlorella_aurata", "Bombus_griseocollis","Calliopsis_andreniformis", "Ceratina_strenua",
          "Halictus_confusus","Halictus_rubicundus","Lasioglossum_coriaceum","Megachile_mendica",
          "Melissodes_bimaculata", "Osmia_atriventris", "Osmia_bucephala","Osmia_pumila",
          "Xylocopa_virginica")



#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree7=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                     "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                     "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                     "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                     "Ctenocolletes", "Alocandrena", "Megandrena",      
                                     "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                     "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                     "Perdita", "Clavipanurgus", "Panurginus",        
                                     "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                     "Arhysosage", "Callonychium", "Cerceris",        
                                     "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                     "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                     "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                     "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                     "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                     "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                     "Samba", "Capicola", "Hesperapis",      
                                     "Eremaphanta", "Melitta", "Redivivoides",    
                                     "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                     "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                     "Sphecodopsis", "Pasites", "Oreopasites",     
                                     "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                     "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                     "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                     "Hexepeolus", "Neolarra", "Biastes",         
                                     "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                     "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                     "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                     "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                     "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                     "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                     "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                     "Peponapis", "Xenoglossa", "Tetraloniella",   
                                     "Tetralonia", "Svastra", "Martinapis",      
                                     "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                     "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                     "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                     "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                     "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                     "Aglae", "Eulaema", "Eufriesea",            
                                     "Tetragonilla", "Tetragonula", "Platytrigona",    
                                     "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                     "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                     "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                     "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                     "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                     "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                     "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                     "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                     "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                     "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                     "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                     "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                     "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                     "Ctenoplectra", "Macrogalea", "Allodapula",      
                                     "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                     "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                     "Allodape", "Fideliopsis", "Fidelia",         
                                     "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                     "Dioxys", "Noteriades", "Radoszkowskiana", 
                                     "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                     "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                     "Haetosmia", "Wainia", "Hoplosmia",           
                                     "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                     "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                     "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                     "Dianthidium", "Anthidiellum", "Paranthidium",  
                                     "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                     "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                     "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                     "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                     "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                     "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                     "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                     "Xenochlora", "Megaloptidia",    
                                     "Dinagapostemon", "Rhinetula",       
                                     "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                     "Eupetersia", "Mexalictus", "Patellapis",      
                                     "Thrincohalictus", "Homalictus",   
                                     "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                     "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                     "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                     "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                     "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                     "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                     "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                     "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                     "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                     "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                     "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                     "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                     "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                     "Hemicotelles", "Mourecotelles", 
                                     "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                     "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                     "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                     "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                     "Eucera", "Chilicola", "Duckeanthidium",
                                     "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                     "Panurgus","Anthidium","Anthophora","Nomada","Sphecodes",
                                     "Augochloropsis","Colletes","Hylaeus"
))


plot(bee.tree7)
nodelabels()
tiplabels()
unique(data7$Species)


#add dummy species labels
bee.tree7$tip.label<-paste(bee.tree7$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree7<-add.species.to.genus(bee.tree7,species[i],
                                    where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree7$tip.label)
bee.tree7<-drop.tip(bee.tree7,bee.tree7$tip.label[ii])
#Our tree
plot(bee.tree7, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree7$tip.label)

#Remove node labels, or the model will fail
bee.tree7$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree7, nodes = "TIPS", scale = TRUE)
A7 <- solve(inv.phylo$Ainv)
rownames(A7) <- rownames(inv.phylo$Ainv)
isSymmetric(A7, check.attributes = FALSE)





dataformcmc7=data7
dataformcmc7$Species
#Let's fix it
dataformcmc7$Species<-stri_replace_first_regex(dataformcmc7$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A7),dataformcmc7$Species)
#All set
#BRAINS-POPULATION TRENDS----
#Brain weight
plot(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc7)
abline(lm(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc7))
summary(lm(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc7))

plot(Bartomeus.Estimate ~ IT, data = dataformcmc7)
abline(lm(Bartomeus.Estimate ~ IT, data = dataformcmc7))
summary(lm(Bartomeus.Estimate ~ IT, data = dataformcmc7))



brm.bartomeuspref.brain<-brm(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc7,
                           cores=4,
                           family = gaussian, cov_ranef = list("Species" = A7),
                           control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeuspref.brain
pp_check(brm.bartomeuspref.brain,nsamples=1000)
bayes_R2(brm.bartomeuspref.brain)
icc(brm.bartomeuspref.brain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeuspref.brain)


#Residuals
plot(Bartomeus.Estimate ~ residuals, data = dataformcmc7)
abline(lm(Bartomeus.Estimate ~ residuals, data = dataformcmc7))

brm.bartomeuspref.residuals<-brm(Bartomeus.Estimate ~ residuals, data = dataformcmc7,
                             cores=4,
                             family = gaussian, cov_ranef = list("Species" = A7),
                             control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeuspref.residuals
pp_check(brm.bartomeuspref.residuals,nsamples=1000)
bayes_R2(brm.bartomeuspref.residuals)
icc(brm.bartomeuspref.brain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeuspref.residuals)
#POPULATION TRENDS-HABITAT PREFERENCE-----
plot(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref)
abline(glm(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref))
summary(glm(Bartomeus.Estimate ~ Pasture.and.Crops,data = data.means.pref))

brm.habpref.pasture<-brm(Bartomeus.Estimate ~ Pasture.and.Crops, data = dataformcmc7,
                                 cores=4,
                                 family = gaussian, cov_ranef = list("Species" = A7),
                                 control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.pasture$fit$b_Intercept
pp_check(brm.habpref.pasture,nsamples=1000)
bayes_R2(brm.habpref.pasture)
icc(brm.habpref.pasture, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.pasture)


plot(Bartomeus.Estimate ~ Forests, data = dataformcmc7)
abline(glm(Bartomeus.Estimate ~ Forests, data = dataformcmc7))
brm.habpref.forests<-brm(Bartomeus.Estimate ~ Forests, data = dataformcmc7,
                         cores=4,
                         family = gaussian, cov_ranef = list("Species" = A7),
                         control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.forests
pp_check(brm.habpref.forests,nsamples=1000)
bayes_R2(brm.habpref.forests)
icc(brm.habpref.pasture, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.forests)

plot(Bartomeus.Estimate ~ Urban,data = data.means.pref)
abline(glm(Bartomeus.Estimate ~ Urban,data = data.means.pref))
summary(glm(Bartomeus.Estimate ~ Urban,data = data.means.pref))
visreg(glm(Bartomeus.Estimate ~ Urban,data = data.means.pref))

plot(Bartomeus.Estimate ~ Urban, data = dataformcmc7)
abline(glm(Bartomeus.Estimate ~ Urban, data = dataformcmc7))
brm.habpref.urban<-brm(Bartomeus.Estimate ~ Urban, data = dataformcmc7,
                         cores=4,
                         family = gaussian, cov_ranef = list("Species" = A7),
                         control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.urban$
pp_check(brm.habpref.urban,nsamples=1000)
bayes_R2(brm.habpref.urban)
icc(brm.habpref.urban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.urban)

#BRAIN SIZES-HABITAT PREFERENCE----
#Urban
plot(Urban ~ Brain.weight,data = data.means.pref)
abline(glm(Urban ~ Brain.weight,data = data.means.pref))

plot(Urban ~ IT,data = data.means.pref)
abline(glm(Urban ~ IT,data = data.means.pref))

visreg(glm(Urban ~ Brain.weight,data = data.means.pref))
visreg(glm(Urban ~ IT,data = data.means.pref))

summary(glm(Urban ~ Brain.weight,data = data.means.pref))
summary(glm(Urban ~ IT,data = data.means.pref))

brm.habpref.brainurban<-brm(Urban ~ Brain.weight, data = dataformcmc7,
                       cores=4,
                       family = gaussian, cov_ranef = list("Species" = A7),
                       control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.brainurban
pp_check(brm.habpref.brainurban,nsamples=1000)
bayes_R2(brm.habpref.brainurban)
icc(brm.habpref.brainurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.brainurban)


brm.habpref.ITurban<-brm(Urban ~ IT, data = dataformcmc7,
                            cores=4,
                            family = gaussian, cov_ranef = list("Species" = A7),
                            control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.ITurban
pp_check(brm.habpref.ITurban,nsamples=1000)
bayes_R2(brm.habpref.ITurban)
icc(brm.habpref.ITurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.ITurban)


plot(Urban ~ residuals,data = data.means.pref, xlim=c(-0.5,0.7))
plot(Urban ~ residuals,data = habitat.brain, xlim=c(-0.5,0.7))

abline(glm(Urban ~ residuals,data = data.means.pref))
visreg(glm(Urban ~ residuals,data = data.means.pref))
summary(glm(Urban ~ residuals,data = data.means.pref))


brm.habpref.resurban<-brm(Urban ~ residuals, data = dataformcmc7,
                            cores=4,
                            family = gaussian, cov_ranef = list("Species" = A7),
                            control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.resurban
pp_check(brm.habpref.resurban,nsamples=1000)
bayes_R2(brm.habpref.resurban)
icc(brm.habpref.brainurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.resurban)

#Forest
plot(Forests ~ Brain.weight,data = data.means.pref)
abline(glm(Forests ~ Brain.weight,data = data.means.pref))

plot(Forests ~ IT,data = data.means.pref)
abline(glm(Forests ~ IT,data = data.means.pref))

visreg(glm(Forests ~ Brain.weight,data = data.means.pref))
summary(glm(Forests ~ Brain.weight,data = data.means.pref))

visreg(glm(Forests ~ IT,data = data.means.pref))
summary(glm(Forests ~ IT,data = data.means.pref))


brm.habpref.brainforest<-brm(Forests ~ Brain.weight, data = dataformcmc7,
                          cores=4,
                          family = gaussian, cov_ranef = list("Species" = A7),
                          control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.brainforest
pp_check(brm.habpref.brainforest,nsamples=1000)
bayes_R2(brm.habpref.brainforest)
icc(brm.habpref.brainurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.brainforest)

brm.habpref.ITforest<-brm(Forests ~ IT, data = dataformcmc7,
                             cores=4,
                             family = gaussian, cov_ranef = list("Species" = A7),
                             control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.ITforest
pp_check(brm.habpref.ITforest,nsamples=1000)
bayes_R2(brm.habpref.ITforest)
icc(brm.habpref.ITforest, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.ITforest)


plot(Forests ~ residuals,data = data.means.pref)
abline(glm(Forests ~ residuals,data = data.means.pref))
visreg(glm(Forests ~ residuals,data = data.means.pref))
summary(glm(Forests ~ residuals,data = data.means.pref))

brm.habpref.resforest<-brm(Forests ~ residuals, data = dataformcmc7,
                             cores=4,
                             family = gaussian, cov_ranef = list("Species" = A7),
                             control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.resforest
pp_check(brm.habpref.resforest,nsamples=1000)
bayes_R2(brm.habpref.resforest)
icc(brm.habpref.resforest, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.resforest)

#RE-DO merging 2 by 2-------
bartomeus.numeric.trends
habpref
species.brains1
subset(species.brains1, subset=(species.brains1$Species=="Ptilothrix bombiformis"))
#My data of brains needs filtering----
species.brains1

data.filtered2<-species.brains1
boxplot(Brain.weight~Species,data = species.brains1, las= 2,cex.axis = 0.6)
boxplot(Brain.weight~Species,data = data.filtered2, las= 2,cex.axis = 0.6)
View(species.brains1)
subset(species.brains1, subset = (Species == "Andrena barbilabris"))
mean(subset(species.brains1, subset = (Species == "Andrena barbilabris"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Andrena barbilabris"))$Brain.weight)
data.filtered2<-subset(data.filtered2, subset = !((Species == "Andrena barbilabris")&(Brain.weight==0.296)))
subset(data.filtered2, subset = (Species == "Andrena barbilabris"))


subset(data.filtered2, subset = (Species == "Andrena dunningi"))
subset(species.brains1, subset = (Species == "Andrena dunningi"))
mean(subset(species.brains1, subset = (Species == "Andrena dunningi"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Andrena dunningi"))$Brain.weight)
data.filtered2<-subset(data.filtered2, subset = !((Species == "Andrena dunningi")&(Brain.weight==0.600)))

subset(species.brains1, subset = (Species == "Andrena fragilis"))

subset(data.filtered2, subset = (Species == "Augochlorella aurata"))
subset(species.brains1, subset = (Species == "Augochlorella aurata"))

mean(subset(species.brains1, subset = (Species == "Augochlorella aurata"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Augochlorella aurata"))$Brain.weight)

data.filtered2<-subset(data.filtered2, subset = !((Species == "Augochlorella aurata")&(Brain.weight==0.705)))


subset(data.filtered2, subset = (Species == "Bombus impatiens"))

subset(data.filtered2, subset = (Species == "Bombus pascuorum"))
subset(species.brains1, subset = (Species == "Bombus pascuorum"))
mean(subset(species.brains1, subset = (Species == "Bombus pascuorum"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Bombus pascuorum"))$Brain.weight)

data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pascuorum")&(Brain.weight==4.937)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pascuorum")&(Brain.weight==3.932)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pascuorum")&(Brain.weight==3.871)))


subset(data.filtered2, subset = (Species == "Bombus pratorum"))

subset(species.brains1, subset = (Species == "Bombus pratorum"))
mean(subset(species.brains1, subset = (Species == "Bombus pratorum"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Bombus pratorum"))$Brain.weight)

data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pratorum")&(Brain.weight==3.626)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pratorum")&(Brain.weight==3.278)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pratorum")&(Brain.weight==3.121)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pratorum")&(Brain.weight==1.070)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus pratorum")&(Brain.weight==1.300)))

subset(data.filtered2, subset = (Species == "Bombus terrestris"))
subset(species.brains1, subset = (Species == "Bombus terrestris"))
mean(subset(species.brains1, subset = (Species == "Bombus terrestris"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Bombus terrestris"))$Brain.weight)
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus terrestris")&(Brain.weight==5.838)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus terrestris")&(Brain.weight==5.564)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus terrestris")&(Brain.weight==6.370)))
boxplot(subset(species.brains1, subset = (Species == "Bombus terrestris"))$Brain.weight)
boxplot(subset(data.filtered2, subset = (Species == "Bombus terrestris"))$Brain.weight)


subset(data.filtered2, subset = (Species == "Bombus hortorum"))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus hortorum")))

subset(data.filtered2, subset = (Species == "Bombus lapidarius"))
subset(species.brains1, subset = (Species == "Bombus lapidarius"))
subset(species.brains1, subset = (Species == "Bombus lapidarius"))$Brain.weight
data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus lapidarius")))


subset(data.filtered2, subset = (Species == "Bombus bimaculatus"))
subset(species.brains1, subset = (Species == "Bombus bimaculatus"))


data.filtered2<-subset(data.filtered2, subset = !((Species == "Bombus bimaculatus")))


subset(data.filtered2, subset = (Species == "Halictus ligatus"))
subset(species.brains1, subset = (Species == "Halictus ligatus"))
mean(subset(species.brains1, subset = (Species == "Halictus ligatus"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Halictus ligatus"))$Brain.weight)
data.filtered2<-subset(data.filtered2, subset = !((Species == "Halictus ligatus")&(Brain.weight==0.395)))


subset(data.filtered2, subset = (Species == "Lasioglossum calceatum"))
subset(data.filtered2, subset = (Species == "Osmia caerulescens"))
subset(species.brains1, subset = (Species == "Osmia caerulescens"))
mean(subset(species.brains1, subset = (Species == "Osmia caerulescens"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Osmia caerulescens"))$Brain.weight)



data.filtered2<-subset(data.filtered2, subset = !((Species == "Osmia caerulescens")&(Brain.weight==1.530)))

subset(data.filtered2, subset = (Species == "Xylocopa virginica"))
subset(species.brains1, subset = (Species == "Xylocopa virginica"))
mean(subset(species.brains1, subset = (Species == "Xylocopa virginica"))$Brain.weight)
sd(subset(species.brains1, subset = (Species == "Xylocopa virginica"))$Brain.weight)



data.filtered2<-subset(data.filtered2, subset = !((Species == "Xylocopa virginica")&(Brain.weight==6.613)))
data.filtered2<-subset(data.filtered2, subset = !((Species == "Xylocopa virginica")&(Brain.weight==5.787)))


data.filtered2





#Now we do means in our brains data
weights.mean<-data.frame(aggregate(Brain.weight ~ Species, data = data.filtered2, FUN = mean))
IT.mean<-data.frame(aggregate(IT ~ Species, data = data.filtered2, FUN = mean))
wit.mean<-merge(weights.mean,IT.mean)

#We add residuals, TRY TO USE PHYLOGENETICS HERE--------
plot(Brain.weight ~ IT, data=wit.mean)
abline(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(log(Brain.weight) ~ log(IT), data=wit.mean))
lm.data.means<-lm(log(Brain.weight) ~ log(IT), data=wit.mean)
lm.data.means$residuals
lm.data.means$residuals
wit.mean$residuals<-lm.data.means$residuals

lm.data.means$model
wit.mean



#Data merging

habitat.brain<-merge(wit.mean,habpref)
brain.trend<-merge(wit.mean,bartomeus.numeric.trends)
trend.habitat<-merge(bartomeus.numeric.trends,habpref)



datatable<-merge(merge(brain.trend, habitat.brain, all.x = TRUE),trend.habitat, all.x = TRUE)

#write.csv(datatable, "datatable.csv")
#pre-analysis
par(mfrow=(c(2,2)))

plot(Bartomeus.Estimate ~ Urban,data=trend.habitat)
abline(glm(Bartomeus.Estimate ~ Urban, data=trend.habitat))

plot(Bartomeus.Estimate ~ Forests,data=trend.habitat)
abline(glm(Bartomeus.Estimate ~ Forests, data=trend.habitat))

plot(Bartomeus.Estimate ~ Brain.weight, data=brain.trend)
abline(glm(Bartomeus.Estimate ~ Brain.weight, data=brain.trend))

plot(Bartomeus.Estimate ~ residuals, data=brain.trend)
abline(glm(Bartomeus.Estimate ~ residuals, data=brain.trend))

#POPULATION TRENDS - HABITAT PREFERENCE--------

#phylogenetics
trend.habitat$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data8<-trend.habitat

species=c("Agapostemon_virescens", "Andrena_carlini", "Andrena_cressonii",       
          "Andrena_erigeniae", "Andrena_nasonii", "Andrena_perplexa",        
          "Augochlora_pura", "Augochlorella_aurata", "Bombus_bimaculatus",      
          "Bombus_fervidus", "Bombus_griseocollis", "Calliopsis_andreniformis",
          "Ceratina_strenua", "Halictus_confusus", "Halictus_rubicundus",     
          "Lasioglossum_bruneri", "Lasioglossum_coriaceum", "Lasioglossum_cressonii",  
          "Lasioglossum_imitatum", "Lasioglossum_oblongum", "Lasioglossum_pectorale",  
          "Lasioglossum_pilosum", "Lasioglossum_tegulare", "Lasioglossum_versatum",   
          "Megachile_brevis", "Megachile_mendica", "Melissodes_bimaculata",   
          "Nomada_pygmaea", "Osmia_atriventris", "Osmia_bucephala",         
          "Osmia_pumila", "Xylocopa_virginica")

#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree8=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                     "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                     "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                     "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                     "Ctenocolletes", "Alocandrena", "Megandrena",      
                                     "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                     "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                     "Perdita", "Clavipanurgus", "Panurginus",        
                                     "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                     "Arhysosage", "Callonychium", "Cerceris",        
                                     "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                     "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                     "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                     "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                     "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                     "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                     "Samba", "Capicola", "Hesperapis",      
                                     "Eremaphanta", "Melitta", "Redivivoides",    
                                     "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                     "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                     "Sphecodopsis", "Pasites", "Oreopasites",     
                                     "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                     "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                     "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                     "Hexepeolus", "Neolarra", "Biastes",         
                                     "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                     "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                     "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                     "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                     "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                     "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                     "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                     "Peponapis", "Xenoglossa", "Tetraloniella",   
                                     "Tetralonia", "Svastra", "Martinapis",      
                                     "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                     "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                     "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                     "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                     "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                     "Aglae", "Eulaema", "Eufriesea",            
                                     "Tetragonilla", "Tetragonula", "Platytrigona",    
                                     "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                     "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                     "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                     "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                     "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                     "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                     "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                     "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                     "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                     "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                     "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                     "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                     "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                     "Ctenoplectra", "Macrogalea", "Allodapula",      
                                     "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                     "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                     "Allodape", "Fideliopsis", "Fidelia",         
                                     "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                     "Dioxys", "Noteriades", "Radoszkowskiana", 
                                     "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                     "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                     "Haetosmia", "Wainia", "Hoplosmia",           
                                     "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                     "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                     "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                     "Dianthidium", "Anthidiellum", "Paranthidium",  
                                     "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                     "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                     "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                     "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                     "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                     "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                     "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                     "Xenochlora", "Megaloptidia",    
                                     "Dinagapostemon", "Rhinetula",       
                                     "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                     "Eupetersia", "Mexalictus", "Patellapis",      
                                     "Thrincohalictus", "Homalictus",   
                                     "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                     "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                     "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                     "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                     "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                     "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                     "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                     "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                     "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                     "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                     "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                     "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                     "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                     "Hemicotelles", "Mourecotelles", 
                                     "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                     "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                     "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                     "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                     "Eucera", "Chilicola", "Duckeanthidium",
                                     "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                     "Panurgus","Anthidium","Anthophora","Sphecodes",
                                     "Augochloropsis","Colletes","Hylaeus"
))


plot(bee.tree8)
nodelabels()
tiplabels()
unique(data8$Species)


#add dummy species labels
bee.tree8$tip.label<-paste(bee.tree8$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree8<-add.species.to.genus(bee.tree8,species[i],
                                    where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree8$tip.label)
bee.tree8<-drop.tip(bee.tree8,bee.tree8$tip.label[ii])
#Our tree
plot(bee.tree8, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree8$tip.label)

#Remove node labels, or the model will fail
bee.tree8$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree8, nodes = "TIPS", scale = TRUE)
A8 <- solve(inv.phylo$Ainv)
rownames(A8) <- rownames(inv.phylo$Ainv)
isSymmetric(A8, check.attributes = FALSE)





dataformcmc8=data8
dataformcmc8$Species
#Let's fix it
dataformcmc8$Species<-stri_replace_first_regex(dataformcmc8$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A8),dataformcmc8$Species)



plot(Bartomeus.Estimate ~ Forests, data = dataformcmc8)

abline(glm(Bartomeus.Estimate ~ Forests, data = dataformcmc8))
brm.habpref.forests2<-brm(Bartomeus.Estimate ~ Forests, data = dataformcmc8,
                         cores=4,
                         family = gaussian, cov_ranef = list("Species" = A8),
                         control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.forests2
pp_check(brm.habpref.forests2,nsamples=1000)
bayes_R2(brm.habpref.forests2)

marginal_effects(brm.habpref.forests2)

plot(Bartomeus.Estimate ~ Urban, data = dataformcmc8)
abline(glm(Bartomeus.Estimate ~ Urban, data = dataformcmc8))
brm.habpref.urban2<-brm(Bartomeus.Estimate ~ Urban, data = dataformcmc8,
                       cores=4,
                       family = gaussian, cov_ranef = list("Species" = A8),
                       control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.urban2
    pp_check(brm.habpref.urban,nsamples=1000)
bayes_R2(brm.habpref.urban2)

marginal_effects(brm.habpref.urban2)

#BRAIN - POPULATION TRENDS----
brain.trend
#phylogeny
brain.trend$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data9<-brain.trend

species=c("Agapostemon_sericeus", "Agapostemon_virescens", "Andrena_barbilabris",     
          "Andrena_carlini", "Andrena_crataegi", "Andrena_dunningi",        
          "Andrena_fragilis", "Andrena_frigida", "Andrena_hippotes",        
          "Andrena_hirticincta", "Andrena_milwaukeensis", "Andrena_miserabilis",     
          "Andrena_nasonii", "Andrena_nubecula", "Andrena_pruni",           
          "Andrena_rugosa", "Andrena_simplex", "Andrena_vicina",          
          "Augochlora_pura", "Augochlorella_aurata", "Augochloropsis_metallica",
          "Bombus_griseocollis", "Bombus_impatiens", "Bombus_ternarius",        
          "Bombus_vagans", "Calliopsis_andreniformis", "Ceratina_calcarata",      
          "Ceratina_strenua", "Colletes_thoracicus", "Halictus_confusus",       
          "Halictus_ligatus", "Halictus_rubicundus", "Hylaeus_mesillae",        
          "Hylaeus_modestus", "Lasioglossum_coriaceum", "Megachile_campanulae",    
          "Megachile_centuncularis", "Megachile_gemula", "Megachile_mendica",       
          "Megachile_pugnata", "Megachile_texana", "Melissodes_bimaculata",   
          "Nomada_cressonii", "Nomada_luteoloides", "Osmia_atriventris",       
          "Osmia_bucephala", "Osmia_lignaria", "Osmia_pumila",            
          "Sphecodes_ranunculi", "Xylocopa_virginica")

#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree9=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                     "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                     "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                     "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                     "Ctenocolletes", "Alocandrena", "Megandrena",      
                                     "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                     "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                     "Perdita", "Clavipanurgus", "Panurginus",        
                                     "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                     "Arhysosage", "Callonychium", "Cerceris",        
                                     "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                     "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                     "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                     "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                     "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                     "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                     "Samba", "Capicola", "Hesperapis",      
                                     "Eremaphanta", "Melitta", "Redivivoides",    
                                     "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                     "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                     "Sphecodopsis", "Pasites", "Oreopasites",     
                                     "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                     "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                     "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                     "Hexepeolus", "Neolarra", "Biastes",         
                                     "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                     "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                     "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                     "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                     "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                     "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                     "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                     "Peponapis", "Xenoglossa", "Tetraloniella",   
                                     "Tetralonia", "Svastra", "Martinapis",      
                                     "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                     "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                     "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                     "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                     "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                     "Aglae", "Eulaema", "Eufriesea",            
                                     "Tetragonilla", "Tetragonula", "Platytrigona",    
                                     "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                     "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                     "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                     "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                     "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                     "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                     "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                     "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                     "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                     "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                     "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                     "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                     "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                     "Ctenoplectra", "Macrogalea", "Allodapula",      
                                     "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                     "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                     "Allodape", "Fideliopsis", "Fidelia",         
                                     "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                     "Dioxys", "Noteriades", "Radoszkowskiana", 
                                     "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                     "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                     "Haetosmia", "Wainia", "Hoplosmia",           
                                     "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                     "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                     "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                     "Dianthidium", "Anthidiellum", "Paranthidium",  
                                     "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                     "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                     "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                     "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                     "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                     "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                     "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                     "Xenochlora", "Megaloptidia",    
                                     "Dinagapostemon", "Rhinetula",       
                                     "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                     "Eupetersia", "Mexalictus", "Patellapis",      
                                     "Thrincohalictus", "Homalictus",   
                                     "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                     "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                     "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                     "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                     "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                     "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                     "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                     "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                     "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                     "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                     "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                     "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                     "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                     "Hemicotelles", "Mourecotelles", 
                                     "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                     "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                     "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                     "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                     "Eucera", "Chilicola", "Duckeanthidium",
                                     "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                     "Panurgus","Anthidium","Anthophora"
))


plot(bee.tree9)
nodelabels()
tiplabels()
unique(data9$Species)


#add dummy species labels
bee.tree9$tip.label<-paste(bee.tree9$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree9<-add.species.to.genus(bee.tree9,species[i],
                                    where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree9$tip.label)
bee.tree9<-drop.tip(bee.tree9,bee.tree9$tip.label[ii])
#Our tree
plot(bee.tree9, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree9$tip.label)

#Remove node labels, or the model will fail
bee.tree9$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree9, nodes = "TIPS", scale = TRUE)
A9 <- solve(inv.phylo$Ainv)
rownames(A9) <- rownames(inv.phylo$Ainv)
isSymmetric(A9, check.attributes = FALSE)





dataformcmc9=data9
dataformcmc9$Species
#Let's fix it
dataformcmc9$Species<-stri_replace_first_regex(dataformcmc9$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A9),dataformcmc9$Species)

plot(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc9)
abline(lm(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc9))

plot(Bartomeus.Estimate ~ IT, data = dataformcmc9)
abline(lm(Bartomeus.Estimate ~ IT, data = dataformcmc9))
summary(lm(Bartomeus.Estimate ~ IT, data = dataformcmc9))



brm.bartomeuspref.brain2<-brm(Bartomeus.Estimate ~ Brain.weight, data = dataformcmc9,
                             cores=4,
                             family = gaussian, cov_ranef = list("Species" = A9),
                             control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeuspref.brain2
pp_check(brm.bartomeuspref.brain,nsamples=1000)
bayes_R2(brm.bartomeuspref.brain2)
icc(brm.bartomeuspref.brain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeuspref.brain2)


#Residuals
plot(Bartomeus.Estimate ~ residuals, data = dataformcmc9)
abline(lm(Bartomeus.Estimate ~ residuals, data = dataformcmc9))

brm.bartomeuspref.residuals2<-brm(Bartomeus.Estimate ~ residuals, data = dataformcmc9,
                                 cores=4,
                                 family = gaussian, cov_ranef = list("Species" = A9),
                                 control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeuspref.residuals2
pp_check(brm.bartomeuspref.residuals,nsamples=1000)
bayes_R2(brm.bartomeuspref.residuals2)
icc(brm.bartomeuspref.brain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeuspref.residuals2)




plot(Bartomeus.Estimate ~ IT, data = dataformcmc9)
abline(lm(Bartomeus.Estimate ~ IT, data = dataformcmc9))

brm.bartomeuspref.IT2<-brm(Bartomeus.Estimate ~ IT, data = dataformcmc9,
                                  cores=4,
                                  family = gaussian, cov_ranef = list("Species" = A9),
                                  control = list(adapt_delta = 0.99,max_treedepth=15))


brm.bartomeuspref.IT2
pp_check(brm.bartomeuspref.residuals,nsamples=1000)
bayes_R2(brm.bartomeuspref.residuals2)
icc(brm.bartomeuspref.brain, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.bartomeuspref.residuals2)


#BRAIN SIZES - HABITAT PREFERENCE-------
habitat.brain

#We remove apis mellifera because is a managed species
habitat.brain<-subset(habitat.brain, subset = !(habitat.brain$Species == "Apis mellifera"))


plot(Urban~residuals,data=habitat.brain)
abline(glm(Urban~residuals,data=habitat.brain))
summary(glm(Urban~residuals,data=habitat.brain))

plot(Urban~residuals,data=dataformcmc7)
abline(glm(Urban~residuals,data=dataformcmc7))
summary(glm(Urban~residuals,data=dataformcmc7))

#phylogenetics
habitat.brain$Species
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
data10<-habitat.brain
unique(data10$Species)


species=c("Agapostemon_virescens","Andrena_carlini","Andrena_nasonii", "Augochlora_pura", 
          "Augochlorella_aurata", "Bombus_griseocollis","Calliopsis_andreniformis", "Ceratina_strenua",
          "Halictus_confusus","Halictus_rubicundus","Lasioglossum_coriaceum","Megachile_mendica",
          "Melissodes_bimaculata", "Osmia_atriventris", "Osmia_bucephala","Osmia_pumila",
          "Ptilothrix_bombiformis",
          "Xylocopa_virginica")



#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
bee.tree10=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                     "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                     "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                     "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                     "Ctenocolletes", "Alocandrena", "Megandrena",      
                                     "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                     "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                     "Perdita", "Clavipanurgus", "Panurginus",        
                                     "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                     "Arhysosage", "Callonychium", "Cerceris",        
                                     "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                     "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                     "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                     "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                     "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                     "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                     "Samba", "Capicola", "Hesperapis",      
                                     "Eremaphanta", "Melitta", "Redivivoides",    
                                     "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                     "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                     "Sphecodopsis", "Pasites", "Oreopasites",     
                                     "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                     "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                     "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                     "Hexepeolus", "Neolarra", "Biastes",         
                                     "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                     "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                     "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                     "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                     "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                     "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                     "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                     "Peponapis", "Xenoglossa", "Tetraloniella",   
                                     "Tetralonia", "Svastra", "Martinapis",      
                                     "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                     "Diadasia", "Alepidosceles", "Diadasina",       
                                     "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                     "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                     "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                     "Aglae", "Eulaema", "Eufriesea",            
                                     "Tetragonilla", "Tetragonula", "Platytrigona",    
                                     "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                     "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                     "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                     "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                     "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                     "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                     "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                     "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                     "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                     "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                     "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                     "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                     "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                     "Ctenoplectra", "Macrogalea", "Allodapula",      
                                     "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                     "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                     "Allodape", "Fideliopsis", "Fidelia",         
                                     "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                     "Dioxys", "Noteriades", "Radoszkowskiana", 
                                     "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                     "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                     "Haetosmia", "Wainia", "Hoplosmia",           
                                     "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                     "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                     "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                     "Dianthidium", "Anthidiellum", "Paranthidium",  
                                     "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                     "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                     "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                     "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                     "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                     "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                     "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                     "Xenochlora", "Megaloptidia",    
                                     "Dinagapostemon", "Rhinetula",       
                                     "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                     "Eupetersia", "Mexalictus", "Patellapis",      
                                     "Thrincohalictus", "Homalictus",   
                                     "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                     "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                     "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                     "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                     "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                     "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                     "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                     "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                     "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                     "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                     "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                     "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                     "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                     "Hemicotelles", "Mourecotelles", 
                                     "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                     "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                     "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                     "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides",
                                     "Eucera", "Chilicola", "Duckeanthidium",
                                     "Tachysphex", "Dasypoda", "Rhodanthidium","Apis",
                                     "Panurgus","Anthidium","Anthophora","Nomada","Sphecodes",
                                     "Augochloropsis","Colletes","Hylaeus"
))


plot(bee.tree10)
nodelabels()
tiplabels()


#add dummy species labels
bee.tree10$tip.label<-paste(bee.tree10$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree10<-add.species.to.genus(bee.tree10,species[i],
                                    where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree10$tip.label)
bee.tree10<-drop.tip(bee.tree10,bee.tree10$tip.label[ii])
#Our tree
plot(bee.tree10, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree10$tip.label)

#Remove node labels, or the model will fail
bee.tree10$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree10, nodes = "TIPS", scale = TRUE)
A10 <- solve(inv.phylo$Ainv)
rownames(A10) <- rownames(inv.phylo$Ainv)
isSymmetric(A10, check.attributes = FALSE)





dataformcmc10=data10
dataformcmc10$Species
#Let's fix it
dataformcmc10$Species<-stri_replace_first_regex(dataformcmc10$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A10),dataformcmc10$Species)



brm.habpref.brainurban2<-brm(Urban ~ Brain.weight, data = dataformcmc10,
                            cores=4,
                            family = gaussian, cov_ranef = list("Species" = A10),
                            control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.brainurban2
pp_check(brm.habpref.brainurban,nsamples=1000)
bayes_R2(brm.habpref.brainurban2)
icc(brm.habpref.brainurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.brainurban2)


brm.habpref.ITurban2<-brm(Urban ~ IT, data = dataformcmc10,
                         cores=4,
                         family = gaussian, cov_ranef = list("Species" = A10),
                         control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.ITurban2
pp_check(brm.habpref.ITurban,nsamples=1000)
bayes_R2(brm.habpref.ITurban2)
icc(brm.habpref.ITurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.ITurban2)



brm.habpref.resurban2<-brm(Urban ~ residuals, data = dataformcmc10,
                          cores=4,
                          family = gaussian, cov_ranef = list("Species" = A10),
                          control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.resurban2
pp_check(brm.habpref.resurban,nsamples=1000)
bayes_R2(brm.habpref.resurban2)
icc(brm.habpref.brainurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.resurban2)

#Forest

brm.habpref.brainforest2<-brm(Forests ~ Brain.weight, data = dataformcmc10,
                             cores=4,
                             family = gaussian, cov_ranef = list("Species" = A10),
                             control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.brainforest2
pp_check(brm.habpref.brainforest,nsamples=1000)
bayes_R2(brm.habpref.brainforest2)
icc(brm.habpref.brainurban, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.brainforest2)

brm.habpref.ITforest2<-brm(Forests ~ IT, data = dataformcmc10,
                          cores=4,
                          family = gaussian, cov_ranef = list("Species" = A10),
                          control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.ITforest2
pp_check(brm.habpref.ITforest,nsamples=1000)
bayes_R2(brm.habpref.ITforest2)
icc(brm.habpref.ITforest, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.ITforest2)

brm.habpref.resforest2<-brm(Forests ~ residuals, data = dataformcmc10,
                           cores=4,
                           family = gaussian, cov_ranef = list("Species" = A10),
                           control = list(adapt_delta = 0.99,max_treedepth=15))

brm.habpref.resforest2
pp_check(brm.habpref.resforest,nsamples=1000)
bayes_R2(brm.habpref.resforest2)
icc(brm.habpref.resforest, re.form = NULL, typical = "mean",
    prob = 0.89, ppd = FALSE, adjusted = FALSE)

marginal_effects(brm.habpref.resforest2)




#Figures google drive-----

#Figure 1
plot(bee.tree, cex = 0.6, main= "Phylogenetic tree", sub="Modification and polytomies added from Hedtke et al. 2013")

#Figure 2
par(mfrow=c(2,2))
boxplot(Brain.weight ~ f.trend,data1, main= "Brain weight by population trends (a)", 
        xlab="Population trend", ylab="Brain weight (mg)", las = 1)

plot(numeric.trend ~ Brain.weight, data = incdec.data, main="Brain weight by population trends (b)", xlab="Brain weight", ylab = "Declining population/ Increasing population", yaxt='n')
fit<-marginal_effects(brm.brainweight)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "purple")
lines(fits$Brain.weight, fits$upper__, col = "purple")




boxplot(residuals ~ f.trend,data1, main= "Brain weight - body size residuals by population trends (c)", 
        xlab="Population trend", ylab="Brain weight- body size residuals", las = 1)


plot(numeric.trend ~ residuals, data = incdec.data, main="Brain weight - body size residuals by population trends (d)", xlab="Brain weight- body size residuals", ylab = "Declining population/ Increasing population", yaxt='n')
fit<-marginal_effects(brm.residuals)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$estimate__, lwd=2)
lines(fits$residuals, fits$lower__, col = "purple")
lines(fits$residuals, fits$upper__, col = "purple")

par(mfrow=c(1,1))



#Figure 3
marginal_effects(brm.scheper.brain)

par(mfrow=c(2,2))
plot(original.trends ~ Brain.weight, data = data.scheper, main="Brain weight by population trends (Scheper)", xlab="Brain weight", ylab = "Declining population/ Increasing population")
abline(lm(original.trends ~ Brain.weight, data = data.scheper))
fit<-marginal_effects(brm.scheper.brain)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "purple")
lines(fits$Brain.weight, fits$upper__, col = "purple")

plot(Bartomeus.Estimate ~ Brain.weight, data = data.nacho, main="Brain weight by population trends (Bartomeus)", xlab="Brain weight", ylab = "Declining population/ Increasing population", las=1)
abline(glm(Bartomeus.Estimate ~ Brain.weight, data = data.nacho))
fit<-marginal_effects(brm.bartomeus.brain)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "purple")
lines(fits$Brain.weight, fits$upper__, col = "purple")


plot(original.trends ~ residuals, data = data.scheper, main="Brain weight - body size residuals by population trends (Scheper)", xlab="Brain weight- body size residuals", ylab = "Declining population/ Increasing population")
abline(lm(original.trends ~ residuals, data = data.scheper))
fit<-marginal_effects(brm.scheper.residuals)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$estimate__, lwd=2)
lines(fits$residuals, fits$lower__, col = "purple")
lines(fits$residuals, fits$upper__, col = "purple")

plot(Bartomeus.Estimate ~ residuals, data = data.nacho, main="Brain weight - body size residuals by population trends (Bartomeus)", xlab="Brain weight- body size residuals", ylab = "Declining population/ Increasing population")
abline(lm(Bartomeus.Estimate ~ residuals, data = data.nacho))
fit<-marginal_effects(brm.bartomeus.residuals)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$estimate__, lwd=2)
lines(fits$residuals, fits$lower__, col = "purple")
lines(fits$residuals, fits$upper__, col = "purple")

#New figures google drive----
#Figure 1
plot(bee.tree7, cex = 1.5, main= "Phylogenetic tree", sub="Modification and polytomies added from Hedtke et al. 2013")


#Figure 2
par(mfrow=c(2,2))

plot(Urban ~ residuals,data = dataformcmc7, ylab="Urban preference",las=1, main="Urban preference related to brain sizes\n Brain weight - body size residuals (a)", xlab="Brain weight - body size residuals")
fit<-marginal_effects(brm.habpref.resurban)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Urban ~ residuals,data = dataformcmc7)
lines(fits$residuals, fits$estimate__, lwd=2, col = "Blue")
lines(c(-0.5,0.7),c(1.04,1.04), col = "black")
lines(c(-0.5,0.7),c(-0.04,-0.04), col = "black")

plot(Forests ~ residuals,data = dataformcmc7, las=1, main="Forest preference related to brain sizes\n Brain weight - Body size residuals (b)", ylab="Forest preference", xlab="Brain weight - body size residuals")
fit<-marginal_effects(brm.habpref.resforest)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
lines(c(-0.5,0.6),c(1.04,1.04), col = "black")
lines(c(-0.5,0.7),c(-0.04,-0.04), col = "black")

points(Forests ~ residuals,data = dataformcmc7)
lines(fits$residuals, fits$estimate__, lwd=2, col = "darkgreen")


plot(Urban ~ Brain.weight,data = dataformcmc7, ylab="Urban preference", las= 1, main="Urban preference related to brain sizes\n Absolute brain weight (c)", xlab="Absolute brain weight")
points(Urban ~ Brain.weight,data = dataformcmc7)
fit<-marginal_effects(brm.habpref.brainurban)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "grey30",lwd=2)
lines(fits$Brain.weight, fits$upper__, col = "grey30",lwd=2)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Urban ~ Brain.weight,data = dataformcmc7)
lines(fits$Brain.weight, fits$estimate__, lwd=2, col = "Blue")
lines(c(0,6),c(1.04,1.04), col = "black")
lines(c(0,6),c(-0.04,-0.04), col = "black")

plot(Forests ~ Brain.weight,data = dataformcmc7, ylab="Forest preference", las=1, main="Forest preference related to brain sizes\n Absolute brain weight (d)", xlab="Absolute brain weight")
fit<-marginal_effects(brm.habpref.brainforest)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Brain.weight, fits$upper__, col = "grey30",lwd = 2)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Forests ~ Brain.weight,data = dataformcmc7)
lines(fits$Brain.weight, fits$estimate__, lwd=2, col = "darkgreen")
lines(c(0,6),c(1.04,1.04), col = "black")
lines(c(0,6),c(-0.04,-0.04), col = "black")


par(mfrow=c(1,1))
dev.off()
#Figure 3
par(mfrow=c(2,2))
plot(Bartomeus.Estimate ~ Urban,data = dataformcmc7, xlab="Urban preference",las=1, ylab="", yaxt="n", main="Habitat preference related to population trends\n Urban preference (a)")
axis(2,cex.axis=0.85, las=1)
title(ylab="Population trends", line=3.1)
summary(glm(Bartomeus.Estimate ~ Urban,data = dataformcmc7))
fit<-marginal_effects(brm.habpref.urban)
fits<-as.data.frame(fit$Urban)
lines(fits$Urban, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Urban, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$Urban, rev(fits$Urban)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ Forests,data = dataformcmc7)
lines(fits$Urban, fits$estimate__, lwd=2, col = "Blue")

plot(Bartomeus.Estimate ~ Forests, xlab= "Forest preference",data = dataformcmc7, las=1, ylab="", yaxt="n", main="Habitat preference related to population trends\n Forest preference (b)")
axis(2,cex.axis=0.85, las=1)
fit<-marginal_effects(brm.habpref.forests)
fits<-as.data.frame(fit$Forests)
lines(fits$Forests, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Forests, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$Forests, rev(fits$Forests)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ Forests,data = dataformcmc7)
lines(fits$Forests, fits$estimate__, lwd=2, col = "darkgreen")

plot(Bartomeus.Estimate ~ Brain.weight, xlab="Absolute brain weight",data = dataformcmc7, las = 1, ylab="", yaxt="n", main="Brain size related to population trends\n Brain weight (c)")
axis(2,cex.axis=0.85, las=1)
title(ylab="Population trends", line=3.1)
fit<-marginal_effects(brm.bartomeuspref.brain)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Brain.weight, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ Forests,data = dataformcmc7)
lines(fits$Brain.weight, fits$estimate__, lwd=2)

plot(Bartomeus.Estimate ~ residuals, data = dataformcmc7, xlab="Brain weight - body size residuals",main = "Brain size related to population trends\n Brain - Body size residuals (d)", yaxt = "n")
axis(2,cex.axis=0.85, las=1)
fit<-marginal_effects(brm.bartomeuspref.residuals)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ residuals,data = dataformcmc7)
lines(fits$residuals, fits$estimate__, lwd=2)



dev.off()



temp<-as.data.frame(data7$Species)
colnames(temp)<-c("Species")
temp2<-merge(temp,
data.filtered)

summary(droplevels(temp2$Species))


#☻New figures google drive2------

#Figure 1
pdf("Figure1.pdf")
par(mfrow=c(2,2))

plot(Forests ~ residuals,data = dataformcmc10, las=1, main="Forest preference related to brain sizes\n Brain weight - Body size residuals (a)", ylab="Forest preference", xlab="Brain weight - body size residuals")
fit<-marginal_effects(brm.habpref.resforest2)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
lines(c(-0.6,0.6),c(1.04,1.04), col = "black")
lines(c(-0.5,0.99),c(-0.04,-0.04), col = "black")

points(Forests ~ residuals,data = dataformcmc10)
lines(fits$residuals, fits$estimate__, lwd=2, col = "darkgreen")

plot(Urban ~ residuals,data = dataformcmc10, ylab="Urban preference",las=1, main="Urban preference related to brain sizes\n Brain weight - body size residuals (b)", xlab="Brain weight - body size residuals")
fit<-marginal_effects(brm.habpref.resurban2)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Urban ~ residuals,data = dataformcmc10)
lines(fits$residuals, fits$estimate__, lwd=2, col = "Blue")
lines(c(-0.5,0.99),c(1.04,1.04), col = "black")
lines(c(-0.5,0.7),c(-0.04,-0.04), col = "black")

par(mfrow=c(1,1))

dev.off()
#Figure 2
pdf("Figure 2.pdf")
par(mfrow=c(2,2))

plot(Bartomeus.Estimate ~ Forests, xlab= "Forest preference",data = dataformcmc8, las=1, ylab="", yaxt="n", main="Preference related to population trends\n Forest preference (a)")
axis(2,cex.axis=0.85, las=1)
title(ylab="Population trends", line=3.1)
fit<-marginal_effects(brm.habpref.forests2)
fits<-as.data.frame(fit$Forests)
lines(fits$Forests, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Forests, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$Forests, rev(fits$Forests)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ Forests,data = dataformcmc8)
lines(fits$Forests, fits$estimate__, lwd=2, col = "darkgreen")

plot(Bartomeus.Estimate ~ Urban,data = dataformcmc8, xlab="Urban preference",las=1, ylab="", yaxt="n", main="Preference related to population trends\n Urban preference (b)")
axis(2,cex.axis=0.85, las=1)

fit<-marginal_effects(brm.habpref.urban2)
fits<-as.data.frame(fit$Urban)
lines(fits$Urban, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Urban, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$Urban, rev(fits$Urban)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ Urban,data = dataformcmc8)
lines(fits$Urban, fits$estimate__, lwd=2, col = "Blue")


plot(Bartomeus.Estimate ~ Brain.weight, xlab="Absolute brain weight",data = dataformcmc9, las = 1, ylab="", yaxt="n", main="Brain size related to population trends\n Brain weight (c)")
axis(2,cex.axis=0.85, las=1)
title(ylab="Population trends", line=3.1)
fit<-marginal_effects(brm.bartomeuspref.brain2)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Brain.weight, fits$upper__, col = "grey30", lwd = 2)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ Brain.weight,data = dataformcmc9)
lines(fits$Brain.weight, fits$estimate__, lwd=2)

plot(Bartomeus.Estimate ~ residuals, data = dataformcmc9, xlab="Brain weight - body size residuals",main = "Brain size related to population trends\n Brain - Body size residuals (d)", yaxt = "n",ylab="")
axis(2,cex.axis=0.85, las=1)

fit<-marginal_effects(brm.bartomeuspref.residuals2)
fits<-as.data.frame(fit$residuals)
lines(fits$residuals, fits$upper__, col = "grey30", lwd = 2)

lines(fits$residuals, fits$lower__, col = "grey30", lwd = 2)
polygon(c(fits$residuals, rev(fits$residuals)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Bartomeus.Estimate ~ residuals,data = dataformcmc9)
lines(fits$residuals, fits$estimate__, lwd=2)

dev.off()




#supp mat Figure 1

#Habitat preference - brains
#pdf("Brain sizes habitat preference tree.pdf")
plot(bee.tree10, cex = 1.5, main= "Brain sizes - Habitat preference models \nPhylogenetic tree", sub="Modification and polytomies added from Hedtke et al. 2013")
#dev.off()

#population trends - brains
#pdf("Brain sizes population trends tree.pdf")
plot(bee.tree9, cex = 0.75, main= "Brain sizes - Population trends models \nPhylogenetic tree", sub="Modification and polytomies added from Hedtke et al. 2013")
#dev.off()

#Population trends - habitat preference
#pdf("Population trends habitat preference tree.pdf")
plot(bee.tree8, cex = 1.1, main= "Population trends - Habitat preference models \nPhylogenetic tree", sub="Modification and polytomies added from Hedtke et al. 2013")
#dev.off()



#Sup mat figure 2


pdf("sup mat figure 2.pdf")
par(mfrow=c(2,2))

plot(Forests ~ Brain.weight,data = dataformcmc10, ylab="Forest preference", las=1, main="Forest preference related to brain sizes\n Absolute brain weight (a)", xlab="Absolute brain weight")
fit<-marginal_effects(brm.habpref.brainforest2)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "grey30", lwd = 2)
lines(fits$Brain.weight, fits$upper__, col = "grey30",lwd = 2)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Forests ~ Brain.weight,data = dataformcmc10)
lines(fits$Brain.weight, fits$estimate__, lwd=2, col = "darkgreen")
lines(c(0,6),c(1.04,1.04), col = "black")
lines(c(0,6),c(-0.04,-0.04), col = "black")

plot(Urban ~ Brain.weight,data = dataformcmc10, ylab="Urban preference", las= 1, main="Urban preference related to brain sizes\n Absolute brain weight (b)", xlab="Absolute brain weight")
points(Urban ~ Brain.weight,data = dataformcmc10)
fit<-marginal_effects(brm.habpref.brainurban2)
fits<-as.data.frame(fit$Brain.weight)
lines(fits$Brain.weight, fits$estimate__, lwd=2)
lines(fits$Brain.weight, fits$lower__, col = "grey30",lwd=2)
lines(fits$Brain.weight, fits$upper__, col = "grey30",lwd=2)
polygon(c(fits$Brain.weight, rev(fits$Brain.weight)), c(fits$upper__, rev(fits$lower__)),
        col = "grey80", border = NA)
points(Urban ~ Brain.weight,data = dataformcmc10)
lines(fits$Brain.weight, fits$estimate__, lwd=2, col = "Blue")
lines(c(0,6),c(1.04,1.04), col = "black")
lines(c(0,6),c(-0.04,-0.04), col = "black")


dev.off()

par(mfrow=c(1,1))










#####search for new brains#########
bartomeus_trends$Species
habitat_preference_US$Species
unique(brains.it$Species)

potential_list<-merge(bartomeus_trends,habitat_preference_US)
potential_list$Species
unique(merge(potential_list,brains.it)$Species)

AAAAA<-unique(merge(potential_list,brains.it))
data.frame(AAAAA$Species,
AAAAA$Brain.Weight..mg.)

merge(potential_list, brains.it)

list.to.fill<-unique(merge(potential_list, brains.it, all.x = TRUE))
View(list.to.fill)
data.frame(list.to.fill$Species,
list.to.fill$Brain.Weight..mg.)

aggregate(list.to.fill$Brain.Weight..mg. ~ list.to.fill$Species, FUN=mean, na.action = na.pass)

data.means.pref$Species

Andrena cressonii
Andrena erigeniae
Andrena perplexa
Lasioglossum bruneri
Lasioglossum oblongum    
Lasioglossum pectorale  
Lasioglossum pilosum     
Lasioglossum tegulare    
Lasioglossum versatum
Megachile brevis
Nomada pygmaea





Bombus bimaculatus
Bombus fervidus
Lasioglossum cressonii
Lasioglossum imitatum   
