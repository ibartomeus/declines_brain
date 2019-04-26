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
library(ggtree)
library(lme4)
library(sjstats)

#DATA CONSTRUCTION------

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

BTT<-merge(brain.it.trends, bartomeus.numeric.trends, by.x = "Species", all.x = TRUE)

write.csv(BTT, "data/data_to_play_with.csv")

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



