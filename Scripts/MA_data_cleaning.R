library(reshape2)
library(googlesheets)
library(rredlist)

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
gs_ls("Traits")
be <- gs_title("Traits")
gs_ws_ls(be)
Traits <- gs_read(ss=be, ws = "Sheet1", skip=0)
Traits<- as.data.frame(Traits)
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
rl_search(as.character(list.of.species[60,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[67,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[72,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[114,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[115,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[122,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[138,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
rl_search(as.character(list.of.species[145,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category

species.temp<-(c(as.character(list.of.species[19,1]),
as.character(list.of.species[20,1]),
as.character(list.of.species[26,1]),
as.character(list.of.species[60,1]),
as.character(list.of.species[67,1]),
as.character(list.of.species[72,1]),
as.character(list.of.species[114,1]),
as.character(list.of.species[115,1]),
as.character(list.of.species[122,1]),
as.character(list.of.species[138,1]),
as.character(list.of.species[145,1])))

category.temp<-c(rl_search(as.character(list.of.species[19,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[20,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[26,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[60,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[67,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[72,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[114,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[115,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[122,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[138,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category,
                 rl_search(as.character(list.of.species[145,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category)
species.iucn<-as.data.frame(species.temp)          
category.iucn<-as.data.frame(category.temp)          
iucn.trends<-(cbind(species.iucn,category.iucn))

# setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/Raw_data")
# write.csv(iucn.trends, "iucn_trends.csv")
# setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain")

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
species.brains1
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
brain.it.trends<-merge(species.brains1, species.trends)
brain.it.trends

#Early graphs-----
#write.csv(brain.it.trends, "data_to_play_with.csv")





#Does Queen have bigger brains?------
boxplot(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, ylab="Brain/IT")

aggregate(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, FUN = mean)
aov.queens<-aov(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex)
TukeyHSD(aov.queens)
