library(reshape2)
library(googlesheets)
library(rredlist)

brains.it <- read.csv("~/Desktop/Tesis/R/declines_brain/Raw_data/brains.it.csv")

#List of species with brain weight and IT-----

brains.it$Encephalization<-brains.it$Brain.Weight..mg./brains.it$IT
brains.t<-subset(brains.it, subset = (is.na(brains.it$Encephalization)==FALSE))
list.of.species<-as.data.frame(unique(brains.t$Species))
spain.bees<-subset(brains.t, subset = (brains.t$Country=="Spain"))
list.of.spanish.bees<-unique(spain.bees$Species)
list.of.spanish.bees<-as.data.frame(list.of.spanish.bees)


getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/Raw_data")
#write.csv(list.of.spanish.bees, "list.of.spanish.bees.csv")
#write.csv(list.of.species, "list.of.species.csv")
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/")
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

scheper_trends <- read.csv("~/Desktop/Tesis/R/declines_brain/Raw_data/scheper_trends.csv")
# 1 means not change along time
scheper_trends

#Import red list----

#You need an API token to use this
rl_use_iucn()
IUCN_REDLIST_KEY="084775ffa81321631f2583dcd416a81af6fbd73efb60a0eab38e0df10082fe07"


list.of.species

for (n in 1:nrow(list.of.species)) {
    tryCatch({temp<-rl_search(as.character(list.of.species[n,1]), key = IUCN_REDLIST_KEY, parse = FALSE)$result[[1]]$category
    print(temp)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

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
A<-c("Dasypoda iberica","Nomada merceti","Flavipanurgus venustus",
     "Andrena semilaevis")

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
getwd()

# setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/Raw_data")
# write.csv(iucn.trends, "iucn_trends.csv")
# setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain")

#US habitats----
habitat_preference_US <- read.csv("~/Desktop/Tesis/R/declines_brain/Raw_data/habitat_preference_US.csv")
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
#Does Queen have bigger brains?------
boxplot(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, ylab="Brain/IT")

aggregate(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, FUN = mean)
aov.queens<-aov(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex)
TukeyHSD(aov.queens)
