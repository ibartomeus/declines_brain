library(reshape2)
library(googlesheets)
library(rredlist)
library(MCMCglmm)
library(brms)
library(data.tree)
library('ctv') 
library(ape)
library(stringi)

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
brain.it.trends<-merge(species.brains1, species.trends)
brain.it.trends
#write.csv(brain.it.trends, "data_to_play_with.csv")

# PHYLOGENETICS-----
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
                                    "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
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
                                    "Tachysphex" 
))

#"Augochlorella" genus is missing
plot(bee.tree)




#Does Queen have bigger brains?------
boxplot(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, ylab="Brain/IT")

aggregate(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, FUN = mean)
aov.queens<-aov(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex)
TukeyHSD(aov.queens)
