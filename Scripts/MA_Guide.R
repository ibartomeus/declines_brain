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

#DATA CONSTRUCTION----

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


#A ver, el pesado de cerebros es delicado, y hay que filtrar outliers y demás, 
# no voy a anotar esta parte porque es tediosa, pero creo que se entiende fácil
#Básicamente evalúo cada resultado y outliers locos asumo que es un error
species.brains1

data.filtered2<-species.brains1
boxplot(Brain.weight~Species,data = species.brains1, las= 2,cex.axis = 0.6)
boxplot(Brain.weight~Species,data = data.filtered2, las= 2,cex.axis = 0.6)
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

#Creamos un data frame con las medias del peso cerebral y de la IT para cada especie
weights.mean<-data.frame(aggregate(Brain.weight ~ Species, data = data.filtered2, FUN = mean))
IT.mean<-data.frame(aggregate(IT ~ Species, data = data.filtered2, FUN = mean))
wit.mean<-merge(weights.mean,IT.mean)

#Esta es la lista de especies para las que tenemos medidas "fiables" de cerebro e IT, 
# pero no en todas hay preferencia de habitats
wit.mean$Species

#Linearizamos y añadimos la variable de residuales general de cada especie
plot(Brain.weight ~ IT, data=wit.mean)
abline(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(log(Brain.weight) ~ log(IT), data=wit.mean))
lm.data.means<-lm(log(Brain.weight) ~ log(IT), data=wit.mean)
lm.data.means$residuals
lm.data.means$residuals
wit.mean$residuals<-lm.data.means$residuals

lm.data.means$model

#this is the dataframe containing brain weights (residuales)
wit.mean

#We load habitat preference (This is a small list from collado et al 201X, we need more)
habpref <- read.csv("Raw_data/habitat_preference_simplify.csv")

#Data merging

habitat.brain<-merge(wit.mean,habpref)

#This is the only data on brains and habitat preference we have
habitat.brain


#Aditionally, we can pre-visualice with plots if there is some signal between habitat preference and brain sizes

plot(Urban~residuals,data=habitat.brain, xlab="Brain size (residuals)", ylab = "Urban preference")
abline(glm(Urban~residuals,data=habitat.brain))
summary(glm(Urban~residuals,data=habitat.brain))

plot(Forests~residuals,data=habitat.brain, xlab="Brain size (residuals)", ylab = "Forest preference")
abline(glm(Forests~residuals,data=habitat.brain))
summary(glm(Forests~residuals,data=habitat.brain))

#PHYLOGENETIC TREE CONSTRUCTION---------
#In this part we are going to build a phylogenetic matrix to control our analysis

#We remove apis mellifera because is a managed species
habitat.brain<-subset(habitat.brain, subset = !(habitat.brain$Species == "Apis mellifera"))


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


##ANALYSIS-----

#Urban

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

#FIGURES----

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
