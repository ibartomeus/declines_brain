#Calculate phylogenetic matrix for USA species
library(visreg) 
library(data.tree)
library(ape)
library(phytools)
#Load extracted data
pref <- read_csv("Data/Usa_data/land_cover_usa.csv.gz") %>% 
    dplyr::select(species, cover.names) %>% 
    mutate_if(is.character,as.factor) 

#Check levels of species and cover names
#First, Species
species_list = pref %>% 
    distinct(species) 

#-----
#Lista de especies
bee.trees=read.tree(file="Data/phylogeny_genus_level.txt")

#Generate Species vector 
species = species_list %>%  
    mutate(species = str_replace_all(species, " ", "_")) %>% 
    pull()

#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species

#Add genus that I don't have 
bee.tree100=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
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
                                       "Notanthidium", "Epanthidium", "Lithurgus",       
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
                                       "Tachysphex", "Rhodanthidium","Apis",
                                       "Sphecodes", "Melissodes"
))


plot(bee.tree100)
nodelabels()
tiplabels()


#add dummy species labels
bee.tree100$tip.label<-paste(bee.tree100$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
    bee.tree100<-add.species.to.genus(bee.tree100,species[i],
                                      where="root")
}

## prune out dummy taxa
ii<-grep("dum",bee.tree100$tip.label)
bee.tree100<-drop.tip(bee.tree100,bee.tree100$tip.label[ii])
#Our tree
plot(bee.tree100, cex = 0.6)

##Check for missing species
setdiff(species,bee.tree100$tip.label)

#Remove node labels, or the model will fail
bee.tree100$node.label=NULL

##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree100, nodes = "TIPS", scale = TRUE)
A100 <- solve(inv.phylo$Ainv)
rownames(A100) <- rownames(inv.phylo$Ainv)
isSymmetric(A100, check.attributes = FALSE)

#no differences. good
setdiff(rownames(A100),species)

#Save data
saveRDS(A100, "Data/Usa_data/phylo_usa.rds")
saveRDS(bee.tree100, "Data/USA_data/bee.tree100.rds")

