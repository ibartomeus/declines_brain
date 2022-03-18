library(reshape2)
library(googlesheets4)
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
library(bipartite)
#importa esto
final_data
#-----
#Lista de especies
final_data$Species
unique(final_data$Species)

??read.tree
getwd()
bee.trees=read.tree(file="Data/phylogeny_genus_level.txt")
data10<-final_data
unique(data10$Species)

species=c( "Andrena_angustior", "Andrena_barbilabris", "Andrena_bicolor",           
           "Andrena_chrysosceles", "Andrena_cineraria", "Andrena_dorsata",            
           "Andrena_flavipes", "Andrena_fucata", "Andrena_fulva",              
           "Andrena_gravida", "Andrena_haemorrhoa", "Andrena_labiata",            
           "Andrena_nigroaenea", "Andrena_nitida", "Andrena_ovatula",            
           "Andrena_pilipes", "Andrena_semilaevis", "Andrena_subopaca",           
           "Andrena_tibialis", "Andrena_wilkella", "Anthidium_manicatum",        
           "Anthophora_bimaculata", "Anthophora_plumipes", "Anthophora_quadrimaculata",  
           "Bombus_hypnorum", "Bombus_jonellus", "Bombus_lucorum",             
           "Bombus_muscorum",     "Bombus_pascuorum", "Bombus_pratorum" ,           
           "Bombus_terrestris",    "Dasypoda_hirtipes", "Lasioglossum_calceatum",     
           "Lasioglossum_fulvicorne",   "Lasioglossum_leucozonium", "Lasioglossum_malachurum",    
           "Lasioglossum_punctatissimum", "Lasioglossum_sexnotatum", "Lasioglossum_zonulum",       
           "Megachile_centuncularis","Megachile_willughbiella", "Osmia_caerulescens",         
           "Osmia_cornuta", "Osmia_leaiana", "Panurgus_banksianus",        
           "Panurgus_calcaratus", "Stelis_breviuscula", "Xylocopa_violacea",          
           "Andrena_carantonica", "Anthidium_oblongatum", "Halictus_quadricinctus",     
           "Lasioglossum_sexstrigatum", "Megachile_rotundata", "Nomada_succincta",           
           "Agapostemon_sericeus", "Andrena_crataegi", "Andrena_hirticincta",        
           "Andrena_milwaukeensis", "Andrena_miserabilis", "Andrena_nubecula",           
           "Andrena_pruni", "Andrena_vicina", "Augochloropsis_metallica",   
           "Bombus_impatiens", "Bombus_ternarius", "Bombus_vagans",              
           "Ceratina_calcarata", "Colletes_thoracicus", "Halictus_ligatus",           
           "Hylaeus_mesillae", "Hylaeus_modestus", "Megachile_campanulae",       
           "Megachile_sculpturalis", "Nomada_luteoloides", "Osmia_cornifrons",           
           "Osmia_lignaria")


#Pick tree 1
bee.mcmc=bee.trees[[1]]
#Make a wasp the outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)
bee.mcmc=chronos(bee.mcmc)


bee.mcmc$tip.label
species
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
                                      "Sphecodes",
                                      "Colletes","Agapostemon","Augochlora",
                                      "Calliopsis", "Melissodes",
                                      "Ptilothrix" 
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





dataformcmc10=data10
dataformcmc10$Species
#Let's fix it
dataformcmc10$Species<-stri_replace_first_regex(dataformcmc10$Species,pattern = " ", replacement = "_")

#no differences. good
setdiff(rownames(A100),dataformcmc10$Species)



#PHYLOGENETIC TREE CONSTRUCTION---------

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

