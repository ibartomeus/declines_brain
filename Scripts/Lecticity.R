#Load libraries
library(dplyr)
library(stringr)

#Load phylogenetic data from europe
bee.tree100 = readRDS("Data/Europe_USA/bee.tree100.rds")
#Load brain data
brain_weight = readr::read_csv("Data/Processing/brain_weight_data.csv")
#Load preferences data
preferences_usa = readr::read_csv("Data/USA_data/preferences_usa.csv") 
preferences_eu = readr::read_csv("Data/Europe_data/preferences_europe.csv") 
#Bind rows
pref = bind_rows(preferences_usa, preferences_eu)

unique(pref$Species)


#Average preferences between USA and Europe
preferences = pref %>% 
  group_by(Species) %>%
  summarise(across(everything(), list(mean),.names = "{.col}")) %>% 
  ungroup()


#Read lecticity
lecticity = readr::read_csv("Data/Bees_Lecticity.csv") 

#Delete underscore from tree
lecticity$species = str_replace(lecticity$species, "_", " ")
lecticity = lecticity %>% rename(Species = species)

#Bind datasets
d = left_join(preferences, lecticity)

#Check species with lacking info
#Agapostemon virescens
#https://lopezuribelab.com/halictidae/agapostemon-virescens/#:~:text=Agapostemon%20virescens%20is%20a%20polylectic,coneflowers%2C%20asters%2C%20and%20goldenrod.
#As the other Agapostemon, they tend to visit many unrelated plant species
#but they tend to prefer Asteraceae flowers
#Polylectic
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Agapostemon virescens", "Polylectic"))
#Andrena angustior
#http://dx.doi.org/10.1016/j.biocon.2017.09.009
#Polylectic
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena angustior", "Polylectic"))
#Andrena hirticincta
#https://www.sharpeatmanguides.com/andrena-mining-bees
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena hirticincta", "Oligolectic"))
#Andrena milwaukeensis
#https://www.discoverlife.org/20/q?search=Andrena+milwaukeensis
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena milwaukeensis", "Polylectic"))
#Andrena pilipes
#https://www.wildbienen.de/eb-apili.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena pilipes", "Polylectic"))
#Andrena vicina
#https://www.discoverlife.org/20/q?search=Andrena+vicina
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena vicina", "Polylectic"))
#Andrena wilkella
##http://dx.doi.org/10.1016/j.biocon.2017.09.009
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena wilkella", "Oligolectic"))

#Augochlorella aurata
#https://www.beesofcanada.com/species/augochlorella-aurata
#https://www.discoverlife.org/20/q?search=Augochlorella+aurata
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Augochlorella aurata", "Polylectic"))

#Augochloropsis metallica
#https://www.gbif.org/species/1353091
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Augochloropsis metallica", "Polylectic"))

#Bombus griseocollis
#https://doi.org/10.1002/ecy.2697
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Bombus griseocollis", "Polylectic"))

#Calliopsis andreniformis
#https://www.sharpeatmanguides.com/calliopsis-andreniformis
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Calliopsis andreniformis", "Polylectic"))

#Ceratina strenua
#https://www.beesofcanada.com/species/ceratina-strenua
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Ceratina strenua", "Polylectic"))

#Colletes thoracicus
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Colletes thoracicus", "Polylectic"))

#Lasioglossum malachurum

#Megachile texana

#Megachile willughbiella

#Osmia atriventris
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia atriventris", "Polylectic"))

#Osmia lignaria
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia lignaria", "Polylectic"))

#Osmia pumila
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia pumila", "Polylectic"))

#Osmia cornifrons
#http://dx.doi.org/10.1098/rsos.200225
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia cornifrons", "Polylectic"))

#Lasioglossum malachurum
#https://www.wildbienen.de/eb-lmala.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Lasioglossum malachurum", "Polylectic"))

#Megachile texana
#https://entnemdept.ufl.edu/hallg/melitto/floridabees/litomegachile.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Megachile texana", "Polylectic"))

#Megachile texana
#https://entnemdept.ufl.edu/hallg/melitto/floridabees/litomegachile.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Megachile texana", "Polylectic"))


#Megachile willughbiella
#https://www.wildbienen.de/eb-mwill.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Megachile willughbiella", "Polylectic"))



