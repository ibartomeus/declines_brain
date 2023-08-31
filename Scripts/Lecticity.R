


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
#Polylectic

#Andrena angustior
#http://dx.doi.org/10.1016/j.biocon.2017.09.009
#Polylectic

#Andrena wilkella
##http://dx.doi.org/10.1016/j.biocon.2017.09.009
#Oligolectic



