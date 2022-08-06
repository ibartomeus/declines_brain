#Calculate habitat preferences

#Load library
library(tidyverse)
library(bipartite)

#Load extracted data
pref <- read_csv("Data/Usa_data/land_cover_usa.csv.gz") %>% 
dplyr::select(species, cover.names) %>% 
mutate_if(is.character,as.factor)

#Check levels of species and cover names
#First, species
pref %>% 
group_by(species) %>% 
summarise(n_rows = length(species))
#Second, cover.names
pref %>% 
group_by(cover.names) %>% 
summarise(n_rows = length(cover.names))

#Recode levels to calculate preferences
#Check pdf with landcover types in Data/Usa_data
#and make decissions based on that
pref = pref %>% 
mutate(cover.names = fct_recode(as.factor(cover.names),
      Natural = "Barren Land",
      Agricultural = "Cultivated Crops", #just this one for this category...
      Natural = "Deciduous Forest",
      Urban = "Developed, High Intensity",
      Urban = "Developed, Medium Intensity",
      Urban = "Developed, Low Intensity",
      Agricultural = "Developed, Open Space",
      Natural = "Emergent Herbaceous Wetlands",
      Natural = "Evergreen Forest",
      Natural = "Hay/Pasture",
      Natural = "Herbaceous",
      Natural = "Mixed Forest",
      Discard = "Open Water",
      Natural = "Shrub/Scrub",
      Natural = "Woody Wetlands")) %>% 
filter(!cover.names == "Discard") # Don't select open water ones for now

#Check levels again
pref %>% 
group_by(cover.names) %>% 
summarise(n_rows = length(cover.names))

#Now prepare data to calculate preferences
pref.table = pref %>%
count(species, cover.names) %>%
pivot_wider(names_from = cover.names, values_from = n, values_fill = list(n = 0)) %>% 
column_to_rownames(var="species")

#Null model of our pref.table
n.mod <- nullmodel(pref.table, N=1000, method="r2dtable")

#Calculate preferences
#Alternative way, probably more elegant

m = pref.table #create matrix from data and overwrite in the for loop

for(z in 1:ncol(pref.table)){
 for(k in 1:nrow(pref.table)){
        
v <- lapply(n.mod, `[[`, k) #generate vector for each row/species
#calculate percentile and store in each matrix position     
m[k,z] <- sum(unlist(v) <  pref.table[k,z] ) / length(unlist(v)) 
        
}}


