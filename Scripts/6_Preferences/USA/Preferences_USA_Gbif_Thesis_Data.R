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
S = pref %>% 
group_by(cover.names) %>% 
summarise(n_rows = length(cover.names))

#Recode levels to calculate preferences
#Check pdf with landcover types in Data/Usa_data
#and make decisions based on that
pref = pref %>% 
mutate(cover.names = fct_recode(as.factor(cover.names),
      Discard = "Barren Land", #1
      Agricultural = "Cultivated Crops", #2
      Natural = "Deciduous Forest", #3
      Urban = "Developed, High Intensity", #4
      Urban = "Developed, Medium Intensity", #5
      Seminatural = "Developed, Low Intensity", #6
      Seminatural = "Developed, Open Space", #7
      Natural = "Emergent Herbaceous Wetlands", #8
      Natural = "Evergreen Forest", #9
      Agricultural = "Hay/Pasture", #10
      Natural = "Herbaceous", #11
      Natural = "Mixed Forest", #12
      Discard = "Open Water", #13
      Natural = "Shrub/Scrub", #14
      Natural = "Woody Wetlands")) %>% #15
filter(!cover.names == "Discard") #Discard these categories classified as "Discard"

#Herbaceous and hay/pasture are classified as two different habitats in NLCD. 
#We merged them because herbaceous areas in our sampling region are always 
#for livestock (Koh et al., 2016).

#Check levels again
check_cover = pref %>% 
group_by(cover.names) %>% 
summarise(n_rows = length(cover.names))

#Now prepare data to calculate preferences
pref.table = pref %>% 
count(species, cover.names) %>%
pivot_wider(names_from = cover.names, values_from = n, values_fill = list(n = 0)) %>% 
column_to_rownames(var="species") 

#Null model of our pref.table
n.mod <- nullmodel(pref.table, N=10000, method="r2dtable")

#Calculate preferences
m = pref.table #create matrix to store data
#check cols
colnames(pref.table)

#Agricultural
pref_agr = pref.table %>% dplyr::select(Agricultural)
n.mod_agr = list()
for(n in 1:10000){
    n.mod_agr[[n]] = n.mod[[n]][,1]           
} 

for(k in 1:nrow(pref_agr)){
    v <- lapply(n.mod_agr, `[[`, k)
    m[k,1] <- sum(unlist(v) <  pref.table[k,1] ) / length(unlist(v)) 
    
}

#Natural
pref_nat = pref.table %>% dplyr::select(Natural)
n.mod_nat = list()
for(n in 1:10000){
    n.mod_nat[[n]] = n.mod[[n]][,2]           
} 

for(k in 1:nrow(pref_nat)){
    v <- lapply(n.mod_nat, `[[`, k)
    m[k,2] <- sum(unlist(v) <  pref.table[k,2] ) / length(unlist(v)) 
    
}
#Urban
pref_urb = pref.table %>% dplyr::select(Urban)
n.mod_urb = list()
for(n in 1:10000){
    n.mod_urb[[n]] = n.mod[[n]][,3]           
} 

for(k in 1:nrow(pref_urb)){
    v <- lapply(n.mod_urb, `[[`, k)
    m[k,3] <- sum(unlist(v) <  pref.table[k,3] ) / length(unlist(v)) 
    
}

#Seminatural
pref_sem = pref.table %>% dplyr::select(Seminatural)
n.mod_sem = list()
for(n in 1:10000){
    n.mod_sem[[n]] = n.mod[[n]][,4]           
} 

for(k in 1:nrow(pref_sem)){
    v <- lapply(n.mod_sem, `[[`, k)
    m[k,4] <- sum(unlist(v) <  pref.table[k,4] ) / length(unlist(v)) 
    
}

m = rownames_to_column(m, var = "Species")
write_csv(m, "Data/Usa_data/preferences_usa.csv")


