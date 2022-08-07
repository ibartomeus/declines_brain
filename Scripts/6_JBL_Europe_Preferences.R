#Calculate habitat preferences

#Load library
library(tidyverse)
library(bipartite)

#Load extracted data
pref <- read_csv("Data/Europe_data/land_cover_europe.csv.gz") %>% 
    dplyr::select(Species, Cover_names) %>% 
    mutate_if(is.character,as.factor) 

#Check levels of species and cover names
#First, Species
check_species = pref %>% 
    group_by(Species) %>% 
    summarise(n_rows = length(Species))
#Second, Cover_names
check_cover = pref %>% 
    group_by(Cover_names) %>% 
    summarise(n_rows = length(Cover_names))

#Recode levels to calculate preferences
#Check levels here
#https://land.copernicus.eu/user-corner/technical-library/corine-land-cover-nomenclature-guidelines/html/index-clc-512.html
pref = pref %>% 
mutate(Cover_names = fct_recode(as.factor(Cover_names),
     Seminatural = "Agro-forestry areas", #1
     Urban = "Airports", #2
     Agricultural = "Annual crops associated with permanent crops", #3
     Natural = "Bare rocks", #4
     Natural = "Beaches, dunes, sands", #5
     Natural = "Burnt areas", #6
     Natural = "Coastal lagoons", #7
     Natural = "Coniferous forest", #8
     Urban = "Construction sites", #9
     Natural = "Discontinuous urban fabric", #10
     Urban = "Dump sites", #11
     Discard = "Estuaries", #12 Probably filter out
     Agricultural = "Fruit trees and berry plantations", #13
     Urban = "Green urban areas", #14
     Urban = "Industrial or commercial units", #15
     Agricultural = "Land principally occupied by agriculture, with significant areas of natural vegetation", #16
     Discard = "Mineral extraction sites", #17 check
     Natural = "Mixed forest", #18
     Natural = "Moors and heathland", #19
     Natural = "Natural grasslands", #20
     Discard = "NODATA", #21 Filter out
     Agricultural = "Non-irrigated arable land", #22
     Natural = "Olive groves", #23
     Natural = "Peat bogs", #24
     Seminatural = "Permanently irrigated land", #25
     Urban = "Port areas", #26
     Urban = "Road and rail networks and associated land", #27
     Discard = "Salines", #28 Discard for now
     Natural = "Salt marshes", #29
     Natural = "Sclerophyllous vegetation", #30
     Discard = "Sea and ocean", #31 Discard for now
     Natural = "Sparsely vegetated areas", #32
     Urban = "Sport and leisure facilities", #33
     Natural = "Water bodies", #34
     Natural = "Water courses" #35
     )) %>% 
    filter(!Cover_names == "Discard") # Don't select open water ones for now

#Check levels again
pref %>% 
group_by(Cover_names) %>% 
summarise(n_rows = length(Cover_names))

#Now prepare data to calculate preferences
pref.table = pref %>%
count(Species, Cover_names) %>%
pivot_wider(names_from = Cover_names, values_from = n, values_fill = list(n = 0)) %>% 
column_to_rownames(var="Species")

#Null model of our pref.table
n.mod <- nullmodel(pref.table, N=10000, method="r2dtable")

#Calculate preferences
#Alternative way, probably more elegant

m = pref.table #create matrix from data and overwrite in the for loop

for(z in 1:ncol(pref.table)){
    for(k in 1:nrow(pref.table)){
        
v <- lapply(n.mod, `[[`, k) #generate vector for each row/species
#calculate percentile and store in each matrix position     
m[k,z] <- sum(unlist(v) <  pref.table[k,z] ) / length(unlist(v)) 
        
    }}

