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
     Seminatural = "Agro-forestry areas", #1 (2.4.4) Check with Nacho, dehesas and others (MA & JB)
     Urban = "Airports", #2 Check with Nacho (MA & JB)
     Agricultural = "Annual crops associated with permanent crops", #3 (2.4.1) Crops (MA & JB)
     Discard = "Bare rocks", #4 Very different cover type, not included (MA & JB)
     Discard = "Beaches, dunes, sands", #5 (3.3.1) Very different cover type, not included (MA & JB)
     Discard = "Burnt areas", #6 Not included, lack of details (MA & JB)
     Discard = "Coastal lagoons", #7 (5.2.1) Not included, only water surface  (MA & JB)
     Natural = "Coniferous forest", #8 (MA & JB)
     Urban = "Construction sites", #9 Excluded, complex to consider because it lacks habitat context (MA & JB)
     Seminatural = "Discontinuous urban fabric", #10 Based on the images and the description has a lot of vegetation cover (MA & JB)
     Discard = "Dump sites", #11 Pollinators here are likely to depend much on the context not on the dump (MA & JB)
     Discard = "Estuaries", #12 Filter out, water body (MA & JB)
     Agricultural = "Fruit trees and berry plantations", #13 (MA & JB)
     Urban = "Green urban areas", #14 Urban as embedded within the city (MA & JB)
     Urban = "Industrial or commercial units", #15 Highly modified (MA & JB)
     Seminatural = "Land principally occupied by agriculture, with significant areas of natural vegetation", #16 By definion, seminatural (MA & JB)
     Discard = "Mineral extraction sites", #17 Discard as is not clear the vegetation context (MA & JB)
     Natural = "Mixed forest", #18 (MA & JB)
     Natural = "Moors and heathland", #19 (MA & JB)
     Natural = "Natural grasslands", #20 (MA & JB)
     Discard = "NODATA", #21 Filter out
     Agricultural = "Non-irrigated arable land", #22 cultivated land (MA & JB)
     Agricultural = "Olive groves", #23 As it is a monoculture despite some very natural practises (MA & JB)
     Natural = "Peat bogs", #24 (MA & JB)
     Seminatural = "Permanently irrigated land", #25 (MA & JB)
     Urban = "Port areas", #26 Check with Nacho (MA & JB)
     Seminatural = "Road and rail networks and associated land", #27 Check with Nacho (MA & JB)
     Seminatural = "Salines", #28 Check with Nacho (MA & JB) but just only 200 records
     Natural = "Salt marshes", #29  Flowering plant communities (MA & JB)
     Natural = "Sclerophyllous vegetation", #30  Sclerophyllous shrubs and low shrubs (MA & JB)
     Discard = "Sea and ocean", #31 Discard for now (MA & JB)
     Natural = "Sparsely vegetated areas", #32 Areas with sparse vegetation, covering 10-50% of surface (MA & JB)
     Seminatural = "Sport and leisure facilities", #33 Check with Nacho (MA & JB)
     Discard = "Water bodies", #34 Out as they are very general (MA & JB) 
     Discard = "Water courses" #35 Out as they are very general (MA & JB)
     )) %>% 
    filter(!Cover_names == "Discard") # Don't select open water ones for now

#Check levels again
s = pref %>% 
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

#Save data
m = rownames_to_column(m, var = "Species")
write_csv(m, "Data/Europe_data/preferences_europe.csv")
