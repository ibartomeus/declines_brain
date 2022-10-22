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
pref = pref %>% 
    mutate(Cover_names = fct_recode(as.factor(Cover_names),
Urban = "Airports", #1
Discard = "Bare rocks", #2
Natural = "Beaches, dunes, sands", #3
Natural = "Broad-leaved forest", #4
Natural = "Coastal lagoons", #5
Agricultural = "Complex cultivation patterns", #6
Natural = "Coniferous forest", #7
Urban = "Construction sites", #8
Urban = "Continuous urban fabric", #9
Seminatural = "Discontinuous urban fabric", #10
Discard = "Dump sites", #11
Discard = "Estuaries", #12
Agricultural = "Fruit trees and berry plantations", #13
Seminatural = "Green urban areas", #14)
Urban = "Industrial or commercial units", #15
Natural = "Inland marshes", #16
Natural = "Intertidal flats", #17
Agricultural = "Land principally occupied by agriculture, with significant areas of natural vegetation", #18
Discard = "Mineral extraction sites", #19
Natural = "Mixed forest", #20
Natural = "Moors and heathland", #21
Agricultural = "Natural grasslands", #22
Agricultural = "Non-irrigated arable land", #23
Agricultural = "Pastures", #24
Natural = "Peat bogs", #25
Urban = "Port areas", #26) 
Seminatural = "Road and rail networks and associated land", #27
Natural = "Salt marshes", #28
Discard = "Sea and ocean", #29
Natural = "Sparsely vegetated areas", #30
Seminatural = "Sport and leisure facilities", #31
Natural = "Transitional woodland-shrub", #32
Agricultural = "Vineyards", #33
Discard = "Water bodies", #34
Discard = "Water courses" #35  
)) %>% 
    filter(!Cover_names == "Discard") 

#Check levels again
check_cover = pref %>% 
    group_by(Cover_names) %>% 
    summarise(n_rows = length(Cover_names))

#Now prepare data to calculate preferences
pref.table = pref %>%
count(Species, Cover_names) %>%
pivot_wider(names_from = Cover_names, values_from = n, values_fill = list(n = 0)) %>% 
column_to_rownames(var="Species")


set.seed(1) 
#Null model of our pref.table
n.mod <- nullmodel(pref.table, N=10000, method="r2dtable")

#Calculate preferences
#Alternative way, probably more elegant

m = pref.table #create matrix to store data
#Natural
pref_nat = pref.table %>% dplyr::select(Natural)
n.mod_nat = list()
for(n in 1:10000){
n.mod_nat[[n]] = n.mod[[n]][,1]           
} 

for(k in 1:nrow(pref_nat)){
v <- lapply(n.mod_nat, `[[`, k)
m[k,1] <- sum(unlist(v) <  pref.table[k,1] ) / length(unlist(v)) 

}
#Urban
pref_urb = pref.table %>% dplyr::select(Urban)
n.mod_urb = list()
for(n in 1:10000){
    n.mod_urb[[n]] = n.mod[[n]][,2]           
} 

for(k in 1:nrow(pref_urb)){
    v <- lapply(n.mod_urb, `[[`, k)
    m[k,2] <- sum(unlist(v) <  pref.table[k,2] ) / length(unlist(v)) 
    
}
#Agricultural
pref_agr = pref.table %>% dplyr::select(Agricultural)
n.mod_agr = list()
for(n in 1:10000){
    n.mod_agr[[n]] = n.mod[[n]][,3]           
} 

for(k in 1:nrow(pref_agr)){
    v <- lapply(n.mod_agr, `[[`, k)
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
write_csv(m, "Data/Europe_data/preferences_europe.csv")


#Safety check
#Check preferences for column 2 species 2 (0.0143)
pref_urb = pref.table %>% dplyr::select(Urban) #select column of interest
#Create empty list
n.mod_urb = list()
#Select first column from our list of matrices
for(n in 1:10000){
    n.mod_urb[[n]] = n.mod[[n]][,2]    #select col2       
} 

v <- lapply(n.mod_urb, `[[`, 2) #select 2nd value
sum(unlist(v) <  pref.table[2,2] ) / length(unlist(v)) 

