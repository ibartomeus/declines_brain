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
#1 Check with Nacho, dehesas and others 
Seminatural = "Agro-forestry areas", #1) 2.4.4 
#2 Check with Nacho 
Urban = "Airports", #2) 1.2.4 
#3 Crops 
Agricultural = "Annual crops associated with permanent crops", #3) 2.4.1 
#4 Very different cover type, not included 
Discard = "Bare rocks", #4) 3.3.2 
#5 Very different cover type, not included 
Discard = "Beaches, dunes, sands", #5) 3.3.1 
#6 Not included, lack of details 
Discard = "Burnt areas", #6 3.3.4
#7 Not included, only water surface
Discard = "Coastal lagoons", #7) 5.2.1 
#8 Straightforward
Natural = "Coniferous forest", #8) 3.1.2
#9 Excluded, complex to consider because it lacks habitat context
Urban = "Construction sites", #9) 1.3.3
#10 Based on the images and the description has a lot of vegetation cover
Urban = "Discontinuous urban fabric", #10) 1.1.2
#11 Pollinators here are likely to depend much on the context not on the dump
Discard = "Dump sites", #11) 1.3.2
#12 Filter out, water body
Discard = "Estuaries", #12) 5.2.2 
#13 Straightforward
Agricultural = "Fruit trees and berry plantations", #13) 2.2.2
#14 Urban as embedded within the city
Urban = "Green urban areas", #14) 1.4.1
#15 Highly modified
Urban = "Industrial or commercial units", #15) 1.2.1
#16 By definion, seminatural
Seminatural = "Land principally occupied by agriculture, with significant areas of natural vegetation", #16) 2.4.3
#17 Discard as is not clear the vegetation context
Discard = "Mineral extraction sites", #17) 1.3.1
#18 Straightforward 3.1.3
Natural = "Mixed forest", #18) 3.1.3
#19 Crystal clear category
Natural = "Moors and heathland", #19) 3.2.2
#20 Crystal clear category
Natural = "Natural grasslands", #20) 3.2.1
#21 Filter out
Discard = "NODATA", #21 
#22 cultivated land
Agricultural = "Non-irrigated arable land", #22) 2.1.1
#23 As it is a monoculture despite some very
Agricultural = "Olive groves", #23) 2.2.3
#24 Crystal clear category
Natural = "Peat bogs", #24) 4.1.2
#25 Crystal clear category
Seminatural = "Permanently irrigated land", #25) 2.1.2
#26 Check with Nacho
Urban = "Port areas", #26) 1.2.3
#27 Check with Nacho
Seminatural = "Road and rail networks and associated land", #27) 1.2.2
#28 Check with Nacho but just only 200 records
Seminatural = "Salines", #28) 4.2.2 
#29 Flowering plant communities 
Natural = "Salt marshes", #29) 4.2.1
#30 Sclerophyllous shrubs and low shrubs
Natural = "Sclerophyllous vegetation", #30) 3.2.3
#31 Discard for now 
Discard = "Sea and ocean", #31) 5.2.3
#32 Areas with sparse vegetation, covering 10-50% of surface 
Natural = "Sparsely vegetated areas", #32) 3.3.3
#33 Check with Nacho 
Seminatural = "Sport and leisure facilities", #33) 1.4.2 
#34 Excluded as it is very general
Discard = "Water bodies", #34) 5.1.2
#35 Excluded as it is very general
Discard = "Water courses" #35 5.1.1 
)) %>% 
filter(!Cover_names == "Discard") 

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
