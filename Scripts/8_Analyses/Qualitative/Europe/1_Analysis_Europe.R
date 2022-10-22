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

#Convert to qualitative
m[m >= 0.95] <- 1
m[m <= 0.05] <- 0
m[m > 0.05 & m < 0.95 ] <- NA

m = rownames_to_column(m, var = "Species")

write_csv(m, "Data/Europe_data/preferences_europe_qualitative.csv")

# Analysis
#Analysis of Europe bee preferences

#Load libraries
library(tidyverse) 
library(brms)

#Load data ----
#Load brain data
brain_weight = read_csv("Data/Processing/brain_weight_data.csv")
#Load preferences
preferences = read_csv("Data/Europe_data/preferences_europe_qualitative.csv") 
#Load phylogenetic matrix to correct for in analysis
A10 = readRDS("Data/Europe_data/phylo_europe.rds")

#Prepare data----
d = left_join(preferences, brain_weight) %>% 
    mutate(Species = str_replace_all(Species, " ", "_"))

#Convert to long to model everything at the same time
long_data = d %>% gather(Habitat, Preference, 2:5, -c(Species))

#Prepare col brain weight/IT
long_data = long_data %>% 
    mutate(brain_it = Brain.weight/IT) %>% 
drop_na(Preference)



#Analysis Preference ~ residuals----
model1 = brm(Preference ~ residuals * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

pp_check(model1)

bayes_R2(model1)

ce1 <- conditional_effects(model1, effects = "residuals:Habitat",points=T) 

bayes_R2(model1)

ggplot(ce1[[1]], aes(x = residuals, y = estimate__, color=Habitat)) +
    geom_line() +
    geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Residuals") + 
    ggtitle("Europe") #+ facet_wrap(~ Habitat)
#Save data and model1 output
write_csv(long_data , "Data/Europe_data/data_preference_residuals_europe_qualitative.csv")
write_csv(ce1[[1]], "Data/Europe_data/model_output_preference_residuals_europe_qualitative.csv")


#Analysis Preference ~ brain weight----
model2 = brm(Preference ~ Brain.weight * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

pp_check(model2)
bayes_R2(model2)

ce2 <- conditional_effects(model2, effects = "Brain.weight:Habitat",points=T) 

ggplot(ce2[[1]], aes(x = Brain.weight, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Brain weight") +
    ggtitle("Europe") 

#Save data
write_csv(ce2[[1]], "Data/Europe_data/model_output_preference_brain_weight_europe_qualitative.csv")

#Analysis Preference ~ IT----
model3 = brm(Preference ~ IT * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

pp_check(model3)
bayes_R2(model3)


ce3 <- conditional_effects(model3, effects = "IT:Habitat",points=T) 

ggplot(ce3[[1]], aes(x = IT, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Intertegular distance") +
    ggtitle("Europe")  

#Save data
write_csv(ce3[[1]], "Data/Europe_data/model_output_preference_it_europe_qualitative.csv")

#Analysis Preference ~ brain_it----
model4 = brm(Preference ~ brain_it * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

ce4 <- conditional_effects(model4, effects = "brain_it:Habitat",points=T) 

ggplot(ce4[[1]], aes(x = brain_it, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = brain_it, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("brain_it") +
    ggtitle("Europe") 

#Save data
write_csv(ce4[[1]], "Data/Europe_data/model_output_preference_brain_it_europe_qualitative.csv")

