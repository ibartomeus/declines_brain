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
                                    Discard = "Barren Land", #Very different cover type, not included (MA & JB)
                                    Agricultural = "Cultivated Crops", #(MA & JB)
                                    Natural = "Deciduous Forest", #(MA & JB)
                                    Urban = "Developed, High Intensity", #Greater than 50% of cover (MA & JB) 
                                    Urban = "Developed, Medium Intensity", #Greater than 50% of cover (MA & JB) 
                                    Seminatural = "Developed, Low Intensity", #Low urban impact (MA & JB)
                                    Seminatural = "Developed, Open Space", #Open areas with vegetation (MA & JB)
                                    Natural = "Emergent Herbaceous Wetlands", #(MA & JB)
                                    Natural = "Evergreen Forest", #(MA & JB)
                                    Agricultural = "Hay/Pasture", #Highly managed by humans (MA & JB)
                                    Natural = "Herbaceous", #See explanation below (MA & JB)
                                    Natural = "Mixed Forest", #(MA & JB)
                                    Discard = "Open Water", #Low vegetation and land, not included (MA & JB)
                                    Natural = "Shrub/Scrub", #Heath classified as natural (MA & JB)
                                    Natural = "Woody Wetlands")) %>% #(MA & JB)
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


#Convert to qualitative
m[m >= 0.95] <- 1
m[m <= 0.05] <- 0
m[m > 0.05 & m < 0.95 ] <- NA

m = rownames_to_column(m, var = "Species")
write_csv(m, "Data/Usa_data/preferences_usa_qualitative.csv")


#Analysis of USA bee preferences

#Load libraries
library(tidyverse) 
library(brms)

#Load data ----
#Load brain data
brain_weight = read_csv("Data/brain_weight_data.csv")
#Load preferences
preferences = read_csv("Data/Usa_data/preferences_usa_qualitative.csv") 
#Load phylogenetic matrix to correct for in analysis
A10 = readRDS("Data/Usa_data/phylo_usa.rds")

#Prepare data----
d = left_join(preferences, brain_weight) %>% 
    mutate(Species = str_replace_all(Species, " ", "_"))

#Convert to long to model everything at the same time
long_data = d %>% gather(Habitat, Preference, 2:5, -c(Species))

#Prepare col brain weight/IT
long_data = long_data %>% 
    mutate(brain_it = Brain.weight/IT)

#Analysis Preference ~ residuals----
model1 = brm(Preference ~ residuals * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

ce1 <- conditional_effects(model1, effects = "residuals:Habitat",points=T) 

bayes_R2(model1)

p1 = ggplot(ce1[[1]], aes(x = residuals, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Residuals") + 
    ggtitle("USA")

#Save data and model1 output
write_csv(long_data , "Data/Usa_data/data_preference_residuals_usa_qualitative.csv")
write_csv(ce1[[1]], "Data/Usa_data/model_output_preference_residuals_usa_qualitative.csv")

#Analysis Preference ~ brain weight----
model2 = brm(Preference ~ Brain.weight * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

pp_check(model2)
bayes_R2(model2)
ce2 <- conditional_effects(model2, effects = "Brain.weight:Habitat",points=T) 

p2 = ggplot(ce2[[1]], aes(x = Brain.weight, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Brain weight") +
    ggtitle("USA")    

#Save data
write_csv(ce2[[1]], "Data/Usa_data/model_output_preference_brain_weight_usa_qualitative.csv")

#Analysis Preference ~ IT----
model3 = brm(Preference ~ IT * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

pp_check(model3)
bayes_R2(model3)
ce3 <- conditional_effects(model3, effects = "IT:Habitat",points=T) 

p3 = ggplot(ce3[[1]], aes(x = IT, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Intertegular distance") +
    ggtitle("USA")  

#Save data
write_csv(ce3[[1]], "Data/Usa_data/model_output_preference_it_usa_qualitative.csv")

#Plot all together
library(patchwork)
p1 / p2 / p3

#Analysis Preference ~ brain_it----
model4 = brm(Preference ~ brain_it * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=bernoulli())

ce4 <- conditional_effects(model4, effects = "brain_it:Habitat",points=T) 

p4 = ggplot(ce4[[1]], aes(x = brain_it, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = brain_it, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("brain_it") +
    ggtitle("USA") 

#Save data
write_csv(ce4[[1]], "Data/Usa_data/model_output_preference_brain_it_usa_qualitative.csv")



