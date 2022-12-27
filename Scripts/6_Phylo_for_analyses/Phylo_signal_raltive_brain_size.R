
#Here we calculate the phylogenetic signal of relative brain size----
#Load libraries
library(ape)
library(phytools)
library(dplyr)
library(readr)
library(stringr)
library(data.table)

#Read tarit data and phylogenetic signal
trait = read_csv("Data/Processing/brain_weight_data.csv") %>% 
mutate(Species = str_replace_all(Species, " ", "_"))
phylo = readRDS("Data/Processing/phylo_raw.rds")


trait1 = unlist(trait$residuals)
#Prepare names for analysis
d = setNames(trait1, trait$Species)

#Convert to phylo tree
p_tree = nj(phylo)
#Check tip labels format
p_tree$tip.label

#Run analysis
output = phylosig(p_tree, d, method="lambda",test=TRUE)
#Check output
output
#Save output
saveRDS(output, "Data/Processing/phylo_signal.rds")

#Here we calculate the phylogenetic signal of habitat preference----
#Load preferences
preferences_europe = read_csv("Data/Europe_data/preferences_europe.csv") %>% 
mutate(country = "europe")
preferences_usa = read_csv("Data/Usa_data/preferences_usa.csv") %>% 
mutate(country = "usa")
phylo = readRDS("Data/Europe_USA/phylo_all.rds")

#There are duplicated species between usa and europe
f <- intersect(preferences_europe$Species, preferences_usa$Species)
#Filter them out from usa and just select europe which had higher number of records
preferences_usa = preferences_usa %>% 
filter(!Species %in% f)

#Merge datasets
preferences = bind_rows(preferences_europe, preferences_usa)%>% 
mutate(Species = str_replace_all(Species, " ", "_"))
trait1 = unlist(preferences$Urban)
trait2 = unlist(preferences$Natural)
trait3 = unlist(preferences$Agricultural)
#Prepare names for analysis
d1 = setNames(trait1, preferences$Species)
d2 = setNames(trait2, preferences$Species)
d3 = setNames(trait3, preferences$Species)
#Convert to phylo tree
p_tree = nj(phylo)
#Check tip labels format
p_tree$tip.label

#Run analysis
output1 = phylosig(p_tree, d1, method="lambda",test=TRUE)
output1
output2 = phylosig(p_tree, d2, method="lambda",test=TRUE)
output2
output3 = phylosig(p_tree, d3, method="lambda",test=TRUE)
output3
#Save output
saveRDS(output2, "Data/Processing/phylo_signal_natural.rds")
