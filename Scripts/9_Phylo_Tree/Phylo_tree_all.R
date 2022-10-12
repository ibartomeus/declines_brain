#Phylogenetic tree for USA and Europe

#Install packages
#devtools::install_github("YuLab-SMU/ggtree",force=TRUE)
#It took me a while to install it (I had to update a lot of stuff)

library(ggtree) #v3.5.3.002
library(ggplot2)
library(stringr)
library(tidyverse)
library(tibble)

#Load phylogenetic data from europe
bee.tree100 = readRDS("Data/Europe_USA/bee.tree100.rds")
#Load brain data
brain_weight = readr::read_csv("Data/Processing/brain_weight_data.csv")
#Load preferences data
preferences_usa = readr::read_csv("Data/USA_data/preferences_usa.csv") 
preferences_eu = readr::read_csv("Data/Europe_data/preferences_europe.csv") 
#Bind rows
pref = bind_rows(preferences_usa, preferences_eu)

#Average preferences between USA and Europe
preferences = pref %>% 
group_by(Species) %>%
summarise(across(everything(), list(mean),.names = "{.col}")) %>% 
ungroup()

#Delete underscore from tree
bee.tree100$tip.label = str_replace(bee.tree100$tip.label, "_", " ")

#Generate vector with species
v = bee.tree100$tip.label
#Filter species with that are in the phylo
brain_weight = brain_weight %>% filter(Species %in% v)


#Fill a column based on condition
brain_weight = brain_weight %>% 
    mutate(Country = case_when((brain_weight$Species %in% preferences_eu$Species & 
    brain_weight$Species %in% preferences_usa$Species ~ "Both"),
    (brain_weight$Species %in% preferences_usa$Species ~ "USA"),
    (brain_weight$Species %in% preferences_eu$Species ~ "Europe")))


#Plot tree
p <- ggtree(bee.tree100) %<+% brain_weight  + 
    geom_tippoint(aes(size = residuals, color=Country))+
    theme_tree2()+scale_size_continuous(range = c(-1.75, 1.85),name="Residuals")

#Convert wide format to long for plotting heatmap
d=gather(preferences, condition, measurement, Agricultural:Seminatural, factor_key=TRUE)

p2 = ggplot(d, aes(x=condition, y=Species)) + 
    geom_tile(aes(fill=measurement),color = "black") +
    scale_fill_viridis_c(name = "Habitat\n preference") + 
    theme_minimal() +
    xlab(NULL) + 
    ylab(NULL) + 
    theme(axis.text.x = element_text(face="bold", color="black", 
    size=7.5, angle=45, vjust=1.15, hjust=1),
    axis.text.y = element_text(face="italic", color="black", size=4, hjust=0))

#Nice way to insert ordered heatmap
library(aplot)
p2 %>% insert_left(p) 

