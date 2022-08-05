#Calculate habitat preferences

#Load library
library(tidyverse)

#Load extracted data
pref <- read_csv("Data/Usa_data/land_cover_usa.csv.gz") %>% 
dplyr::select(species, cover.names) %>% 
mutate_if(is.character,as.factor)


s = pref %>% 
group_by(species) %>% 
summarise(n_rows = length(species))



pref %>%  distinct(species)

pref.table = pref %>%
    count(species, cover.names) %>%
    pivot_wider(names_from = cover.names, values_from = n, values_fill = list(n = 0))



pref.table<-aggregate(cover.names~species, FUN=summary, pref)


#Convertimos long data en wide data, así sacamos cuantas ocurrencias de cada especie en cada habitat
pref.table<-aggregate(pref$cover.names~pref$species, FUN=summary, pref)
pref.table<-c(pref.table)
pref.tabletemp<-pref.table$`pref$cover.names`
rownames(pref.tabletemp)<-pref.table$`pref$species`
usa_table<-data.frame(pref.tabletemp)
str(usa_table)
str(pref.table)
names(pref.table)
View(usa_table)
