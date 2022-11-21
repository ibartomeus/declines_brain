
#Here I load the filtered data and check the families and genera of the different species

#Load library
library(taxize)
library(readr)
library(dplyr)
library(stringr)

#Read data
all_cleaning = read_csv("Raw_data/all_cleaning.csv")

#Create spp list
specieslist = unique(all_cleaning$Species)

#Download data
d <- tax_name(query = c(specieslist), get = c("order", "family", "genus"), db = "ncbi")

#Missing values
d_missing = d %>% 
filter(is.na(order))

#Non missing
d_non_missing = d %>% 
filter(!is.na(order)) 

#Try to add missing variables with mutate
d_missing = d_missing %>% 
mutate(order = "Hymenoptera") %>% 
mutate(family = case_when(query=="Osmia lateralis" ~ "Megachilidae",
                          query=="Panurgus venustus" ~ "Andrenidae",
                          query=="Andrena varians" ~ "Andrenidae",
                          query=="Andrena rhyssonota" ~ "Andrenidae",
                          query=="Anthophora dispar" ~ "Apidae",
                          query=="Eucera rufa" ~ "Apidae",
                          query=="Chelostoma philadephi" ~ "Megachilidae",
                          query=="Lasioglossum dialictus" ~ "Halictidae")) %>% 
mutate(genus = word(query, 1))

#Bind rows again
all = bind_rows(d_non_missing, d_missing)
#Save data
write_csv(all, "Data/Processing/taxonomic_rank.csv")

#Check total number of families and genus

nlevels(factor(unique(all$family))) #6
nlevels(factor(unique(all$genus))) #30
                    
 