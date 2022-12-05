
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
d1 = tax_name(sci = c(specieslist[1:50]),  db = 'ncbi', get = c("order", "family", "subfamily", "genus"))
d2 = tax_name(sci = c(specieslist[51:100]),  db = 'ncbi', get = c("order", "family", "subfamily", "genus"))
d3 = tax_name(sci = c(specieslist[101:nlevels(factor(specieslist))]),  db = 'ncbi', get = c("order", "family", "subfamily", "genus"))

d = bind_rows(d1,d2,d3)

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
                          query=="Lasioglossum dialictus" ~ "Halictidae",
                          query=="Megachile mucida" ~ "Megachilidae")) %>% 
mutate(genus = word(query, 1)) %>% 
mutate(subfamily = case_when(query=="Osmia lateralis" ~ "Megachilinae",
                          query=="Panurgus venustus" ~ "Panurginae",
                          query=="Andrena varians" ~ "Andreninae",
                          query=="Andrena rhyssonota" ~ "Andreninae",
                          query=="Anthophora dispar" ~ "Apinae",
                          query=="Eucera rufa" ~ "Apinae",
                          query=="Chelostoma philadephi" ~ "Megachilinae",
                          query=="Lasioglossum dialictus" ~ "Halictinae",
                          query=="Megachile mucida" ~ "Megachilinae"))

#Bind rows again
all = bind_rows(d_non_missing, d_missing)
#Save data
write_csv(all, "Data/Processing/taxonomic_rank.csv")

#Check total number of families and genus
nlevels(factor(unique(all$family))) #6
nlevels(factor(unique(all$genus))) #31
nlevels(factor(unique(all$subfamily))) #11
  
 