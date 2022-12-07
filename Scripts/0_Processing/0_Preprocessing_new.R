#Preprocessing----
#In this script we fix typos and rename species when neccesary to their synonyms
#The data source is Sayol et al., but we also have added extra species from Collado thesis
#Sayol http://dx.doi.org/10.1098/rspb.2020.0762

#Load libraries----
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

#Read datasets
#1st Sayol 
sayol = read_csv("Raw_data/BeeTraits_individual.csv") %>% 
dplyr::select(!c(head.weight.g, no.optic.lobes.weight.mg,body.mass.g)) %>% 
mutate(species = str_replace(species, "_", " "))
#snd Collado (is similar but Collado has some extra species)
#Note Sayol data is clean but Collado still needs some processing
collado <- read.csv("Raw_data/brains.it.csv", dec = ",") %>% 
dplyr::select(!c(Place, No.optic.lobes.weight, 
Notes, Head.weight..g., Plant, Date, Weigth..g., ID, Genus)) %>% 
rename(Brain.weight = Brain.Weight..mg.)

#Prepare data----
#Important:
#The column of interest is brain fix (mg)
#Heads are extracted and fixed and then the brain is removed and weighted
#Fresh brains can be converted with a conversion factor to fix! 
#brain.fix.mg = brain.fresh.mg x 1.2262. #Check sayol etal
sayol = sayol %>% 
#mutate(brain.fix.mg = case_when(is.na(brain.fix.mg) == T ~ brain.fresh.mg * 1.2262,
#is.na(brain.fix.mg) == F ~ brain.fix.mg)) %>% 
rename(Species = species) %>% 
rename(Sex =  sex) %>% 
rename(Country  = country) %>% 
rename(IT = IT.mm) %>% 
rename(Brain.weight = brain.fix.mg) 

#Reshaping data and some cleaning
sayol_main = sayol %>% 
dplyr::select(Species, IT, Brain.weight, Sex, Country)  #select cols of interest


#Select species from Collado that are not in Sayol
#Check levels
s = sayol_main %>% distinct(Species)
spp = s$Species
#Subset collado
collado_filtered = collado %>% 
filter(!Species == "Dasypoda visnaga") %>%  #Dasypoda visnaga's brain was conserved in ethanol, not formol
filter(!Species %in% spp) 
#Bind both datasets
all = bind_rows(sayol_main, collado_filtered) %>% 
na.omit() #filter out na's

#Cleaning----
#Delete Species with sp. 
all_cleaning <- filter(all, !grepl(" sp.",Species))
#Some more typos
all_cleaning$Species[all_cleaning$Species=="Anthopora plumipes"] <- "Anthophora plumipes"
all_cleaning$Species[all_cleaning$Species=="Anthopora dispar"] <- "Anthophora dispar"
all_cleaning$Species[all_cleaning$Species=="Agaposemon sericeus"] <- "Agapostemon sericeus"
all_cleaning$Species[all_cleaning$Species=="Andena vicina"] <- "Andrena vicina"
#Two species are best to rename it with synonym
all_cleaning$Species[all_cleaning$Species=="Flavipanurgus venustus"] <- "Panurgus venustus"
all_cleaning$Species[all_cleaning$Species=="Psithyrus vestalis"] <- "Bombus vestalis"
#Filter out non-species
all_cleaning = subset(all_cleaning, subset = !((Species == "Andrena (Chlorandrena)")))
all_cleaning = subset(all_cleaning, subset = !((Species == "Andrena (Chrysandrena)")))
all_cleaning = subset(all_cleaning, subset = !((Species == "Lasioglossum dialictus"))) #dialictus is a subgenus

#Filter out Apis mellifera
all_cleaning <- all_cleaning %>% filter(!Species=="Apis mellifera")    
#Before filtering females, rename f and females equally
levels(factor(all_cleaning$Sex))
#Fix levels
all_cleaning = all_cleaning %>% 
mutate(Sex =replace(Sex, Sex == "F", "Female")) %>% 
mutate(Sex =replace(Sex, Sex == "M", "Male")) %>% 
mutate(Sex =replace(Sex, Sex == "Queen?", "Queen"))


#Final filtering----
#Select females and delete outliers
#IMPORTANT! select just females
all_cleaning = all_cleaning %>%  filter(Sex == "Female") %>% dplyr::select(!Sex)
#Check unique cases and levels for methods
d = all_cleaning %>% 
group_by(Species) %>% 
summarise(individuals = n())
median(d$individuals)
sum(d$individuals) #433
nlevels(factor(unique(d$Species))) #113 species

#Check for possible outliers
#Brain weight
#Extract levels
l = levels(factor(all_cleaning$Species))
#Create function to plot everything
s = function(v) {
ggplot(all_cleaning %>% filter(Species %in% v), aes(Species, Brain.weight)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1, size=4))
}
#Plot 1st 50 records
s(l[1:50])
s(l[50:100])
s(l[100:130])

#Is quite easy to have outliers with brain weights 
#For this, we filter the ones out of 1.5 times IQR
all_cleaning = all_cleaning %>%
group_by(Species) %>% 
mutate(Max_val = quantile(Brain.weight, 0.75) + 1.5 *IQR(Brain.weight)) %>% 
mutate(Min_val = quantile(Brain.weight, 0.25) -  1.5 * IQR(Brain.weight)) %>% 
filter(Brain.weight <= Max_val) %>% #Species with one unique value are not considered!
filter(Brain.weight >= Min_val) %>% #Species with one unique value are not considered!
ungroup()

#Check again filtered plots
s(l[1:50])
s(l[50:100])
s(l[100:130])

#Create dataframe with average measurements of IT and brain weight
weights.mean <- data.frame(aggregate(Brain.weight ~ Species, data = all_cleaning, FUN = mean))
IT.mean <- data.frame(aggregate(IT ~ Species, data = all_cleaning, FUN = mean))
wit.mean <- merge(weights.mean, IT.mean)
#Check number of levels
#levels(factor(wit.mean$Species))

#Save data ----
#Save data to load raw values in methods
write_csv(all_cleaning, "Raw_data/all_cleaning.csv")
#Save data to calculate residuals
saveRDS(wit.mean, "Data/Processing/wit.mean.rds")


