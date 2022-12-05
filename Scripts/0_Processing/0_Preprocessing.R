#Preprocessing----

#In this script we fix typos and rename species when neccesary to their synonyms

#Load libraries
library(readr)
library(dplyr)
library(stringr)

#First we clean USA records again (there were some wrong measurements in the old dataset)
#Read both datasets
#read usa
usa_brain = read_csv("Raw_data/DB_brain_US.csv") %>% 
  dplyr::select(!c(head.weight.g, no.optic.lobes.weight.mg)) %>% 
  mutate(species = str_replace(species, "_", " "))
#read raw dataset sent by Collado
brains.it <- read.csv("Raw_data/brains.it.csv", dec = ",") %>% 
  dplyr::select(!c(Place, No.optic.lobes.weight, 
            Notes, Head.weight..g., Plant, Date)) %>% 
  rename(Body.weight = Weigth..g.) %>% 
  rename(Brain.weight = Brain.Weight..mg.)

#Filter out USA brains from sayol, these ones are not corrected!
#brains.it = brains.it %>% 
#filter(!Country == "USA")

spp = usa_brain$species
#Do it just for USA spp 
brains.it.usa = brains.it %>% filter(Country=="USA")
brains.it.eur = brains.it %>% filter(!Country=="USA")

#Filter out Sayol species
brains.it.usa = brains.it.usa %>% 
filter(!Species %in% spp)

#Bind again datasets
brains.it = bind_rows(brains.it.eur, brains.it.usa)

#Prepare data----
#Important:
#The column of interest is brain fix (mg)
#Heads are extracted and fixed and then the brain is removed and weighted
#Fresh brains can be converted with a conversion factor to fix! 
#brain.fix.mg = brain.fresh.mg x 1.2262. #Check sayol etal
#Convert fresh to fix when value is na (I guess that almost always when fresh is measured)
#The trend is the same but looks a bit nicer without adding the brains with this conversion

usa_brain = usa_brain %>% 
#mutate(brain.fix.mg = case_when(is.na(brain.fix.mg) == T ~ brain.fresh.mg * 1.2262,
#       is.na(brain.fix.mg) == F ~ brain.fix.mg)) %>% 
rename(Genus = genus) %>% 
dplyr::select(!c(family, brain.fresh.mg)) %>% 
rename(Species = species) %>% 
rename(Sex =  sex) %>% 
rename(Country  = country) %>% 
rename(IT = IT.mm) %>% 
rename(Body.weight = body.mass.g) %>% 
rename(Brain.weight = brain.fix.mg)

#Check for differences between datasets
colnames(usa_brain)
colnames(brains.it)
setdiff(colnames(usa_brain), colnames(brains.it))
#All columns are the same now

#Generate now dataset with usa corrected and europe
all = bind_rows(brains.it, usa_brain)

#Reshaping data and some cleaning
all_cleaning = all %>% 
filter(!ID == "F33") %>%  #Dasypoda visnaga's brain was conserved in ethanol, not formol
mutate(Brain.IT = (Brain.weight/IT)) %>% #create new col
dplyr::select(Species, IT, Brain.weight, Brain.IT, Sex, Country) %>% #select cols of interest
na.omit() #filter out na's

#Check levels
s = all_cleaning %>% distinct(Species)

#Delete Species with sp. 
all_cleaning <- filter(all_cleaning, !grepl(" sp.",Species))

#Some more typos
all_cleaning$Species[all_cleaning$Species=="Anthopora plumipes"] <- "Anthophora plumipes"
all_cleaning$Species[all_cleaning$Species=="Anthopora dispar"] <- "Anthophora dispar"
all_cleaning$Species[all_cleaning$Species=="Agaposemon sericeus"] <- "Agapostemon sericeus"
all_cleaning$Species[all_cleaning$Species=="Andena vicina"] <- "Andrena vicina"

#Two species are best to rename it with synonym
all_cleaning$Species[all_cleaning$Species=="Flavipanurgus venustus"] <- "Panurgus venustus"
all_cleaning$Species[all_cleaning$Species=="Psithyrus vestalis"] <- "Bombus vestalis"

#Filter out Apis mellifera
all_cleaning <- all_cleaning %>% filter(!Species=="Apis mellifera")    

#Check and clean non-sense brain weights
#Andrena barbilabris
all_cleaning <- subset(all_cleaning, 
subset = !((Species == "Andrena barbilabris") & (Brain.weight==0.296)))

#Filter out non-species
all_cleaning = subset(all_cleaning, subset = !((Species == "Andrena (Chlorandrena)")))
all_cleaning = subset(all_cleaning, subset = !((Species == "Andrena (Chrysandrena)")))

#Before filtering females, rename f and females equally
levels(factor(all_cleaning$Sex))

#Fix levels
all_cleaning = all_cleaning %>% 
mutate(Sex =replace(Sex, Sex == "F", "Female")) %>% 
mutate(Sex =replace(Sex, Sex == "M", "Male")) %>% 
mutate(Sex =replace(Sex, Sex == "Queen?", "Queen"))

#IMPORTANT! select just females
all_cleaning = all_cleaning %>%  filter(Sex == "Female") %>% dplyr::select(!Sex)
#Save data to load raw values in methods
write_csv(all_cleaning, "Raw_data/all_cleaning.csv")


#Check unique cases and levels for methods
d = all_cleaning %>% 
group_by(Species) %>% 
summarise(individuals = n())
median(d$individuals)
sum(d$individuals) #433
nlevels(factor(unique(d$Species))) #113 species

#Check for possible outliers
#1st Brain weight
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
s(l[100:113])

#Is quite easy to have outliers with brain weights (Weight them is highly ticky and values are ver sensitive to mistakes)
#For this, we select only values between 
all_cleaning = all_cleaning %>%
group_by(Species) %>% 
mutate(Max_val = quantile(Brain.weight, 0.75) + 1.5 *IQR(Brain.weight)) %>% 
mutate(Min_val = quantile(Brain.weight, 0.25) -  1.5 * IQR(Brain.weight)) %>% 
filter(Brain.weight <= Max_val) %>% 
filter(Brain.weight >= Min_val) %>% 
ungroup()

s(l[1:50])
s(l[50:100])
s(l[100:113])

#Just for safety do the same with IT's
#all_cleaning = all_cleaning %>%
#group_by(Species) %>% 
#mutate(Max_val = quantile(IT, 0.75) + 1.5 *IQR(IT)) %>% 
#mutate(Min_val = quantile(IT, 0.25) -  1.5 * IQR(IT)) %>% 
#filter(IT <= Max_val) %>% 
#filter(IT >= Min_val) %>% 
#ungroup()

#Create dataframe with average measurements of IT and brain weight
weights.mean <- data.frame(aggregate(Brain.weight ~ Species, data = all_cleaning, FUN = mean))
IT.mean <- data.frame(aggregate(IT ~ Species, data = all_cleaning, FUN = mean))
wit.mean <- merge(weights.mean, IT.mean)

saveRDS(wit.mean, "Data/Processing/wit.mean.rds")


