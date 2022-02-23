
#Read zip data generated in scripts 3 for USA and Europe
#Bind Europa and USA data, last step before analysis!
#Load libraries
library(tidyverse)
library(data.table)
europe <- data.frame(fread("Data/Europe_data/all_above_50_europe.csv.gz"))
usa <- data.frame(fread("Data/Usa_data/all_above_50_usa.csv.gz"))

#Add missing col of Europe
europe$subregion <- NA

#Bind data
final_data <- rbind(europe, usa)

#Make all cols with first capital letter
colnames(final_data) <- str_to_title(colnames(final_data))

#Save data again
#write.csv(final_data, file=gzfile("Data/final_data.csv.gz"), row.names=FALSE)
#MA, this would read as follows:
final_data <- data.frame(fread("Data/final_data.csv.gz"))

#Some quick checks about the data
#Check number of species
nlevels(factor(final_data$Species_name))
#Check species above 100 records
all_above_100 <- final_data %>% 
    group_by(Species_name) %>% filter(n() >= 100) %>% ungroup()
nlevels(factor(all_above_100$Species_name))
#Check number of records by continent
<<<<<<< HEAD
eu <- final_data %>% filter(Continent=="Europe") #344758
us <- final_data %>% filter(Continent=="North America") #25773
nlevels(factor(eu$Species_name))
nlevels(factor(us$Species_name))
=======
eu <- final_data %>% filter(Continent=="Europe") #46701
us <- final_data %>% filter(Continent=="North America") #3239
>>>>>>> c33128dbc99352cd7b806a727430bd75f4465e5a

#Histogram of counts per species
#1rst for all
all_sum <- data.frame(final_data %>% 
                    group_by(Species_name) %>%
                    summarise(no_rows = length(Species_name)) 
                    %>%  arrange(-no_rows))

all_sum$Species_name <- factor(all_sum$Species_name, levels = all_sum$Species_name)

ggplot(all_sum, aes(x=Species_name, y=no_rows)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1, size=5))
