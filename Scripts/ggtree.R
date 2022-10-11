#Install packages
#devtools::install_github("YuLab-SMU/ggtree",force=TRUE)
#It took me a while to install it (I had to update a lot of stuff)

library(ggtree) #v3.5.3.002
library(ggplot2)

#Load phylogenetic data from europe
#trial
bee.tree100 = readRDS("Data/Europe_data/bee.tree100.rds")

# generate some random values for each tip label in the data
d1 <- data.frame(id=bee.tree100$tip.label, val=rnorm(length(bee.tree100$tip.label), sd=3))

# Make some more data with another random value.
d2 <- data.frame(id=bee.tree100$tip.label, value = abs(rnorm(length(bee.tree100$tip.label), mean=100, sd=50)))


preferences = readr::read_csv("Data/Europe_data/preferences_europe.csv") 

colnames(preferences)


library(stringr)
library(tidyverse)
library(tibble)

preferences =preferences %>% 
mutate(Species = str_replace(Species, " ", "_")) %>% 
column_to_rownames("Species")

p <- ggtree(bee.tree100) %<+% d2  + 
geom_tippoint(aes(size = value))+
theme_tree2()+scale_size_continuous(range = c(0, 1.5),name="Brain\n size")+
    scale_colour_viridis_c()


preferences_1 = preferences %>% rownames_to_column('gene')

d=gather(preferences_1, condition, measurement, Natural:Seminatural, factor_key=TRUE)

p2 = ggplot(d, aes(x=condition, y=gene)) + 
geom_tile(aes(fill=measurement),color = "black") +
scale_fill_viridis_c() + 
theme_minimal() +
xlab(NULL) + 
ylab(NULL) + 
theme(axis.text.x = element_text(face="bold", color="black", 
size=7.5, angle=45, vjust=1.15, hjust=1),axis.text.y = element_text(face="italic", color="black", 
size=4))


library(aplot)
p2 %>% insert_left(p) 

