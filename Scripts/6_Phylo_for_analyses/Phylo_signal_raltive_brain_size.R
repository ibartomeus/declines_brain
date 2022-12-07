
#Here we calculate the phylogenetic signal of relative brain size
#Load libraries
library(ape)
library(phytools)
#Read tarit data and phylogenetic signal
trait = read_csv("Data/Processing/brain_weight_data.csv") %>% 
mutate(Species = str_replace_all(Species, " ", "_"))
phylo = readRDS("Data/Processing/phylo_raw.rds")

#Prepare names for analysis
d = setNames(trait1[,"residuals"], trait$Species)

#Convert to phylo tree
p_tree = nj(phylo)
#Check tip labels format
p_tree$tip.label

#Run analysis
output = phylosig(p_tree, d, method="lambda",test=TRUE)
#Check output
output
