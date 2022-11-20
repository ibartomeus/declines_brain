all_cleaning = read_csv("Raw_data/all_cleaning.csv")


install.packages("taxize")

library(taxize)

specieslist = unique(all_cleaning$Species)

t <- tax_name(query = c(specieslist), get = c("phylum","class", "order", "family", "genus"), db = "ncbi")
