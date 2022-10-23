
#Check correlation between preferences 
#Load preferences
library(readr)
ma_preferences = read_csv("Data/Usa_data/preferences_usa_thesis_ma.csv") 
gbif_preferences = read_csv("Data/Usa_data/preferences_usa_gbif.csv") 
#Merge datasets
all = left_join(gbif_preferences, ma_preferences, by="Species") 

#Explore correlations
p1 = ggstatsplot::ggscatterstats(data = all, x = Agricultural.x, y = Agricultural.y,
                            type = 'spearman') 
p2 = ggstatsplot::ggscatterstats(data = all, x = Natural.x, y = Natural.y,
                            type = 'spearman') 
p3 = ggstatsplot::ggscatterstats(data = all, x = Urban.x, y = Urban.y,
                            type = 'spearman') 
p4 = ggstatsplot::ggscatterstats(data = all, x = Seminatural.x, y = Seminatural.y,
                            type = 'spearman') 
#Plot
library(patchwork)
p1 + p2 + p3 + p4
