
#In this script we plot encephalisation 
#(log(brain)~log(body))
#Although more species were used
#I am going to show only the ones from
#our final analyses (after filtering)


#Load libraries----
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(elementalist) 
#devtools::install_github("teunbrand/elementalist")
#install.packages("remotes")
#remotes::install_github("teunbrand/elementalist")
#Read data----
m = read_csv("Data/Processing/residuals_model.csv")
r = read_csv("Data/Processing/residuals_data.csv")
d = read_csv("Data/Processing/taxonomic_rank.csv") %>% 
mutate(Species = str_replace_all(query, " ", "_")) %>% 
select(!c(query, db))
#Add taxonomic rank to dataset
r = left_join(r,d)

#Select just species that fulfilled our criteria of filtering
#Load preferences
#preferences_europe = read_csv("Data/Europe_data/preferences_europe_qualitative.csv") 
#preferences_usa = read_csv("Data/Usa_data/preferences_usa_qualitative.csv") 

#sp = c(preferences_europe$Species, preferences_usa$Species)

r = r %>% 
mutate(Species = str_replace_all(Species, "_", " ")) 
#filter(Species %in% sp)


#Plot----
ggplot(m, aes(x = log_it, y = estimate__)) +
geom_line() +
geom_point(data = r, aes(log_it, log_brain_weight, color = subfamily)) + 
theme_bw() +
ylab("log(Brain size)") +
xlab("log(Body size)") +
theme(legend.position = c(.95, .75),
legend.justification = c("right", "top"),
legend.box.just = "right",
legend.background = element_rect_round(radius = unit(0.2, "snpc"),fill = "grey"),
legend.key = element_rect(fill = "grey"))



