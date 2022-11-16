#Histogram of habitat preferences
#Load libraries
library(tidyverse) 

#Load preferences
preferences_europe = read_csv("Data/Europe_data/preferences_europe.csv") 
preferences_usa = read_csv("Data/Usa_data/preferences_usa.csv") 

#Merge datasets
preferences = bind_rows(preferences_europe, preferences_usa)


#Convert to long to model everything at the same time
long_data = preferences %>% gather(Habitat, Preference, 2:4, -c(Species))

#Relevel for plotting
long_data = long_data %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural", "Urban"))) 

labels.minor <- c("Low", "", "High")

long_data$Habitat <- factor(long_data$Habitat, levels = c("Natural", "Agricultural", "Urban"),
                  labels = c("A) Natural", "B) Agricultural", "C) Urban"))

#Plot
ggplot(long_data, aes(x=Preference, color= Habitat)) +
    geom_histogram(aes(fill= Habitat)) + 
    xlab("Habitat preference") +
    ylab("Frequency") +
    facet_wrap(~ Habitat) +
    scale_colour_viridis_d(end=0.98)+
    scale_fill_viridis_d(end=0.98) +
    jtools::theme_apa() +   
    theme(axis.title = element_text(face = "bold"),
    panel.border=element_rect(size=1), 
    legend.position = "none",
    strip.text = element_text(face = "bold", size = rel(1),hjust = -0.01),
    strip.background = element_rect(fill = "white", colour = "black", size = 0,linetype="solid"))+
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.5),
                       labels = paste0(c(0,0.5,1), "\n", labels.minor)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,60))+
    geom_vline(aes(xintercept = 0.05), colour="black", linetype="dashed") +
    geom_vline(aes(xintercept = 0.95), colour="black", linetype="dashed") 

