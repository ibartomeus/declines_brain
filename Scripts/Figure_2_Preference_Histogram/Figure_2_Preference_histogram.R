#Histogram of habitat preferences
#Load libraries
library(tidyverse) 

#Load preferences
preferences_europe = read_csv("Data/Europe_data/preferences_europe.csv") 
preferences_usa = read_csv("Data/Usa_data/preferences_usa.csv") 

#Merge datasets
preferences = bind_rows(preferences_europe, preferences_usa)


#Convert to long to model everything at the same time
long_data = preferences %>% gather(Habitat, Preference, 2:5, -c(Species))

#Relevel for plotting
long_data = long_data %>% 
    mutate(Habitat = recode_factor(Habitat, "Seminatural" = "Semi-developed")) %>% 
    mutate(Habitat=fct_relevel(Habitat,c("Natural","Agricultural","Semi-developed", "Urban"))) 



labels.minor <- c("Low", "", "High")

#Plot
ggplot(long_data, aes(x=Preference, color= Habitat)) +
    geom_histogram(aes(fill= Habitat)) + 
    xlab("Habitat preference") +
    ylab("Frequency") +
    facet_wrap(~ Habitat) +
    scale_colour_viridis_d(end=0.98)+
    scale_fill_viridis_d(end=0.98) +
    theme_bw()+
    theme(axis.title = element_text(face = "bold"), 
    strip.text.x = element_blank() , strip.background = element_blank(),
    panel.border=element_rect(size=0.8))+
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.5),
                       labels = paste0(c(0,0.5,1), "\n", labels.minor)) +
    scale_y_continuous(expand = c(0, 0))+
    geom_vline(aes(xintercept = 0.05), colour="black", linetype="dashed") +
    geom_vline(aes(xintercept = 0.95), colour="black", linetype="dashed")


