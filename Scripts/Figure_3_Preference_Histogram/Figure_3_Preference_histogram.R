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

labels.minor <- c("Avoidance", rep("", times = 2), "Preference","")

long_data$Habitat <- factor(long_data$Habitat, levels = c("Natural", "Agricultural", "Urban"),
                  labels = c("A) Natural", "B) Agricultural", "C) Urban"))

long_data = long_data %>% 
mutate(Preference_colour = case_when(Preference <= 0.2 ~ "A", 
                                     Preference >= 0.8 ~ "B",
                                     T ~ "C"))

long_data$Preference_colour <- factor(long_data$Preference_colour, levels = c("A","C", "B"),
                            labels = c("Avoidance", "No preference", "Preference"))


#Plot
ggplot(long_data, aes(x=Preference, color= Preference_colour)) +
    geom_histogram(aes(fill= Preference_colour),bins = 35, alpha = 0.8,linewidth=0.5) + 
    xlab("Habitat preference") +
    ylab("Frequency") +
    facet_wrap(~ Habitat) +
    scale_fill_manual(name="" ,values=c("coral2", "gray", "palegreen3")) +
    scale_color_manual(name="" ,values=c("coral2", "gray", "palegreen3")) +
    jtools::theme_apa() +   
    theme(axis.title = element_text(face = "bold"),
    panel.border=element_rect(size=1), 
    strip.text = element_text(face = "bold", size = rel(1),hjust = -0.01),
    strip.background = element_rect(fill = "white", colour = "black", size = 0,linetype="solid"))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,60))+
    geom_vline(aes(xintercept = 0.20), colour="black", linetype="dashed") +
    geom_vline(aes(xintercept = 0.80), colour="black", linetype="dashed") +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.2),
                       labels = c(0,0.2,0.4,0.6,0.8,1)) 

    
    