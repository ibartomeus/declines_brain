#Calculate intraclass correlation coefficiet
#for brain and body size
#Exploration of the dataset (number of replicates)
#and show graphically interspecific variation

#Load libraries
library(dplyr)
library(stringr)
library(readr)
library(lme4)
library(performance)

#Read data
long = read_csv("Raw_data/all_cleaning.csv")

#Check number of replicates per individual
long %>% 
group_by(Species) %>%   
summarise(length = n()) %>% 
ungroup() %>% 
summarise(mean(length))


#Filter out the species that we don't use
#Load preferences data
preferences_usa = readr::read_csv("Data/USA_data/preferences_usa.csv") 
preferences_eu = readr::read_csv("Data/Europe_data/preferences_europe.csv") 
#Bind rows
pref = bind_rows(preferences_usa, preferences_eu)
spp = unique(pref$Species)

#Select just the species that we used in the study for calculating preferences
#after applying our filtering criteria (see mthods for further justification)
long = long %>% 
filter(Species %in% spp)


#Calculate intraclass correlation coefficient for brain size
ggplot(long, aes(Brain.weight)) +
geom_histogram()
shapiro.test(log(long$Brain.weight))
qqnorm(log(long$Brain.weight))
qqline(log(long$Brain.weight))
#Mixed model 
m1 <- lmer(log(Brain.weight) ~ 1 | Species, data = long)
performance::icc(m1)
performance::icc()
?icc()

#Calculate intraclass correlation coefficient for body size
ggplot(long, aes(IT)) +
geom_histogram()
shapiro.test(log(long$IT))
qqnorm(log(long$IT))
qqline(log(long$IT))
#Mixed model 
m2 <- lmer(log(IT) ~ 1 | Species, data = long)
performance::icc(m2)

#Show graphically variation in the distribution across species 
#For both brain and body sizes
#1st for brain weight
d = long %>% 
group_by(Species) %>% 
summarise(mean = mean(Brain.weight), std = sd(Brain.weight)) %>% 
arrange(desc(mean))
#Plotting
p1 = ggplot(d, aes(x=reorder(Species, mean), y=mean, fill=std)) + 
geom_point()+
geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
position=position_dodge(0.05)) +
theme_bw() +
coord_flip() +
theme(legend.position = "none",axis.text.y = element_text(size = 4)) +
xlab("Species") +
ylab("Brain weight (mg)")

#2nd for body size
d1 = long %>% 
group_by(Species) %>% 
summarise(mean = mean(IT), std = sd(IT)) %>% 
arrange(desc(mean))
#Plotting
p2 = ggplot(d1, aes(x=reorder(Species, mean), y=mean, fill=std)) + 
geom_point()+
geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
position=position_dodge(0.05)) +
theme_bw() +
coord_flip() +
theme(legend.position = "none",axis.text.y = element_text(size = 4)) +
xlab("Species") +
ylab("Intertegular distance (mm)")

p1 + p2
