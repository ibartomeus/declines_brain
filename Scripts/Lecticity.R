#To answer one of the issues raised by a reviewer
#We explore how the bee's diet (oligo/polylectic)
#is associated with habitat preference (1) and brain size (2)

#The data of the diet comes from the paper of Ferran
#but because is missing for some species
#I have look the diets for missing ones (~20spp)

#Load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(readr)

# (1) Lecticity ~ habitat preference ----
#First we explore this association with habitat preference
#Load preferences data
preferences_usa = readr::read_csv("Data/USA_data/preferences_usa.csv") 
preferences_eu = readr::read_csv("Data/Europe_data/preferences_europe.csv") 
#Bind rows
pref = bind_rows(preferences_usa, preferences_eu)

#Average preferences between USA and Europe
preferences = pref %>% 
group_by(Species) %>%
summarise(across(everything(), list(mean),.names = "{.col}")) %>% 
ungroup()

#Read lecticity
lecticity = read_csv("Data/Bees_Lecticity.csv") 
#Delete underscore from tree
lecticity$species = str_replace(lecticity$species, "_", " ")
lecticity = lecticity %>% rename(Species = species)

#Bind datasets
d = left_join(preferences, lecticity)

#Check species with lacking info
#Agapostemon virescens
#https://lopezuribelab.com/halictidae/agapostemon-virescens/#:~:text=Agapostemon%20virescens%20is%20a%20polylectic,coneflowers%2C%20asters%2C%20and%20goldenrod.
#As the other Agapostemon, they tend to visit many unrelated plant species
#but they tend to prefer Asteraceae flowers
#Polylectic
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Agapostemon virescens", "Polylectic"))
#Andrena angustior
#http://dx.doi.org/10.1016/j.biocon.2017.09.009
#Polylectic
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena angustior", "Polylectic"))
#Andrena hirticincta
#https://www.sharpeatmanguides.com/andrena-mining-bees
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena hirticincta", "Oligolectic"))
#Andrena milwaukeensis
#https://www.discoverlife.org/20/q?search=Andrena+milwaukeensis
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena milwaukeensis", "Polylectic"))
#Andrena pilipes
#https://www.wildbienen.de/eb-apili.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena pilipes", "Polylectic"))
#Andrena vicina
#https://www.discoverlife.org/20/q?search=Andrena+vicina
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena vicina", "Polylectic"))
#Andrena wilkella
##http://dx.doi.org/10.1016/j.biocon.2017.09.009
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Andrena wilkella", "Oligolectic"))

#Augochlorella aurata
#https://www.beesofcanada.com/species/augochlorella-aurata
#https://www.discoverlife.org/20/q?search=Augochlorella+aurata
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Augochlorella aurata", "Polylectic"))

#Augochloropsis metallica
#https://www.gbif.org/species/1353091
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Augochloropsis metallica", "Polylectic"))

#Bombus griseocollis
#https://doi.org/10.1002/ecy.2697
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Bombus griseocollis", "Polylectic"))

#Calliopsis andreniformis
#https://www.sharpeatmanguides.com/calliopsis-andreniformis
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Calliopsis andreniformis", "Polylectic"))

#Ceratina strenua
#https://www.beesofcanada.com/species/ceratina-strenua
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Ceratina strenua", "Polylectic"))

#Colletes thoracicus
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Colletes thoracicus", "Polylectic"))

#Osmia atriventris
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia atriventris", "Polylectic"))

#Osmia lignaria
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia lignaria", "Polylectic"))

#Osmia pumila
#https://doi.org/10.1073/pnas.1218503110
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia pumila", "Polylectic"))

#Osmia cornifrons
#http://dx.doi.org/10.1098/rsos.200225
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Osmia cornifrons", "Polylectic"))

#Lasioglossum malachurum
#https://www.wildbienen.de/eb-lmala.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Lasioglossum malachurum", "Polylectic"))

#Megachile texana
#https://entnemdept.ufl.edu/hallg/melitto/floridabees/litomegachile.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Megachile texana", "Polylectic"))

#Megachile texana
#https://entnemdept.ufl.edu/hallg/melitto/floridabees/litomegachile.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Megachile texana", "Polylectic"))


#Megachile willughbiella
#https://www.wildbienen.de/eb-mwill.htm
d = d %>% 
mutate(Lecticity = replace(Lecticity, Species == "Megachile willughbiella", "Polylectic"))



#Explore statistical differences across groups-----
#1st explore normality
a = d %>% filter(Lecticity== "Polylectic")
ggplot(a, aes(Agricultural)) +
  geom_histogram()
b = d %>% filter(Lecticity== "Oligolectic")
ggplot(b, aes(Agricultural)) +
  geom_histogram()
#Let's pick a non-parametric option for low sample sizes
wt1 = wilcox.test(a$Natural, b$Natural)
wt2 = wilcox.test(a$Agricultural, b$Agricultural)
wt3 = wilcox.test(a$Urban, b$Urban)

#Plotting----
#Here we load some libraries to include Wilcoxon rank test in the plot
#for publication
library(ggpubr)
library(rstatix)

#First graph Natural habitat
stat.test1 <- d %>%
wilcox_test(Natural ~ Lecticity) %>%
add_significance()
stat.test1 <- stat.test1 %>% add_xy_position(x = "Lecticity")
#Build plot
p1_1 = ggplot(d, aes(x = Lecticity, y = Natural)) + 
geom_violin(alpha=0.4, fill="#d7e1ee",linewidth=0)+
geom_boxplot(colour="black",fill="grey",
width = .15, 
outlier.shape = NA) +
geom_jitter(alpha=0.3,width = 0.1) +
ylab("Habitat Preference")+
xlab(NULL)+
ggtitle("Natural")+
stat_pvalue_manual(stat.test1, label =  "Wilcoxon test, p = {p}",vjust = -0.05, bracket.nudge.y = 0.05) +
scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
theme_bw()


#Second graph Agricultural habitat
stat.test2 <- d %>%
wilcox_test(Agricultural ~ Lecticity) %>%
add_significance()
stat.test2 <- stat.test2 %>% add_xy_position(x = "Lecticity")
#Build plot
p1_2 = ggplot(d, aes(x = Lecticity, y = Agricultural)) + 
geom_violin(alpha=0.4, fill="#d7e1ee",linewidth=0)+
geom_boxplot(colour="black",fill="grey",
width = .15, 
outlier.shape = NA) +
geom_jitter(alpha=0.3,width = 0.1) +
ylab(NULL)+
xlab("Diet")+
ggtitle("Agricultural")+
stat_pvalue_manual(stat.test2, label =  "Wilcoxon test, p = {p}",vjust = -0.05, bracket.nudge.y = 0.05) +
scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
theme_bw()

#Third graph Urban habitat
stat.test3 <- d %>%
wilcox_test(Urban ~ Lecticity) %>%
add_significance()
stat.test3 <- stat.test3 %>% add_xy_position(x = "Lecticity")
#Build plot
p1_3 = ggplot(d, aes(x = Lecticity, y = Urban)) +
geom_violin(alpha=0.4, fill="#d7e1ee",linewidth=0)+
geom_boxplot(colour="black",fill="grey",
width = .15, 
outlier.shape = NA) +
geom_jitter(alpha=0.3,width = 0.1) +
ylab(NULL)+
xlab(NULL)+
ggtitle("Urban")+
stat_pvalue_manual(stat.test3, label =  "Wilcoxon test, p = {p}",vjust = -0.05, bracket.nudge.y = 0.05) +
scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
theme_bw()

panel1 = p1_1+p1_2+p1_3 


# (2) Lecticity ~ brain size ----
#Load brain data
brain_weight = readr::read_csv("Data/Processing/brain_weight_data.csv")

d1 = left_join(brain_weight, d) %>% 
filter(!is.na(Lecticity))

#Test statistical differences across groups----
glimpse(d1)
a1 = d1 %>% filter(Lecticity== "Polylectic")
ggplot(a1, aes(Brain.weight)) +
  geom_histogram()
b1 = d1 %>% filter(Lecticity== "Oligolectic")
ggplot(b1, aes(Brain.weight)) +
  geom_histogram()
#Test for statistical differences
wilcox.test(a1$Brain.weight, b1$Brain.weight)
wilcox.test(a1$residuals, b1$residuals)

#Plotting----

#First graph brain weight
stat.test4 <- d1 %>%
wilcox_test(Brain.weight ~ Lecticity) %>%
add_significance()
stat.test4 <- stat.test4 %>% add_xy_position(x = "Lecticity")
#Build plot
p2_1 = ggplot(d1, aes(x = Lecticity, y = Brain.weight)) + 
geom_violin(alpha=0.4, fill="#d7e1ee",linewidth=0)+
geom_boxplot(colour="black",fill="grey",
width = .15, 
outlier.shape = NA) +
geom_jitter(alpha=0.3,width = 0.1) +
ylab("Absolute brain size")+
xlab("Diet")+
stat_pvalue_manual(stat.test4, label =  "Wilcoxon test, p = {p}",vjust = -0.05, bracket.nudge.y = 0.05) +
scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
theme_bw()


#Second graph residuals
stat.test5 <- d1 %>%
wilcox_test(residuals ~ Lecticity) %>%
add_significance()
stat.test5 <- stat.test5 %>% add_xy_position(x = "Lecticity")
#Build plot
p2_2 = ggplot(d1, aes(x = Lecticity, y = residuals)) + 
geom_violin(alpha=0.4, fill="#d7e1ee",linewidth=0)+
geom_boxplot(colour="black",fill="grey",
width = .15, 
outlier.shape = NA) +
geom_jitter(alpha=0.3,width = 0.1) +
ylab("Relative brain size")+
xlab("Diet")+
stat_pvalue_manual(stat.test5, label =  "Wilcoxon test, p = {p}",vjust = -0.05, bracket.nudge.y = 0.05) +
scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
theme_bw()

panel2 =  p2_1 + p2_2


library(cowplot)

plot_grid(plot_grid(panel1, labels = c('A'))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)),

plot_grid(panel2, labels = c('B'))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)), nrow = 2)





