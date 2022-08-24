#Analysis of Europe bee preferences

#Load libraries
library(tidyverse) 
library(brms)

#Load data ----
#Load brain data
brain_weight = read_csv("Data/drain_weight_data.csv")
#Load preferences
preferences_europe = read_csv("Data/Europe_data/preferences_europe.csv") 
preferences_usa = read_csv("Data/Usa_data/preferences_usa.csv") 
#merge datasets
preferences = bind_rows(preferences_europe, preferences_usa)

#Load phylogenetic matrix to correct for in analysis
A10 = readRDS("Data/phylo_all.rds")

#Prepare data----
d = left_join(preferences, brain_weight) %>% 
    mutate(Species = str_replace_all(Species, " ", "_"))

#Convert to long to model everything at the same time
long_data = d %>% gather(Habitat, Preference, 2:5, -c(Species))

#Analysis preference~brain weight----
model1 = brm(Preference ~ Brain.weight * Habitat + (1|gr(Species, cov = A)), 
             data = long_data,prior = prior1, data2 = list(A = A10), family=zero_one_inflated_beta())

pp_check(model1,nsamples=100) +ylim(0,5)
bayes_R2(model1)
marginal_effects(model1)

#Plotting preference~brain weight----
ce1 <- conditional_effects(model1, effects = "Brain.weight:Habitat",points=T) 

ggplot(ce1[[1]], aes(x = Brain.weight, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Brain weight") +
    ggtitle("Europe and USA occurrences")


#Analysis preference~brain weight----
model2 = brm(Preference ~ IT * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())

pp_check(model2,nsamples=100) +ylim(0,5)
bayes_R2(model2)
marginal_effects(model2)

#Plotting preference~brain weight----
ce2 <- conditional_effects(model2, effects = "IT:Habitat",points=T) 

ggplot(ce2[[1]], aes(x = IT, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("IT distance") +
    ggtitle("Europe and USA occurrences")
