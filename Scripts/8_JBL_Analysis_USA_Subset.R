#Analysis of USA bee preferences

#Load libraries
library(tidyverse) 
library(brms)

#Load data ----
#Load brain data
brain_weight = read_csv("Data/brain_weight_data.csv")
#Load preferences
preferences = read_csv("Data/Usa_data/preferences_usa_subset.csv") 
#Load phylogenetic matrix to correct for in analysis
A10 = readRDS("Data/Usa_data/phylo_usa.rds")

#Prepare data----
d = left_join(preferences, brain_weight) %>% 
    mutate(Species = str_replace_all(Species, " ", "_"))
#Convert to long to model everything at the same time
long_data = d %>% gather(Habitat, Preference, 2:4, -c(Species))


#Analysis Preference ~ residuals----
model1 = brm(Preference ~ residuals * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())

ce1 <- conditional_effects(model1, effects = "residuals:Habitat",points=T) 

bayes_R2(model1)

p1 = ggplot(ce1[[1]], aes(x = residuals, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Residuals") + 
    ggtitle("USA")

#Save data and model1 output
write_csv(long_data , "Data/Usa_data/data_preference_residuals_usa_subset.csv")
write_csv(ce1[[1]], "Data/Usa_data/model_output_preference_residuals_usa_subset.csv")

#Analysis Preference ~ brain weight----
model2 = brm(Preference ~ Brain.weight * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())

pp_check(model2)
bayes_R2(model2)
ce2 <- conditional_effects(model2, effects = "Brain.weight:Habitat",points=T) 

p2 = ggplot(ce2[[1]], aes(x = Brain.weight, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Brain weight") +
    ggtitle("USA")    

#Save data
write_csv(ce2[[1]], "Data/Usa_data/model_output_preference_brain_weight_usa_subset.csv")

#Analysis Preference ~ IT----
model3 = brm(Preference ~ IT * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())

pp_check(model3)
bayes_R2(model3)
ce3 <- conditional_effects(model3, effects = "IT:Habitat",points=T) 

p3 = ggplot(ce3[[1]], aes(x = IT, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = IT, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Intertegular distance") +
    ggtitle("USA")  

#Save data
write_csv(ce3[[1]], "Data/Usa_data/model_output_preference_it_usa_subset.csv")

#Plot all together
library(patchwork)
p1 / p2 / p3
