#Analysis of Europe bee preferences

#Load libraries
library(tidyverse) 
library(brms)

#Load data ----
#Load brain data
brain_weight = read_csv("Data/Processing/brain_weight_data.csv")
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

#Prepare col brain weight/IT
long_data = long_data %>% 
    mutate(brain_it = Brain.weight/IT)

#Analysis Preference ~ residuals----
model1 = brm(Preference ~ residuals * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())

ce1 <- conditional_effects(model1, effects = "residuals:Habitat",points=T) 

bayes_R2(model1)
marginal_effects(model1)

p1 = ggplot(ce1[[1]], aes(x = residuals, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21) +
geom_line(aes(color=Habitat)) +
theme_bw() +
ylab("Habitat preference") +
xlab("Residuals") + 
ggtitle("USA and Europe")

#Save data and model1 output
write_csv(long_data , "Data/Europe_USA/data_preference_residuals_europe_usa.csv")
write_csv(ce1[[1]], "Data/Europe_USA/model_output_preference_residuals_europe_usa.csv")

#Analysis Preference ~ brain weight----
model2 = brm(Preference ~ Brain.weight * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())

pp_check(model2)
bayes_R2(model2)
marginal_effects(model2)

ce2 <- conditional_effects(model2, effects = "Brain.weight:Habitat",points=T) 

p2 = ggplot(ce2[[1]], aes(x = Brain.weight, y = estimate__, color=Habitat)) +
geom_point(data =  long_data, aes(x = Brain.weight, y = (Preference)), shape=21) +
geom_line(aes(color=Habitat)) +
theme_bw() +
ylab("Habitat preference") +
xlab("Brain weight") +
ggtitle("USA and Europe") 

#Save data
write_csv(ce2[[1]], "Data/Europe_USA/model_output_preference_brain_weight_europe_usa.csv")


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
ggtitle("USA and Europe")  

#Save data
write_csv(ce3[[1]], "Data/Europe_USA/model_output_preference_it_europe_usa.csv")

#Plot all together
library(patchwork)
p1 / p2 / p3

#Analysis Preference ~ brain_it----
model4 = brm(Preference ~ brain_it * Habitat + (1|gr(Species, cov = A)), 
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())

ce4 <- conditional_effects(model4, effects = "brain_it:Habitat",points=T) 

p4 = ggplot(ce4[[1]], aes(x = brain_it, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = brain_it, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("brain_it") +
    ggtitle("USA") 

#Save data
write_csv(ce4[[1]], "Data/Europe_USA/model_output_preference_brain_it_europe_usa.csv")


