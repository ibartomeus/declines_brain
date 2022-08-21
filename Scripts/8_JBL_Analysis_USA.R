#Analysis of USA bee preferences

#Load libraries
library(tidyverse) 
library(brms)

#Load data ----
#Load brain data
brain_weight = read_csv("Data/drain_weight_data.csv")
#Load preferences
preferences = read_csv("Data/Usa_data/preferences_usa.csv") 
#Load phylogenetic matrix to correct for in analysis
A10 = readRDS("Data/Usa_data/phylo_usa.rds")

#Prepare data----
d = left_join(preferences, brain_weight) %>% 
mutate(Species = str_replace_all(Species, " ", "_"))
#Convert to long to model everything at the same time
long_data = d %>% gather(Habitat, Preference, 2:5, -c(Species))


#Analysis----
s= get_prior(Preference ~ residuals * Habitat + (1|gr(Species, cov = A)), warmup = 500, iter = 2000,
             data = long_data, data2 = list(A = A10), family=zero_one_inflated_beta())


prior1 <- c(set_prior("normal(0,1)", class = "b", coef = "HabitatNatural"),
            set_prior("normal(0,1)", class = "b", coef = "HabitatSeminatural"),
            set_prior("normal(0,1)", class = "b", coef = "HabitatUrban"))


model = brm(Preference ~ Brain.weight * Habitat + (1|gr(Species, cov = A)), warmup = 500, iter = 2000,
                  data = long_data,prior=prior1, data2 = list(A = A10), family=zero_one_inflated_beta())


#Check model fit
pp_check(model,nsamples=100) + ylim(0,5)
#Check r
performance::r2(model)
#Check output
marginal_effects(model)


