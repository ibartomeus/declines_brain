

#Load brain data
brain_weight = read_csv("Data/Processing/brain_weight_data.csv")
#Load preferences
preferences_europe = read_csv("Data/Europe_data/preferences_europe_qualitative.csv") 
preferences_usa = read_csv("Data/Usa_data/preferences_usa_qualitative.csv") 

#Extract sepcecies from preferences
sp = c(preferences_europe$Species, preferences_usa$Species)
#Select final species from the trait datatset
brain_weight = brain_weight %>% 
filter(Species %in% sp)

#Add subfamily to species dataset
d = read_csv("Data/Processing/taxonomic_rank.csv") %>% 
mutate(Species = query) %>% 
select(!c(query, db))
#Add taxonomic rank to dataset
brain_weight = left_join(brain_weight,d)

brain_subfamily = brain_weight %>% 
group_by(subfamily) %>% 
summarise(mean_sub = mean(residuals),
          sd_sub = sd(residuals),
          n_lev = n()) %>% 
mutate(se_sub = sd_sub / sqrt(n_lev),
         lower_ci = mean_sub - qt(1 - (0.05 / 2), n_lev - 1) * se_sub,
         upper_ci = mean_sub + qt(1 - (0.05 / 2), n_lev - 1) * se_sub)


ggplot(brain_subfamily, aes(mean_sub, subfamily)) +
geom_point() +
geom_errorbar(aes(xmin = lower_ci,
xmax = upper_ci))
  
  
  