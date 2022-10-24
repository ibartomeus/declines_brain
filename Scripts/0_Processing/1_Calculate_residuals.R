#In this script we calculate the residuals from brain weight ~ Intertegular distance
#Note that we model and correct for phylogenetic relatedness across species

#Load libraries----
library(tidyverse)

wit.mean = readRDS("Data/Processing/wit.mean.rds")

#Log variables first
wit.mean = wit.mean %>% 
mutate(log_brain_weight = log(Brain.weight)) %>% 
mutate(log_it = log(IT))    
#Check scatter
ggplot(wit.mean, aes(x= IT, y = Brain.weight)) + geom_point()
ggplot(wit.mean, aes(x= log_it, y = log_brain_weight)) + geom_point()

#Read with phylogenetic distance to add in the modelling process
phylo =readRDS("Data/Processing/phylo_raw.rds")

#Add under score to unify species names
wit.mean = wit.mean %>% 
mutate(Species = str_replace_all(Species, " ", "_"))

#Model with phylo to extract residuals
library(brms)

m1 <-  brm(log_brain_weight ~ log_it + (1|gr(Species, cov = A)) , data = wit.mean,
                 data2 = list(A = phylo),family=gaussian(), iter=4000, 
                 warmup = 1000)

#Model diagnostics
pp_check(m1)
pp_check(m1, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "log_it")
#Looks good

res_df <- m1$data %>% 
    mutate(predict_y = predict(m1)[ , "Estimate"], 
           std_resid = residuals(m1, type = "pearson")[ , "Estimate"])
ggplot(res_df, 
       aes(predict_y, std_resid)) + 
    geom_point(size = 0.8) + stat_smooth(se = FALSE)
#Better model diagnostics with log of brain and it
#Not a clear trend, nice too

#Extract residuals (estimates) 
wit.mean$residuals = residuals(m1)[,1]

#Check species
wit.mean$Species

wit.mean = wit.mean %>% 
    mutate(Species=str_replace_all(Species, "_", " "))

#Save species to search for their occurrence data
write.csv(wit.mean$Species, "Data/Processing/Especies_para_buscar.csv")
#Save data to merge in scripts number 8 for analysis
write_csv(wit.mean, "Data/Processing/brain_weight_data.csv")

