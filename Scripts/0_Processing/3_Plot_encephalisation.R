


#Plot encephalization
#Load libraries
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)

#Read data
m = read_csv("Data/Processing/residuals_model.csv")
r = read_csv("Data/Processing/residuals_data.csv")
d = read_csv("Data/Processing/taxonomic_rank.csv") %>% 
mutate(Species = str_replace_all(query, " ", "_")) %>% 
select(!c(query, db))

#Add taxonomic rank to dataset
r = left_join(r,d)

#Add ggplot
ggplot(m, aes(x = log_it, y = estimate__)) +
geom_line() +
geom_point(data = r, aes(log_it, log_brain_weight, color = subfamily)) + 
theme_bw() +
ylab("log(Brain size)") +
xlab("log(Body size)") #+
#theme(legend.box.background = element_rect(colour = "black"))




