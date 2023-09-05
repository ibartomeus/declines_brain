
library(dplyr)
library(stringr)
library(readr)


#Read data
short = readRDS("Data/Processing/wit.mean.rds")
long = read_csv("Raw_data/all_cleaning.csv")


s = short %>% 
summarise(mean = mean(Brain.weight), sd = sd(Brain.weight)) %>% 
pull(sd)

#
l = long %>% 
group_by(Species) %>% 
summarise(std = sd(Brain.weight)) %>% 
ungroup() %>% 
summarise(mean = mean(std,  na.rm=TRUE)) %>% 
pull(mean)


#Load library
library(lme4)
ggplot(long, aes(Brain.weight)) +
geom_histogram()
shapiro.test(log(long$Brain.weight))
qqnorm(log(long$Brain.weight))
qqline(log(long$Brain.weight))

#Mixed model 
m1 <- lmer(log(Brain.weight) ~ 1 | Species, data = long)
performance::icc(m1)

