#Plotting on ggmap
#Load libraries
library(data.table)
library(tidyverse)
library(ggmap)

#Register my personal API
register_google(key = "AIzaSyDO3arXW_Uc7lsTtoxmTi2ZO6PwOozCjgI") 

#Load clean data with bee occurrences in Europe
europe <- read.table("Data/Europe_data/all_above_50_europe.csv.gz",  header=T, quote="\"", sep=",")

#Plotting all
cda<-get_googlemap("Amsterdam",zoom=5,maptype="satellite")

ggmap(cda) + 
    geom_point(data=europe,
               aes(x=long,y=lat),
               size=0.01,alpha=.1,stroke= 0.2, color="red") 

