#Calculate study site areas

#Load libraries
library(sf)
library(sfheaders)

#EUROPE:
#Read data
euro_map <- st_read("Data/Europe_data/euro_map_1.shp")
#Add area col
euro_map$area <- st_area(euro_map)
#Create dataframe with the correct units
d1 = euro_map %>% group_by(CNTR_CODE) %>% summarize(st_union(geometry), area_NAME = sum(area)) %>% 
mutate(area_NAME1 = units::set_units(area_NAME, value = "km^2"))
#Calculate area
sum(d1$area_NAME1)

#USA:
#Read data
usa_states_1 <- read.csv("Data/Usa_data/usa_states.csv")
#Not an sf object, convert into multypolygon
usa = sfheaders::sf_multipolygon(
  obj = usa_states_1
  , multipolygon_id = "group"
  , x = "long"
  , y = "lat"
) %>% st_set_crs(4326)
#Calculate area
usa$area <- st_area(usa)
#Create dataframe with the correct units
d2 = usa %>% group_by(group) %>% summarize(st_union(geometry), area_NAME = sum(area)) %>% 
mutate(area_NAME1 = units::set_units(area_NAME, value = "km^2"))
#Sum areas
sum(d2$area_NAME1)
usakm2 = round(sum(d2$areakm2),0)
