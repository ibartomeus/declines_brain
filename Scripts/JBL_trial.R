###############################################################---
#Extract coordinates from polygon and explore graphically----
###############################################################---
library(ggplot2) #plotting v.3.3.5
library(dplyr) #data cleaning v.1.0.7
library(geosphere) #Calculate area within polygon
library(sf)
#read data to avoid running all again
d <- read.csv("Data/gbif_data.csv")

#Load worldmap
world <- map_data("world")
#Plot spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region),
                    color = "white", fill = "lightgray", size = 0.05) +
    geom_point(data = d,aes(decimalLongitude, decimalLatitude),
               alpha = 0.7, size = 0.1) 


#Separate by continent
namerica <- d %>% filter(Continent %in% c("North America"))
europe <- d %>% filter(Continent %in% c("Europe"))

#Plot north america spatial data
ggplot() + geom_map(data = world, map = world,
                    aes(long, lat, map_id = region), color = "white", 
                    fill = "lightgray", size = 0.1) +
    geom_point(data = namerica,aes(decimalLongitude, decimalLatitude),
               alpha = 0.1, size = 0.05) + ylim(0,70) +
    coord_sf(xlim = c(-120, -50), ylim = c(20, 70), expand = FALSE)

#East coast seems to be the area with highest point density
#select by state

#Closer look
library(spData) #to load us_states data

#Create USA polygon
polygon <- us_states %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")  %>% sf::st_as_sf(crs = 4326) %>%
    sf::st_transform(crs = 4326) 

#set same coordinate system
usa <- namerica %>% 
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), dim = "XY") %>% 
    st_set_crs(4326)

#Select points within USA
usa_points <- st_intersection(polygon,usa)

#separate geometry to long lat
separated_coord <- usa_points %>%
    mutate(long = unlist(map(usa_points$geometry,1)),
           lat = unlist(map(usa_points$geometry,2)))

#select points within USA
separated_coord <- as.data.frame(separated_coord)

#Plot north america spatial data
ggplot() + geom_map(data = states, map = states,
                    aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
                    size = 0.1) +guides(fill=FALSE)+
    geom_point(data = separated_coord,aes(long, lat),
               alpha = 0.3, size = 0.05) + 
    coord_sf(xlim = c(-100, -60), ylim = c(25, 50), expand = FALSE)


#Now extract states of interest of USA
states <- map_data("state")

#States from east coast with highest point density
vars <- c("vermont", "new hampshire", "massachusetts", "rhode island","connecticut",
          "new jersey","delaware", "maryland")
usa_states <- filter(states, region %in% vars)

#New York has also high point density for the metropolitan area
counties <-map_data("county")
#Select NY state
ny_county <- subset(counties, region=="new york")
#Select metropolitan area region of New York state
vars_ny <- c("orange",  "putnam", "rockland",
             "westchester", "bronx", "new york","kings", "richmond", "queens", "nassau","suffolk")
ny_metropolitan <- filter(ny_county, subregion %in% vars_ny)


usa_high_density <- rbind(ny_metropolitan, usa_states)


#Plot north america spatial data
ggplot() + geom_map(data = usa_high_density, map = usa_high_density,
                    aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
                    size = 0.1) +guides(fill=FALSE)+
    geom_point(data = separated_coord,aes(long, lat),
               alpha = 0.3, size = 0.05) + 
    coord_sf(xlim = c(-100, -60), ylim = c(25, 50), expand = FALSE)


#Now select points that are just in this area

a <- usa_high_density[1:100] %>% 
    st_as_sf(coords = c("long", "lat"), dim = "XY") %>% st_set_crs(4326)%>%
    sf::st_as_sf(crs = 4326)

b <- separated_coord %>% 
    st_as_sf(coords = c("long", "lat"), dim = "XY") %>% st_set_crs(4326)%>%
    sf::st_as_sf(crs = 4326)

east_coast <- st_intersection(a,b)

#Plot area of interest with high density
ggplot() + geom_map(data = usa_high_density, map = usa_high_density,
aes(long, lat, map_id = region, fill=region, group = group), color = "white", 
size = 0.1) +guides(fill=FALSE)+geom_point(data = east_coast,aes(long, lat),
alpha = 0.3, size = 0.05) + coord_sf(xlim = c(-100, -60), ylim = c(25, 50), expand = FALSE)

