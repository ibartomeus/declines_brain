#Read clean USA data and extract land use from raster file
#Load libraries
library(data.table)
library(tidyverse)
library(terra)
#Load data
data <- read.table("Data/Usa_data/all_above_50_usa.csv.gz",  header=T, quote="\"", sep=",") 

#check levels, min 106 max just over 15.0000
data %>% 
group_by(species) %>% 
summarize(no_rows= length(species))

#Extract land use
#Adapted code from  https://stackoverflow.com/a/74418321/13362117
#Extract land use ----
#Raster data downloaded from: https://www.mrlc.gov/data/nlcd-2006-land-cover-conus
NLCD <- rast("Data/Raster_USA/nlcd_2006_land_cover_l48_20210604.img")

#Create vector with coordinates and correct projection
coord <- vect(data, c("long", "lat"), crs="+proj=longlat") |> project(NLCD)    
#Add cover names to data
cover.names <- extract(NLCD, coord, ID=FALSE)
data$cover.names <- cover.names$`NLCD Land Cover Class`
#Select cols of interest
data = data %>% dplyr::select(species, cover.names)
#Save data
write_csv(data, file=gzfile("Data/Usa_data/land_cover_usa.csv.gz"))

