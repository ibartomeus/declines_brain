#Read clean Europe data and extract land use from raster file
#Raster from Europe downloaded from: 
#https://land.copernicus.eu/pan-european/corine-land-cover/clc-2006/fetch-land-file?hash=9ed7266120d5e2869c20b5142f637148b4cf569d

#Load libraries
library(data.table)
library(tidyverse)

#Load clean data with bee occurrences in Europe
europe <- read.table("Data/Europe_data/all_above_50_europe.csv.gz",  header=T, quote="\"", sep=",")

#check levels, min 106 max just over 15.0000
europe %>% 
group_by(species) %>% 
summarize(no_rows= length(species))

#Read raster data
library(terra)
r <- rast("Data/Raster_Europe/Data/U2012_CLC2006_V2020_20u1.tif")

head(cats(r)[[1]])

#Create vector with coordinates
lon <- europe %>% pull(long)
lat <- europe %>% pull(lat)
xy <- cbind(lon, lat) 
v <- vect(xy, crs="+proj=longlat")
vv <- project(v, crs(r)) 

#Extract
activeCat(r) <- "LABEL3" #this has the categories of interest
#Extract land use
land_cover = extract(r, vv)

#Check levels
land_cover %>% 
group_by(LABEL3) %>% 
summarize(n_rows = length(LABEL3))

factor(levels(land_cover$LABEL3))

#Merge datasets
all = cbind(europe, land_cover)

#Select cols of interest
all = all %>%
dplyr::select(species, LABEL3) %>% 
rename(Cover_names = LABEL3) %>% 
rename(Species = species)

#Check data
slice_head(all,n =5)

#Save data
write_csv(all, "Data/Europe_data/land_cover_europe.csv.gz")
