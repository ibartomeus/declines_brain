#Read clean USA data and extract land use from raster file
#Load libraries
library(data.table)
library(tidyverse)
library(raster)
#Load data
data <- read.table("Data/Usa_data/all_above_50_usa.csv.gz",  header=T, quote="\"", sep=",") %>% 
  slice(1:25)

#check levels, min 106 max just over 15.0000
data %>% 
group_by(species) %>% 
summarize(no_rows= length(species))

#Extract land use
#Adapted code from this example https://www.r-bloggers.com/2014/11/spatial-data-extraction-around-buffered-points-in-r/
#Extract land use ----
NLCD <- raster("Data/Raster_USA/nlcd_2006_land_cover_l48_20210604.img")

coords <- data[, c("long", "lat")]  
#convert lat/lon to appropriate projection
names(coords) <- c("x", "y")
coordinates(coords) <- ~x + y
proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs_args <- NLCD@crs@projargs
Datos_transformed <- spTransform(coords, CRS(crs_args))
#extract land cover data for each point
data$num.codes <- raster::extract(NLCD, Datos_transformed)

# generate land cover number to name conversions
num.codes <- unique(unlist(data$num.codes))
cover.names <- NLCD@data@attributes[[1]]$NLCD.Land.Cover.Class[num.codes + 1]
levels(cover.names)[1] <- NA # first level is ""
conversions <- data.frame(num.codes, cover.names)
conversions <- na.omit(conversions)
conversions <- conversions[order(conversions$num.codes),]
#left_join
data = left_join(data,conversions)
#Select cols of interest
data = data %>% dplyr::select(species, cover.names)
#Save data
write_csv(data, file=gzfile("Data/Usa_data/land_cover_usa.csv.gz"))

