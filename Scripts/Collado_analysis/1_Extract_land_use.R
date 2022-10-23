
#Read data ----

data = fread("Data/Usa_data/usa_all_long_lat_thesis.csv.gz")

#Fix colname position
names(data)[1:(ncol(data)-1)] <- names(data)[2:ncol(data)]
data[, ncol(data)] <- NULL

#Select cols of interest
data = data %>% 
dplyr::select(gen_sp, lat, long, country, state, county, city, site, habitat.extracted) %>% 
rename(species = gen_sp) 

#Filter species
#Select species of interest
d <- read.csv("Data/Processing/Especies_para_buscar.csv", row.names = 1)
#spp
spp = d$x
#Species
data=data %>% 
filter(species %in% spp)
#More than 100 records
data = data %>% 
group_by(species) %>% filter(n() >= 100) %>% ungroup()

#Extract land use ----
NLCD <- raster("Data/Raster_USA/nlcd_2011_land_cover_l48_20210604.img")
coords <- data[, c("long", "lat")]  
#convert lat/lon to appropriate projection
names(coords) <- c("x", "y")
coordinates(coords) <- ~x + y
proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs_args <- NLCD@crs@projargs
Datos_transformed <- spTransform(coords, CRS(crs_args))
#extract land cover data for each point, given buffer size
data$num.codes <- raster::extract(NLCD, Datos_transformed)
    
# summarize each site's data by proportion of each cover type
summ <- lapply(data$num.codes, function(x){
        prop.table(table(x))
}
)
    
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
write_csv(data, file=gzfile("Data/Usa_data/land_cover_usa_thesis_ma.csv.gz"))
