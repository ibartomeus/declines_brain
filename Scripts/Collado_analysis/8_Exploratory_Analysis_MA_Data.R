
#Read data ----

data = fread("Data/Usa_data/usa_all_long_lat_thesis.csv.gz")

#Fix colname position
names(data)[1:(ncol(data)-1)] <- names(data)[2:ncol(data)]
data[, ncol(data)] <- NULL

data = data %>% 
dplyr::select(gen_sp, lat, long, country, state, county, city, site, habitat.extracted) %>% 
rename(species = gen_sp) 

#data = data %>% 
#group_by(species) %>% filter(n() >= 100) %>% ungroup()

    
#Extract land use ----
year=2011
extract_cover <- function(year,
                          point_d = data,  
                          data_dir="Data/Raster_USA",
                          write_dir="Data/Usa_data"){
    require(raster)
    require(rgdal)
    require(stringr)
    
    # load cover data 

    filename <- paste(data_dir, "/nlcd_",
                      year,
                      "_land_cover_l48_20210604.img",
                      sep="")
    NLCD <- raster(filename)
    
    # load site data
    Datos <- data
    #según tus datos
    coords <- Datos[, c("long", "lat")]  
    #convert lat/lon to appropriate projection
    names(coords) <- c("x", "y")
    coordinates(coords) <- ~x + y
    proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    crs_args <- NLCD@crs@projargs
    Datos_transformed <- spTransform(coords, CRS(crs_args))
    
    #extract land cover data for each point, given buffer size
    Landcover <- raster::extract(NLCD, Datos_transformed)
    
    # summarize each site's data by proportion of each cover type
    summ <- lapply(Landcover, function(x){
        prop.table(table(x))
    }
    )
    
    # generate land cover number to name conversions
    num.codes <- unique(unlist(Landcover))
    cover.names <- NLCD@data@attributes[[1]]$NLCD.Land.Cover.Class[num.codes + 1]
    levels(cover.names)[1] <- NA # first level is ""
    conversions <- data.frame(num.codes, cover.names)
    conversions <- na.omit(conversions)
    conversions <- conversions[order(conversions$num.codes),]
    
    # convert to data frame
    mydf <- data.frame(cover = names(unlist(summ)))
    
    mydf <- cbind(mydf,Datos)
    
    # create cover name column
    mydf$num.codes <- mydf$cover
    
    mydf <- merge(conversions, mydf, by = "num.codes")
    #colnames(mydf) <- c("Species", "Cover_names")
    # write output
    out_name <- paste0(write_dir, "/", "land_cover_", 
                       "usa_thesis_ma.csv.gz")
    
    write_csv(mydf, file=gzfile(out_name))
}


#Extract cover with function
extract_cover(year = 2011, point_d = data, write_dir = "Data/Usa_data")


pref$cover.names==data$habitat.extracted


#Calculate preferences----
#Load library
library(tidyverse)
library(bipartite)

#Load extracted data
pref <- read_csv("Data/Usa_data/land_cover_usa_thesis_ma.csv.gz") %>% 
    dplyr::select(species, cover.names) %>% 
    mutate_if(is.character,as.factor) 

#Check levels of species and cover names
#First, species
pref %>% 
    group_by(species) %>% 
    summarise(n_rows = length(species))
#Second, cover.names
S = pref %>% 
    group_by(cover.names) %>% 
    summarise(n_rows = length(cover.names))

#Recode levels to calculate preferences
#Check pdf with landcover types in Data/Usa_data
#and make decisions based on that
pref = pref %>% 
    mutate(cover.names = fct_recode(as.factor(cover.names),
                                    Discard = "Barren Land", #Very different cover type, not included (MA & JB)
                                    Agricultural = "Cultivated Crops", #(MA & JB)
                                    Natural = "Deciduous Forest", #(MA & JB)
                                    Urban = "Developed, High Intensity", #Greater than 50% of cover (MA & JB) 
                                    Urban = "Developed, Medium Intensity", #Greater than 50% of cover (MA & JB) 
                                    Seminatural = "Developed, Low Intensity", #Low urban impact (MA & JB)
                                    Seminatural = "Developed, Open Space", #Open areas with vegetation (MA & JB)
                                    Natural = "Emergent Herbaceous Wetlands", #(MA & JB)
                                    Natural = "Evergreen Forest", #(MA & JB)
                                    Agricultural = "Hay/Pasture", #Highly managed by humans (MA & JB)
                                    Natural = "Herbaceous", #See explanation below (MA & JB)
                                    Natural = "Mixed Forest", #(MA & JB)
                                    Discard = "Open Water", #Low vegetation and land, not included (MA & JB)
                                    Natural = "Shrub/Scrub", #Heath classified as natural (MA & JB)
                                    Natural = "Woody Wetlands")) %>% #(MA & JB)
    filter(!cover.names == "Discard") #Discard these categories classified as "Discard"

#Herbaceous and hay/pasture are classified as two different habitats in NLCD. 
#We merged them because herbaceous areas in our sampling region are always 
#for livestock (Koh et al., 2016).

#Check levels again
check_cover = pref %>% 
    group_by(cover.names) %>% 
    summarise(n_rows = length(cover.names))

#Now prepare data to calculate preferences
pref.table = pref %>% 
    count(species, cover.names) %>%
    pivot_wider(names_from = cover.names, values_from = n, values_fill = list(n = 0)) %>% 
    column_to_rownames(var="species") 

#Null model of our pref.table
n.mod <- nullmodel(pref.table, N=10000, method="r2dtable")

#Calculate preferences
#Alternative way, probably more elegant

m = pref.table #create matrix to store data

colnames(pref.table)

#Agricultural
pref_agr = pref.table %>% dplyr::select(Agricultural)
n.mod_agr = list()
for(n in 1:1000){
    n.mod_agr[[n]] = n.mod[[n]][,1]           
} 

for(k in 1:nrow(pref_agr)){
    v <- lapply(n.mod_agr, `[[`, k)
    m[k,1] <- sum(unlist(v) <  pref.table[k,1] ) / length(unlist(v)) 
    
}

#Natural
pref_nat = pref.table %>% dplyr::select(Natural)
n.mod_nat = list()
for(n in 1:1000){
    n.mod_nat[[n]] = n.mod[[n]][,2]           
} 

for(k in 1:nrow(pref_nat)){
    v <- lapply(n.mod_nat, `[[`, k)
    m[k,2] <- sum(unlist(v) <  pref.table[k,2] ) / length(unlist(v)) 
    
}
#Urban
pref_urb = pref.table %>% dplyr::select(Urban)
n.mod_urb = list()
for(n in 1:1000){
    n.mod_urb[[n]] = n.mod[[n]][,3]           
} 

for(k in 1:nrow(pref_urb)){
    v <- lapply(n.mod_urb, `[[`, k)
    m[k,3] <- sum(unlist(v) <  pref.table[k,3] ) / length(unlist(v)) 
    
}

#Seminatural
pref_sem = pref.table %>% dplyr::select(Seminatural)
n.mod_sem = list()
for(n in 1:1000){
    n.mod_sem[[n]] = n.mod[[n]][,4]           
} 

for(k in 1:nrow(pref_sem)){
    v <- lapply(n.mod_sem, `[[`, k)
    m[k,4] <- sum(unlist(v) <  pref.table[k,4] ) / length(unlist(v)) 
    
}

m = rownames_to_column(m, var = "Species")
write_csv(m, "Data/Usa_data/preferences_usa_thesis_ma.csv")

#Model data -----
#Load data ----
#Load brain data
brain_weight = read_csv("Data/drain_weight_data.csv")
#Load preferences
preferences = read_csv("Data/Usa_data/preferences_usa_thesis_ma.csv") 
#Create unique dataset
d = left_join(preferences, brain_weight) %>% 
    mutate(Species = str_replace_all(Species, " ", "_"))
#Convert to long to model everything at the same time
long_data = d %>% gather(Habitat, Preference, 2:5, -c(Species))
#Model
model1 = brm(Preference ~ residuals * Habitat, 
             data = long_data, family=zero_one_inflated_beta())

ce1 <- conditional_effects(model1, effects = "residuals:Habitat",points=T) 

bayes_R2(model1)

p1 = ggplot(ce1[[1]], aes(x = residuals, y = estimate__, color=Habitat)) +
    geom_point(data =  long_data, aes(x = residuals, y = (Preference)), shape=21) +
    geom_line(aes(color=Habitat)) +
    theme_bw() +
    ylab("Habitat preference") +
    xlab("Residuals") + 
    ggtitle("USA")
