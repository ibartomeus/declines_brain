#Read clean USA data and extract land use from raster file
#Load libraries
library(data.table)
library(tidyverse)

#Load data
usa <- read.table("Data/Usa_data/all_above_50_usa.csv.gz",  header=T, quote="\"", sep=",")

#check levels, min 106 max just over 15.0000
usa %>% 
group_by(species) %>% 
summarize(no_rows= length(species))

#Extract land use
#Code from using this code as an example https://www.r-bloggers.com/2014/11/spatial-data-extraction-around-buffered-points-in-r/

year=2011
#point_d = "Data/Usa_data/all_above_50_usa.csv.gz"
#data_dir="NLCD_data"
#write_dir="Data/Usa_data"

extract_cover <- function(year,
                          point_d = "Data/Usa_data/all_above_50_usa.csv.gz",  
                          data_dir="NLCD_data",
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
    Datos <- read.table(point_d,  header=T, quote="\"", sep=",") #Si falla, usar como sep "," o ";"
    #según tus datos
    coords <- Datos[, c("longitude", "latitude")]  
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
    
    # write output
    out_name <- paste0(write_dir, "/", "land_cover_", 
                      "usa.csv.gz")
    
    write_csv(mydf, file=gzfile(out_name))
}


#Extract cover with function
extract_cover(year = 2011, point_d = "Data/Usa_data/all_above_50_usa.csv.gz", write_dir = "Data/Usa_data")

