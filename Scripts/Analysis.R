#Load libraries----
library(tidyverse)

#Data Preparation----
#Load data
brains.it <- read.csv("Raw_data/brains.it.csv", dec = ",")

#Clean data
species.brains1 = brains.it %>% 
filter(!ID == "F33") %>%  #Dasypoda visnaga's brain was conserved in ethanol, not formol
rename(Brain.weight = Brain.Weight..mg.) %>% #Rename var
mutate(Brain.IT = (Brain.weight/IT)) %>% #create new col
select(Species, IT, Brain.weight, Brain.IT) %>% #select cols of interest
na.omit() #filter out na's

#Check outliers
boxplot(Brain.weight~Species,data = species.brains1, las= 2,cex.axis = 0.6)

#Check and clean outliers
#Andrena barbilabris
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Andrena barbilabris") & (Brain.weight==0.296)))
#Andrena dunningi
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Andrena dunningi") & (Brain.weight==0.600)))
#Augochlorella aurata
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Augochlorella aurata") & (Brain.weight==0.705)))
#Bombus pascuorum
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Bombus pascuorum") & (Brain.weight==4.937)))
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Bombus pascuorum") & (Brain.weight==3.932)))
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Bombus pascuorum")&(Brain.weight==3.871)))
#Bombus pratorum
species.brains1<-subset(species.brains1, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==3.626)))
species.brains1<-subset(species.brains1, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==3.278)))
species.brains1<-subset(species.brains1, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==3.121)))
species.brains1<-subset(species.brains1, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==1.070)))
species.brains1<-subset(species.brains1, 
subset = !((Species == "Bombus pratorum") & (Brain.weight==1.300)))
#Bombus terrestris
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Bombus terrestris") & (Brain.weight==5.838)))
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Bombus terrestris") & (Brain.weight==5.564)))
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Bombus terrestris") & (Brain.weight==6.370)))
#Bombus hortorum
species.brains1 <- subset(species.brains1, subset = !((Species == "Bombus hortorum")))
#Bombus lapidarius
species.brains1 <- subset(species.brains1, subset = !((Species == "Bombus lapidarius")))
#Bombus bimaculatus
species.brains1 <- subset(species.brains1, subset = !((Species == "Bombus bimaculatus")))
#Halictus ligatus
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Halictus ligatus") & (Brain.weight==0.395)))
#Osmia caerulescens
species.brains1 <- subset(species.brains1, 
subset = !((Species == "Osmia caerulescens") & (Brain.weight==1.530)))
#Xylocopa virginica
species.brains1 <-subset(species.brains1, 
subset = !((Species == "Xylocopa virginica") & (Brain.weight==6.613)))
species.brains1 <-subset(species.brains1, 
subset = !((Species == "Xylocopa virginica") & (Brain.weight==5.787)))

#Create dataframe with average measurements of IT and brain weight
weights.mean <- data.frame(aggregate(Brain.weight ~ Species, data = species.brains1, FUN = mean))
IT.mean <- data.frame(aggregate(IT ~ Species, data = species.brains1, FUN = mean))
wit.mean1 <- merge(weights.mean, IT.mean)

#Linearizamos y añadimos la variable de residuales general de cada especie
#now is one value per species
plot(Brain.weight ~ IT, data=wit.mean)
abline(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(Brain.weight ~ IT, data=wit.mean))
summary(lm(log(Brain.weight) ~ log(IT), data=wit.mean))
lm.data.means<-lm(log(Brain.weight) ~ log(IT), data=wit.mean)
lm.data.means$residuals
lm.data.means$residuals
wit.mean$residuals <- lm.data.means$residuals

#Filter out non-species
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena sp 3")))
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Megascolia sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Halictus sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Eucera sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Dasypoda sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Coelioxys sp.")))
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena (Chlorandrena)")))
wit.mean<-subset(wit.mean, subset = !((Species == "Andrena (Chrysandrena)")))

###CHECK
#Review this step with MA
#Descuento las especies que tenemos ya todo los datos, para hacer una lista de potenciales
potentials <- wit.mean
potentials<-subset(potentials, subset = !((Species == "Agapostemon virescens")))
potentials<-subset(potentials, subset = !((Species == "Andrena carlini")))
potentials<-subset(potentials, subset = !((Species == "Andrena nasonii")))
potentials<-subset(potentials, subset = !((Species == "Augochlora pura")))
potentials<-subset(potentials, subset = !((Species == "Augochlorella aurata")))
potentials<-subset(potentials, subset = !((Species == "Bombus griseocollis")))
potentials<-subset(potentials, subset = !((Species == "Calliopsis andreniformis")))
potentials<-subset(potentials, subset = !((Species == "Ceratina strenua")))
potentials<-subset(potentials, subset = !((Species == "Halictus confusus")))
potentials<-subset(potentials, subset = !((Species == "Halictus rubicundus")))
potentials<-subset(potentials, subset = !((Species == "Lasioglossum coriaceum")))
potentials<-subset(potentials, subset = !((Species == "Megachile mendica")))
potentials<-subset(potentials, subset = !((Species == "Melissodes bimaculata")))
potentials<-subset(potentials, subset = !((Species == "Osmia atriventris")))
potentials<-subset(potentials, subset = !((Species == "Osmia bucephala")))
potentials<-subset(potentials, subset = !((Species == "Osmia pumila")))
potentials<-subset(potentials, subset = !((Species == "Ptilothrix bombiformis")))
potentials<-subset(potentials, subset = !((Species == "Xylocopa virginica")))
#Este dataframe tiene las especies filtradas
potentials$Species
##write.csv(potentials$Species, "Especies_para_buscar.csv")

#Loa
library(data.table)
usa <- data.frame(fread("Data/Usa_data/all_above_50_usa.csv.gz"))

# Function to extract the data in R instead of Arc
# inputs: year, point data file, 
# cover data directory, and output file directory
# output: csv file with site id, cover type, and % in buffer
# Year is only used to give the output a name


names(usa)[which(names(usa)=="lat")]<-"latitude"
names(usa)[which(names(usa)=="long")]<-"longitude"
write.csv(usa,"Datos.csv")

year = 2011

extract_cover <- function(year,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
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
    Datos <- read.csv(point_d, header=T, sep=",") #Si falla, usar como sep "," o ";"
    #según tus datos
    coords <- Datos[, c("longitude", "latitude")]  
    #convert lat/lon to appropriate projection
    names(coords) <- c("x", "y")
    coordinates(coords) <- ~x + y
    proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    crs_args <- NLCD@crs@projargs
    Datos_transformed <- spTransform(coords, CRS(crs_args))
    
    #extract land cover data for each point, given buffer size
    Landcover <- extract(NLCD, Datos_transformed)
    
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
    mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                       cover = names(unlist(summ)),
                       percent = unlist(summ)
    )
    
    # create cover name column
    mydf$cover2 <- mydf$cover
    levels(mydf$cover2) <- conversions$cover.names
    
    # write output
    out_name <- paste(write_dir, "/",
                      year, "_cover_", 
                      "_m_buffer.csv", sep="")
    write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado
extract_cover(year = 2011, point_d = "Datos.csv", write_dir = "extracted")


#Cargamos justo lo que acabamos de crear
covers1 <- read.csv("Data/2011_cover__m_buffer.csv")
#Esto es la lista de id de los usos de suelo
conversions2 <- conversions
covers2<-data.frame(covers1$id,covers1$cover)
#Hacemos coincidir los nombres de las variables
names(covers2)<-c("id","num.codes")
#Coinciden
covers2
conversions2
#Vemos que cada numero está asignado a un uso de suelo
covers3<-merge(covers2,conversions2)
summary(covers3)
cbind(Datos,covers3)
#Lo juntamos a nuestro dataframe
covers4<-cbind(Datos,covers3)

summary(covers4$cover.names)

#Save data with land cover types
write.csv(covers4,"data/usa_habitats_raw.csv")


#Load data
usa_habitats_raw<-read.csv("data/usa_habitats_raw.csv")
pref <- data.frame(usa_habitats_raw$species,usa_habitats_raw$cover.names)
names(pref)<-c("species","cover.names")
pref$cover.names<-as.factor(pref$cover.names)
pref$species<-as.factor(pref$species)
summary(pref)
summary(pref)
#Convertimos long data en wide data, así sacamos cuantas ocurrencias de cada especie en cada habitat
pref.table<-aggregate(pref$cover.names~pref$species, FUN=summary, pref)
pref.table<-c(pref.table)
pref.tabletemp<-pref.table$`pref$cover.names`
rownames(pref.tabletemp)<-pref.table$`pref$species`
usa_table<-data.frame(pref.tabletemp)
str(usa_table)
str(pref.table)
names(pref.table)
View(usa_table)
write.csv(usa_table,"usa_table.csv")
#Sacamos preferencias de habitats
#Creamos modelo nulo
n.mod<-nullmodel(usa_table, N=1000, method="r2dtable")
#Buscamos el umbral de los cuantiles 0.05 para evitación y 0.95 para preferencia
#hay que hacerlo para cada habitat
BL.05=NULL
BL.95=NULL
l=NULL
which(names(usa_table)=="Barren.Land")
which(names(usa_table)=="Cultivated.Crops")
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Barren.Land")
        ]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Barren.Land")
        ]
    }
    l
    BL.05[k]=quantile(l, c(.05))
    BL.95[k]=quantile(l, c(.95))
}
BL.05
BL.95
CC.05=NULL
CC.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Cultivated.Crops")
        ]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Cultivated.Crops")
        ]
    }
    l
    CC.05[k]=quantile(l, c(.05))
    CC.95[k]=quantile(l, c(.95))
}
CC.05
CC.95
DF.05=NULL
DF.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Deciduous.Forest")
        ]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Deciduous.Forest")
        ]
    }
    l
    DF.05[k]=quantile(l, c(.05))
    DF.95[k]=quantile(l, c(.95))
}
DF.05
DF.95
DH.05=NULL
DH.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Developed..High.Intensity")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Developed..High.Intensity")]
    }
    l
    DH.05[k]=quantile(l, c(.05))
    DH.95[k]=quantile(l, c(.95))
}
DH.05
DH.95
DL.05=NULL
DL.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Developed..Low.Intensity")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Developed..Low.Intensity")]
    }
    l
    DL.05[k]=quantile(l, c(.05))
    DL.95[k]=quantile(l, c(.95))
}
DL.05
DL.95
DM.05=NULL
DM.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Developed..Medium.Intensity")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Developed..Medium.Intensity")]
    }
    l
    DM.05[k]=quantile(l, c(.05))
    DM.95[k]=quantile(l, c(.95))
}
DM.05
DM.95
DO.05=NULL
DO.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Developed..Open.Space")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Developed..Open.Space")]
    }
    l
    DO.05[k]=quantile(l, c(.05))
    DO.95[k]=quantile(l, c(.95))
}
DO.05
DO.95
EH.05=NULL
EH.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Emergent.Herbaceous.Wetlands")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Emergent.Herbaceous.Wetlands")]
    }
    l
    EH.05[k]=quantile(l, c(.05))
    EH.95[k]=quantile(l, c(.95))
}
EH.05
EH.95
EF.05=NULL
EF.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Evergreen.Forest")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Evergreen.Forest")]
    }
    l
    EF.05[k]=quantile(l, c(.05))
    EF.95[k]=quantile(l, c(.95))
}
EF.05
EF.95
HB.05=NULL
HB.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Herbaceous")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Herbaceous")]
    }
    l
    HB.05[k]=quantile(l, c(.05))
    HB.95[k]=quantile(l, c(.95))
}
HB.05
HB.95
HP.05=NULL
HP.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Hay.Pasture")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Hay.Pasture")]
    }
    l
    HP.05[k]=quantile(l, c(.05))
    HP.95[k]=quantile(l, c(.95))
}
HP.05
HP.95
MF.05=NULL
MF.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Mixed.Forest")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Mixed.Forest")]
    }
    l
    MF.05[k]=quantile(l, c(.05))
    MF.95[k]=quantile(l, c(.95))
}
MF.05
MF.95
CO.05=NULL
CO.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Open.Water")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Open.Water")]
    }
    l
    CO.05[k]=quantile(l, c(.05))
    CO.95[k]=quantile(l, c(.95))
}
CO.05
CO.95
SS.05=NULL
SS.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Shrub.Scrub")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Shrub.Scrub")]
    }
    l
    SS.05[k]=quantile(l, c(.05))
    SS.95[k]=quantile(l, c(.95))
}
SS.05
SS.95
WW.05=NULL
WW.95=NULL
l=NULL
for(k in 1:(nrow(usa_table))){
    for(n in 1:1000){
        (n.mod[[n]])[k,which(names(usa_table)=="Woody.Wetlands")]
        l[n]=(n.mod[[n]])[k,which(names(usa_table)=="Woody.Wetlands")]
    }
    l
    WW.05[k]=quantile(l, c(.05))
    WW.95[k]=quantile(l, c(.95))
}
WW.05
WW.95
names(usa_table)
menor<-cbind(BL.05,CC.05,DF.05,DH.05,DL.05,DM.05,DO.05,EH.05,EF.05,HP.05,HB.05,MF.05,CO.05,SS.05,WW.05)
mayor<-cbind(BL.95,CC.95,DF.95,DH.95,DL.95,DM.95,DO.95,EH.95,EF.95,HP.95,HB.95,MF.95,CO.95,SS.95,WW.95)
names(usa_table)
colnames(menor)
usa_table-menor
i=NULL
k=13
usa_table[,1]
AS.BL=NULL
BL<-NULL
for(k in 1:nrow(usa_table)){
    for(i in 1:1000){
        AS.BL[i]<-n.mod[[i]][k,1]}
    AS.BL<-as.numeric(AS.BL)
    BL[k]<-row.names(as.data.frame(which(quantile(AS.BL,seq(0,1, length=100))==usa_table[k,1])))[1]
}
#data frame en blanco para las preferencias
preferencias<-data.frame(rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)),rep(NA,nrow(usa_table)))
length(usa_table)
nrow(usa_table)
#BARREN LANDS PREFERENCE
#Para primera especie en primer habitat sería
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][1,1]}
AS.BL<-as.numeric(AS.BL)
#Donde cae de nuestra matriz de observados, en los cuantiles de los modelos nulos?
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[1,1])
#Cuantil 0
row.names(as.data.frame(which(quantile(AS.BL,seq(0,1, length=100))==usa_table[1,1])))
row.names(as.data.frame(which(quantile(AS.BL,seq(0,1, length=100))==usa_table[1,1])))[1]
BL.1<-row.names(as.data.frame(which(quantile(AS.BL,seq(0,1, length=100))==usa_table[1,1])))[1]
preferencias[1,1]<-0
#Para la segunda
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][2,1]}
AS.BL<-as.numeric(AS.BL)
#Donde cae de nuestra matriz de observados, en los cuantiles de los modelos nulos?
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[2,1])
#Cuantil 0
#Es cero igualmente
preferencias[2,1]<-0
#Muchos de Barren Land son 0
#Solo bombus vagans aparece, vamos a verlo
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][15,1]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[15,1])
#Es 1
#por lo que Barren Lands son todo 0 salvo bombus vagans que es 1
BL<-rep(0,nrow(preferencias))
BL[15]<-1
preferencias[,1]<-BL
#Cultivated.Crops
#Todos 0 salvo la sp.13 y 14
CC
usa_table[,2]
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][13,2]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[13,2])
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][14,2]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[14,2])
#Ambos están en el cuantil 1, pinta mal esto
CC<-rep(0,nrow(preferencias))
CC[13]<-1
CC[14]<-1
preferencias[,2]<-CC
#Decidious Forest
#15 to 20
usa_table[,3]
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][15,3]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[15,3])
#Es 1
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][16,3]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[16,3])
#Es 1
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][17,3]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[17,3])
#Es 1
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][18,3]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[18,3])
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][19,3]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[19,3])
for(i in 1:1000){
    AS.BL[i]<-n.mod[[i]][20,3]}
AS.BL<-as.numeric(AS.BL)
which(quantile(AS.BL,seq(0,1, length=100))==usa_table[20,3])
#Todo es 1
quantile(AS.BL,seq(0,1, length=100))
seq(0,1, length=100)
which(quantile(AS.BL,seq(0,1, length=100))==5)
which(quantile(AS.BL,seq(0,1, length=100))==5)
usa_table[1,1]
AAAAAAA<-which(quantile(AS.BL,seq(0,1, length=100))==usa_table[1,1])
row.names(as.data.frame(AAAAAAA))[1]
