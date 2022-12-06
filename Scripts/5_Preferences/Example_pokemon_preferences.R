###This is an example

#First we create an example table, it must be an interaction matrix crossing species an habitats

#We create some fictional habitats with abundance
Meadow <- c(3,89,17,12,50,0,47,38)
Garden <- c(11,77,20,3,53,2,63,30)
Cave <- c(89,1,18,97,70,50,72,50)

#And add some fictional species (it was easier for me to think about pokemon than real animals, sorry)
red <- data.frame(Meadow,Garden,Cave, row.names = c("Zubat","Caterpie","Clefairy","Geodude","Rattata","Haunter","Pidgey","Miltank"))

#This is our interaction matrix, it's important that our matrix has the same sampling effort
#for every habitat, if we don't have it, we must correct the data to simulate this
#Dataframe need to have this format, I may have code to format average dataframe to this form, but I have to look for it
red

#Now we create some null models from our matrix, using the nullmodel function, and 
#using method = "rd2table" that generates random 2-way tables maintaining the sum of rows 
#and columns using Patefield's algorithm so the proportional abundance of species and habitats
#is maintained, note this for the Methods section of the paper.

n.mod <- nullmodel(red, N=1000, method="r2dtable")

#And now we need to extract the quantile .95 and .05 from that nullmodels and to stablish
#threshold for preference/avoidance

#We need to do it for every habitat, first, Meadows
Meadow.05=NULL
Meadow.95=NULL
l=NULL

for(k in 1:(nrow(red))){
    for(n in 1:1000){
        (n.mod[[n]])[k,1]
        l[n]=(n.mod[[n]])[k,1]
    }
    l
    Meadow.05[k]=quantile(l, c(.05))
    Meadow.95[k]=quantile(l, c(.95))
}
#Second, gardens
Garden.05=NULL
Garden.95=NULL
l=NULL

for(k in 1:(nrow(red))){
    for(n in 1:1000){
        (n.mod[[n]])[k,2]
        l[n]=(n.mod[[n]])[k,2]
    }
    l
    Garden.05[k]=quantile(l, c(.05))
    Garden.95[k]=quantile(l, c(.95))
}
#Third, caves
Caves.05=NULL
Caves.95=NULL
l=NULL

for(k in 1:(nrow(red))){
    for(n in 1:1000){
        (n.mod[[n]])[k,3]
        l[n]=(n.mod[[n]])[k,3]
    }
    l
    Caves.05[k]=quantile(l, c(.05))
    Caves.95[k]=quantile(l, c(.95))
}

#Now we create preference and avoidance matrix
preference.red<-cbind(Meadow.95,Garden.95,Caves.95)
row.names(preference.red)<-row.names(red)
preference.red

avoidance.red<-cbind(Meadow.05,Garden.05,Caves.05)
row.names(avoidance.red)<-row.names(red)
avoidance.red
#And compare with our data

#Here, TRUE means preference for certain habitat
#Abundance higher than expected means preference
red>preference.red

#And here, TRUE means avoidance for that habitat
#And abundance lower than expected means avoidance
red<avoidance.red

#The rest of the species, would have indifference

#We can see the quantile distribution for a species in an habitat, for example we pick zubats in caves
#We extract the data from the null models
cavezubatdistribution<-NULL
for (n in 1:1000) {
    cavezubatdistribution[n]<-(n.mod[[n]])[1,3]
    
}

yzubcave <- dnorm(cavezubatdistribution, mean = mean(cavezubatdistribution), sd = sd(cavezubatdistribution))
# Give the chart file a name.
plot(cavezubatdistribution,yzubcave, main = "Distribución de preferencias para Zubat en Cuevas", xlab = "Ocurrencias" , ylab = "Probabilidad")
abline(v=quantile(cavezubatdistribution, probs = 0.05), col="grey")
abline(v=quantile(cavezubatdistribution, probs = 0.95), col="grey")
red[1,3]
quantile(cavezubatdistribution)


#We can also ask, where our ocurrence number for a species within an habitat falls in the quantile distribution
#We use for example, miltank in gardens
miltankgardendistribution<-NULL
for (n in 1:1000) {
    miltankgardendistribution[n]<-(n.mod[[n]])[8,2]
    
}

ymilgard <- dnorm(miltankgardendistribution, mean = mean(miltankgardendistribution), sd = sd(miltankgardendistribution))
plot(miltankgardendistribution,ymilgard, main = "Distribución de preferencias para Miltank en Jardines", xlab = "Ocurrencias" , ylab = "Probabilidad")
abline(v=quantile(miltankgardendistribution, probs = 0.05), col="grey")
abline(v=quantile(miltankgardendistribution, probs = 0.95), col="grey")
red[8,2]
quantile(miltankgardendistribution, probs = seq(0,1,0.01))
which(quantile(miltankgardendistribution, probs = seq(0,1,0.01))==red[8,2])
#Our miltank has no garden preference, because it falls around 33%-41% within the quantile distribution
