###This is an example
library(bipartite)
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



#Calculate percentiles
l=NULL
d=NULL

for(z in 1:ncol(red)){
    
    for(k in 1:nrow(red)){
        
v <- lapply(n.mod, `[[`, k) #generate vector for each row/species

percentil <- sum(unlist(v) <  red[k,z] ) / length(unlist(v)) #calculate percentile

l[k] <- percentil
}
    
    
d <- as.data.frame(cbind(d,l))
    
}



m = red #create matrix from data and overwrite in the for loop

for(z in 1:ncol(red)){

for(k in 1:nrow(red)){
    
v <- lapply(n.mod, `[[`, k) #generate vector for each row/species
    
red2[k,z] <- sum(unlist(v) <  red[k,z] ) / length(unlist(v)) #calculate percentile and stroe in position
    
}}


