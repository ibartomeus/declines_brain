brains.it <- read.csv("~/Desktop/Tesis/R/declines_brain/Raw_data/brains.it.csv")

#List of species with brain weight and IT

brains.it$Encephalization<-brains.it$Brain.Weight..mg./brains.it$IT
brains.t<-subset(brains.it, subset = (is.na(brains.it$Encephalization)==FALSE))
list.of.species<-as.data.frame(unique(brains.t$Species))
spain.bees<-subset(brains.t, subset = (brains.t$Country=="Spain"))
list.of.spanish.bees<-unique(spain.bees$Species)
list.of.spanish.bees<-as.data.frame(list.of.spanish.bees)

getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/Raw_data")
write.csv(list.of.spanish.bees, "list.of.spanish.bees.csv")
write.csv(list.of.species, "list.of.species.csv")
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/declines_brain/")

#Does Queen have bigger brains?------
boxplot(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, ylab="Brain/IT")

aggregate(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex, FUN = mean)
aov.queens<-aov(brains.it$Brain.Weight..mg./brains.it$IT~brains.it$Sex)
TukeyHSD(aov.queens)
