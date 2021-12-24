library(lme4)
data1 <- read.csv("data/data_to_play_with.csv")
data1
View(data1)

#Encephalization
boxplot(data1$Brain.IT~data1$f.trend, notch=TRUE,data = data1, ylab= "Brain/IT", xlab="Population trends")
aggregate(Brain.IT~f.trend, FUN = mean, data = data1)


b.aov<-(aov(Brain.IT~f.trend, data = data1))
TukeyHSD(b.aov)

#DECLINING / INCRESING ONLY
incdec.data<-subset(data1, subset = (data1$f.trend == "declining" | data1$f.trend == "increasing"))
droplevels(incdec.data$f.trend)
boxplot(incdec.data$Brain.IT ~ incdec.data$f.trend)


incdec.data$numeric.trend <- NULL
for (n in 1:nrow(incdec.data)) {
    if (incdec.data$f.trend[n] == "declining") {
        incdec.data$numeric.trend[n]= 0
        }else{
    if (incdec.data$f.trend[n] == "increasing") {       
        incdec.data$numeric.trend[n]= 1
        }else{ print("not increasing not declining")}
    }}





plot(numeric.trend ~ Brain.IT, data = incdec.data, main="", xlab="Brain/IT", ylab = "Declining population/ Increasing population", yaxt='n')
xweight <- seq(0, 2, 0.01)
fit <- glm(f.trend ~ Brain.IT, family = binomial, data = incdec.data)
yweight <- predict(fit, list(Brain.IT = xweight), type="response")
lines(xweight, yweight)

trend.brainit<- glm(f.trend ~ Brain.IT, family = binomial, data = incdec.data)
summary(trend.brainit)

#Does mixed models make sense? The species level comes inevitably attached to the trend, so obviously it depends on it
glm.trend.brainit<-glmer(f.trend~Brain.IT + (1|Species), family = binomial, data = incdec.data)
summary(glm.trend.brainit)


glm.trend.brainit<-glm(f.trend~Brain.IT + Species, family = binomial, data = incdec.data)
summary(glm.trend.brainit)


incdec.data$numeric.trend

#Mixed models
c.aov<-(aov(Brain.IT~f.trend + Error(Species), data = brain.it.trends))
summary(c.aov)



#Residuals
boxplot(brain.it.trends$residuals~brain.it.trends$f.trend, notch=TRUE,data = brain.it.trends, ylab= "Brain/IT residuals", xlab="Population trends")
d.aov<-(aov(residuals~f.trend, data = brain.it.trends))
TukeyHSD(d.aov)

e.aov<-(aov(residuals~f.trend + Error(Species), data = brain.it.trends))
summary(e.aov)