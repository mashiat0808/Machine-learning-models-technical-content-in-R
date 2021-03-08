library(ggplot2)
library(tidyverse)

library(cluster)    
library(factoextra)
#Load Dataset
customerdata <- read.csv(file.choose())

  
head(customerdata)
customerdata$Gender <- as.factor(customerdata$Gender)
customerdata$Gender <- ifelse(customerdata$Gender=="Male",1,2)

df<- customerdata[,c(2,3,4,5)]

#Elbow plot
wss <- 0
for (i in 1:10){ 
  wss[i] <- kmeans(df,centers=i, nstart = 25)$tot.withinss
}
wss
plot(1:10, wss, type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares",col="red",pch=16,lwd=3)

#create model
k <- kmeans(df,centers=6, nstart = 25)
table(k$cluster) 

plot(df$Age,df$Annual.Income..k..,col=k$cluster,pch=19,
     xlab="Annual Income",ylab="Age",main="By cluster")
k$centers
table(k$cluster)
