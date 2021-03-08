library(ISLR)
library(rpart)


library(rpart.plot)

mushroom <- read.csv(file.choose())

#data exploration
names(mushroom)
nrow(mushroom)
head(mushroom)

#factorizing the dataset
mushroom <- within(mushroom, {
  class <- factor(class)
  cap.shape <- factor(cap.shape)
  cap.surface<- factor(cap.surface)
  cap.color<- factor(cap.color)
  bruises<- factor(bruises)
  odor<- factor(odor)
  gill.attachment<- factor(gill.attachment)
  gill.spacing<- factor(gill.spacing)
  gill.size<- factor(gill.size)
  gill.color<- factor(gill.color)
  stalk.shape<- factor(stalk.shape)
  stalk.root<- factor(stalk.root)
  stalk.surface.above.ring<- factor(stalk.surface.above.ring)
  stalk.surface.below.ring<- factor(stalk.surface.below.ring)
  stalk.color.above.ring<- factor(stalk.color.above.ring)
  stalk.color.below.ring<- factor(stalk.color.below.ring)
  veil.type<- factor(veil.type)
  veil.color<- factor(veil.color)
  ring.number<- factor(ring.number)  
  ring.type<- factor(ring.type)
  spore.print.color<- factor(  spore.print.color)
  population<- factor(population)
  habitat<- factor(habitat)
  
}
#setting apart testing and training data
set.seed(123)
sam <- sample(nrow(mushroom),nrow(mushroom)*0.70)
train <- mushroom[sam,]
test <- mushroom[-sam,]

#decision tree modelling
fit1 <- rpart(class ~.,method="class",data=train)
rpart.plot(fit1,extra=4)

table(train$class)
#training the model
p <- predict(fit1,type="class")
t <- table("Actual"=train$class,"Predicted"=p)
t
sum(diag(t))/sum(t)
round(100*prop.table(t,1),2)

#testing the model
q <- predict(fit1,newdata=test,type="class")
t1 <- table("Actual"=test$class,"Predicted"=q)
t1
sum(diag(t1))/sum(t1)
round(100*prop.table(t1,1),2)
