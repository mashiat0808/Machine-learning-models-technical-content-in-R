cars <- read.csv("C:/Users/MD/Downloads/CarPrice_Assignment.csv")

dim(cars)
summary(cars)
str(cars)
#cleaning the dataset and getting rid of NA values
missing.observation <- sum(is.na(cars))
missing.observation

clean.car <- na.omit(data.frame(cars[,-c(6,10,17,35,37,38,179,194,202)]))
clean.car

str(clean.car)
#complete visualization of the dataset
clean.car <- within(clean.car, {
  fueltype <- factor(fueltype)
  carbody <- factor(carbody)
})

plot(clean.car)
#Detection of multicolinearity
library(faraway)
x<- lm(price~.,clean.car)
summary(x)
n<-vif(x)
ifelse(n>10,1,0)
#since there are few values exceeding 10 for VIF values, we can conclude that multicolinearity exists
#the model is not eligible for linear regression

#removing the insignificant and multicolinear variables from the dataset
newcars <- subset(clean.car, , select=c(price, horsepower,carwidth
))
linearmodel<- lm(price~horsepower+carwidth, newcars)

summary(linearmodel)
vif(linearmodel)

#the model is free of colinearity

cor(clean.car$price,clean.car$horsepower)
cor(newcars$price,newcars$horsepower)
cor(clean.car$price,clean.car$carwidth)
cor(newcars$price,newcars$carwidth)

#carwidth and horsepower  are significant in predicting the price of car


#testing model

smp_size <- floor(.7*nrow(clean.car))
set.seed(1000)
train_ind <- sample(seq_len(nrow(clean.car)),size=smp_size)

train <- clean.car[train_ind,]
test <- clean.car[-train_ind,]

model <- lm(price~ carwidth+horsepower, data=train)
priceprediction <- predict(model, test)

head(priceprediction)
summary(model)

prediction <- data.frame(cbind(predict=test$price),predicted=(priceprediction)) 
head(prediction,increasing=True)
accuracy <- mean(apply(prediction,1,min)/ apply(prediction,1,max))
accuracy


