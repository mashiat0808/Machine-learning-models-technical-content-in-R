
dpen <- read.csv(file.choose())

#summarizing data
dim(dpen)
summary(dpen)
plot(dpen)

#Looking to find N/A values or data discrepencies
library(skimr)
missing.oberservation <- sum(is.na(dpen))
missing.oberservation
skim(dpen)

#checking for the entire model
m1=glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,family=binomial,data=dpen)
m1

summary(m1)
library(faraway)
#using step function to find the best AIC and Residual Deviatace
step(m1, trace = 1, keep = NULL, steps = 1000)


#Making the optimal model from VIF score as well
m3=glm(formula = Outcome ~ Pregnancies + Glucose +  
         BMI + DiabetesPedigreeFunction , family = binomial, 
       data = dpen)
m3
vif(m3)

## calculating logits
exp(m3$coef)


#dividing testing and training data
library(tidymodels)
x =initial_split(dpen, prop = 3/4, strata = dpen$Outcome)
train= training(x)
test=testing(x)





train.predicted <- predict(m3, train, type="response")  # predicted scores

test.predicted <- predict(m3, test, type="response")  # predicted scores

#finding out optimal cutoff
library(InformationValue)
optCutOff <- optimalCutoff(train$Outcome, train.predicted)


misClassError(test$Outcome, test.predicted, threshold = optCutOff)

#plotting ROC
plotROC(train$Outcome, train.predicted) #train data

plotROC(test$Outcome, test.predicted) #test data


#Creating confusion matrix
Concordance(test$Outcome, predicted)

sensitivity(test$Outcome, predicted, threshold = optCutOff)

specificity(test$Outcome, predicted, threshold = optCutOff)

x=confusionMatrix(test$Outcome, predicted, threshold = optCutOff)

prop.table(confusionMatrix(test$Outcome, predicted, threshold = optCutOff))*100
x

#checking accuracy
accuracy= ((x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1]))*100
accuracy



