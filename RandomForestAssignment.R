library(randomForest)

#opening the dataset
heartdisease <- read.csv(file.choose())

'
The mapping of columns and features of heartdisease dataset:
age: age in years
sex: sex (1 = male; 0 =female)
cp: chest pain type (1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 4 = asymptomatic)
trestbps: resting blood pressure (in mm Hg on admission to the hopsital)
choi: serum cholestoral in mg/dl
fbs: fasting blood sugar > 120 mg/dl (1 = true; = 0 false)
restecg: resting electrocardiographic results (1 = normal; 2 = having ST-T wave abnormality; 2 = showing probable or definite left ventricular hypertrophy)
thalach: maximum heart rate achieved
exang: exercise induced angina (1 = yes; 0 = no)
oldpeak: ST depression induced by exercise relative to rest
slope: the slope of the peak exercise ST segment (1 = upsloping; 2 = flat; 3 = downsloping)
ca: number of major vessels (0-3) colored by flourosopy
thai: (3 = normal; 6 = fixed defect; 7 = reversable defect)
num: diagnosis of heart disease. It is an integer valued from 0 (no presence) to 4.
'

#viewing the dataset
heartdisease

#factorizing num
heartdisease$num <- as.factor(heartdisease$num)
levels(heartdisease$num)

## data consists of 303 cases
##illustrative box plots of features stratified by diagonosis of heart disease

par(mfrow=c(3,3), mai=c(.3,.6,.1,.1))
plot(age ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(sex ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(cp ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(trestbps ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(choi ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(fbs ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(restecg ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(thalach ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(exang ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(oldpeak ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(slope ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(ca ~ num, data=heartdisease, col=c(grey(.2),2:6))
plot(thai ~ num, data=heartdisease, col=c(grey(.2),2:6))


table(heartdisease$num)
#splitting training and testing data
set.seed(1234)
sam <- sample(nrow(heartdisease),floor(nrow(heartdisease)*0.85))
train <- heartdisease[sam,]
test <- heartdisease[-sam,]

#fitting random forest
fit <- randomForest(num ~ ., data = train,ntree=501 ,mtry = 3, importance=TRUE)
fit

#analyzing and viewing the fit
importance(fit)
varImpPlot(fit)


#accuracy analysis
p <- predict(fit,train)
t1 <- table("Actual diagnosis"=train$num,"Predicted type"=p)
100*prop.table(t1,1)

q <- predict(fit,newdata=test)
t2 <- table("Actual diagnosis"=test$num,"Predicted type"=q)
round(100*prop.table(t2,1),1)

#accuracy percentage
pcorr=100*sum(test$num==q)/(nrow(test))
pcorr

##Accuracy is 76.08%