#Libraries Required
library(forecast)
library(ggplot2)
library(dplyr)

#Load data
data  <- read.csv(file.choose())
newdata <- data[,2]
datats <-ts(newdata, frequency=12, start=c(2018))
#Plot
datats %>% plot()

#Decompose
decompose(datats, type = "multiplicative") %>% plot()


#Plot and compare MA models
autoplot(datats, series = "Main Data")+
  autolayer(ma(datats,3), series = "MA-3") +
  autolayer(ma(datats,7), series = "MA-7") +
  ylab("Sales") + xlab("Year") + ggtitle("Sales Data")

#Plot and compare Exp Smoothing fitted values
autoplot(datats, series = "Main Data") +
  autolayer(fitted(ses(datats)), series = "Simple exponential smoothing") +
  autolayer(fitted(holt(datats)), series = "Holt exponential smoothing") +
  autolayer(fitted(hw(datats, seasonal = "multiplicative")), series = "Holt-Winter exponential smoothing") +
  ylab("Sales") + xlab("Year") + ggtitle("Sales Data")

#Plot and compare Exp Smoothing forecasts
autoplot(datats, series = "Main Data") +
  autolayer(ses(datats,12), series = "Simple exponential smoothing") +
  autolayer(holt(datats,12), series = "Holt exponential smoothing") +
  autolayer(hw(datats, seasonal = "multiplicative",12), series = "Holt-Winter exponential smoothing") +
  ylab("Sales") + xlab("Year") + ggtitle("Sales Data")

#Checking accuracy 
model1 <- ses(datats)
model2 <- holt(datats)
model3 <- hw(datats)
model4 <- hw(datats, seasonal = "multiplicative")
accuracy(model1)
accuracy(model2)
accuracy(model3)
accuracy(model4)

#Plot ARIMA models
ts <- datats %>% log() %>% auto.arima(1,1,1)

pred <- forecast(ts, h = 12)

pred <- pred$mean

pred <- exp(pred)
pred
autoplot(datats, series = "Main Data")+
  autolayer(pred, series = "ARIMA forecast") +
  ylab("Sales") + xlab("Year") + ggtitle("Sales Data")
Box.test(ts$residual, type = "Ljung")



##Model performance
tdata <- ts(data = datats, frequency = 12, start = c(2018, 1), end = c(2019,12))

model <- arima(x = log(datats), order = c(1,1,1))

#Prediction
pred <- predict(model, n.ahead = 1*12)

pred <- 2.718^(pred$pred)

##Check result
tail(datats,12)
round(pred,0)



