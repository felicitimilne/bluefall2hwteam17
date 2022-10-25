# code for time series 2 homework 1 

# libraries
library(dplyr)
library(ggfortify)
library(lubridate)
library(tseries)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(ggplot2)
library(seasonalview)
library(aTSA)
library(imputeTS)
library(prophet)

#load data
df0 <- read.csv("https://raw.githubusercontent.com/felicitimilne/bluefall2hwteam17/main/hrl_load_metered.csv")
head(df0)
#Impute the average of previous and next observation to fix the zeros for DLS
df0[c(5280:5290),]
df0[5283,6] <- 904.2965
df0[c(14180:14190),]
df0[14187,6] <- 844.047

df1 <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test1.csv")
head(df1)

df2<- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test2.csv")
head(df2)

train <- rbind(df0,df1,df2)


validation <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test3.csv")

test <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test4.csv")


#get rid of useless variables
train <- train[,c(1,6)]

validation <- validation[,c(1,6)]

test <- test[,c(1,6)]

#Change variable to a date time object

train$datetime_beginning_ept <- mdy_hm(train$datetime_beginning_ept, tz = Sys.timezone())

validation$datetime_beginning_ept <- mdy_hm(validation$datetime_beginning_ept, tz = Sys.timezone())

test$datetime_beginning_ept <- mdy_hm(test$datetime_beginning_ept, tz = Sys.timezone())

#retraining data
new_val <- rbind(train, validation)
new_val$datetime_beginning_ept <- mdy_hm(new_val$datetime_beginning_ept, tz = Sys.timezone())

# create time series object with the new training/validation data
energy.train <- ts(train[,2], start = 2019, frequency = 24) 
energy.val <-ts(new_val[,2], start = 2019, frequency = 24)
energy.test <- ts(test[,2], start = 2019, frequency = 24)

# autoplot
autoplot(energy.train) +
  ggtitle("Energy Usage") +
  xlab("Time") +
  ylab("Energy")

#decomposition plot
decomp_stl <- stl(energy.train, s.window = 7)
plot(decomp_stl)
autoplot(decomp_stl)

#subseries plot that plots the averages of the seasons
ggsubseriesplot(energy.train)


##################################################################

# ESM

#Create the holt winter's model
HW <- hw(energy.train, seasonal = "multiplicative", h = 168)
#summary(HW)


# Calculate prediction errors from forecast
error=validation$mw-HW$mean

# Calculate prediction error statistics (MAE and MAPE)
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(validation$mw))*100

MAE
MAPE

#test
HW.test <- hw(energy.val, seasonal = "multiplicative", h = 168)

# Calculate prediction errors from forecast
error.test=test$mw-HW.test$mean

# Calculate prediction error statistics (MAE and MAPE)
MAE.test=mean(abs(error.test))
MAPE.test=mean(abs(error.test)/abs(test$mw))*100

MAE.test
MAPE.test

##################################################################

# ARIMA
#Model 1,0,2,2,1,2

model1222 <- Arima(energy.train, order = c(1,0,2), seasonal = c(2,1,2))

#Test against validation set
#forecast compared to validation for auto model
model1222forecast <- forecast::forecast(model1222, h = 168)

#calculate the error for the model
model1222forecast.error = validation$mw - model1222forecast$mean

#calculate the MAE and MAPE for the model
model1222forecast.MAE = mean(abs(model1222forecast.error))
model1222forecast.MAE

model1222forecast.MAPE=mean(abs(model1222forecast.error)/abs(validation$mw))*100
model1222forecast.MAPE

#retrain hand-picked ARIMA model 1 -> chosen ARIMA to test
model1222_test <- Arima(energy.val, order = c(1,0,2), seasonal = c(2,1,2))
#forecast
model1222forecast_test <- forecast::forecast(model1222_test, h = 168)

#calculate the MAE and MAPE for the test set
model1222forecast_test.error = test$mw - model1222forecast_test$mean

model1222forecast_test.MAE = mean(abs(model1222forecast_test.error))
model1222forecast_test.MAE

model1222forecast_test.MAPE = mean(abs(model1222forecast_test.error)/test$mw)*100
model1222forecast_test.MAPE

##################################################################

# Prophet

prophet.data <- data.frame(ds = train$datetime_beginning_ept, y = train$mw)

Prof <- prophet()
Prof <- add_country_holidays(Prof, "US")
Prof <- add_seasonality(Prof, name='monthly', period=30.5, fourier.order=6)
Prof <- fit.prophet(Prof, prophet.data)

Prof


forecast.data <- make_future_dataframe(Prof, periods = 168, freq = 'hour')


#plot(Prof, predict(Prof, forecast.data))

# Calculate prediction errors from forecast

Prophet.error <- validation$mw - tail(predict(Prof, forecast.data)$yhat, 168)

# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(validation$mw))*100

Prophet.MAE
Prophet.MAPE


##Test

prophet.data.test <- data.frame(ds = new_val$datetime_beginning_ept, y = new_val$mw)

Prof.test <- prophet()
Prof.test <- add_country_holidays(Prof.test, "US")
Prof.test <- add_seasonality(Prof.test, name='monthly', period=30.5, fourier.order=6)
Prof.test <- fit.prophet(Prof.test, prophet.data.test)

Prof.test

forecast.data.test <- make_future_dataframe(Prof.test, periods = 168, freq = 'hour')


# Calculate prediction errors from forecast

Prophet.error.test <- test$mw - tail(predict(Prof.test, forecast.data.test)$yhat, 168)

# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE.test <- mean(abs(Prophet.error))
Prophet.MAPE.test <- mean(abs(Prophet.error)/abs(test$mw))*100

Prophet.MAE.test
Prophet.MAPE.test

######################################################################

#Neural Network

set.seed(476)
NN.Model <- nnetar(diff(energy, 24), p = 1, P = 2)


checkresiduals(NN.Model)

NN.Forecast <- forecast::forecast(NN.Model, h = 168)


Pass.Forecast <- rep(NA, 168)

for(i in 1:24){
  Pass.Forecast[i] <- energy[length(energy) - 24 + i] + NN.Forecast$mean[i]
}

for(i in 25:168){
  Pass.Forecast[i] <- Pass.Forecast[length(Pass.Forecast) - 192 + i] + NN.Forecast$mean[i]
}

Pass.Forecast <- ts(Pass.Forecast, start = 2022, frequency = 24)

validation_ts <- ts(validation[,2], start = 2022, frequency = 24)


# Calculate prediction errors from forecast
NN.error <- validation_ts - Pass.Forecast

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE <- mean(abs(NN.error))
NN.MAPE <- mean(abs(NN.error)/abs(validation_ts))*100

NN.MAE

NN.MAPE


### Test
NN.Model.test <- nnetar(diff(energy.test, 24), p = 1, P = 2)

NN.Forecast.test <- forecast::forecast(NN.Model.test, h = 168)


Pass.Forecast.test <- rep(NA, 168)

for(i in 1:24){
  Pass.Forecast.test[i] <- energy.test[length(energy.test) - 24 + i] + NN.Forecast.test$mean[i]
}

for(i in 25:168){
  Pass.Forecast.test[i] <- Pass.Forecast.test[length(Pass.Forecast.test) - 192 + i] + NN.Forecast.test$mean[i]
}


Pass.Forecast.test <- ts(Pass.Forecast.test, start = 2022, frequency = 24)

test_ts <- ts(test[,2], start = 2022, frequency = 24)


# Calculate prediction errors from forecast
NN.error.test <- test_ts - Pass.Forecast.test

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE.test <- mean(abs(NN.error.test))
NN.MAPE.test <- mean(abs(NN.error.test)/abs(test_ts))*100

NN.MAE.test

NN.MAPE.test

