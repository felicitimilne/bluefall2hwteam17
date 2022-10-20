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
df <- read.csv("https://raw.githubusercontent.com/felicitimilne/bluefall2hwteam17/main/hrl_load_metered.csv")
head(df)

df2 <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test1.csv")
head(df2)


df3 <- rbind(df,df2)

validation <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test2.csv")

#get rid of useless variables
df3 <- df[,c(1,6)]

validation <- validation[,c(1,6)]

#Change variable to a date time object
df3$datetime_beginning_ept <- mdy_hm(df3$datetime_beginning_ept, tz = Sys.timezone())

validation$datetime_beginning_ept <- mdy_hm(validation$datetime_beginning_ept, tz = Sys.timezone())

#Impute the average of previous and next observation to fix the zeros for DLS
df3[c(5280:5290),]
df3[5283,2] <- 904.2965

df3[c(14180:14190),]
df3[14187,2] <- 844.047

# create time series object
energy <- ts(df3[,2], start = 2019, frequency = 24) # frequency = 24 hours * 365.25 days in a year

validation <- ts(validation[,2], start = 2021, frequency = 24)

# autoplot
autoplot(energy) +
  ggtitle("Energy Usage") +
  xlab("Time") +
  ylab("Energy")

#decomposition plot
decomp_stl <- stl(energy, s.window = 7)
plot(decomp_stl)
autoplot(decomp_stl)

#subseries plot that plots the averages of the seasons
ggsubseriesplot(energy)

# Prophet

prophet.data <- data.frame(ds = df3$datetime_beginning_ept, y = df3$mw)

Prof <- prophet()
Prof <- add_country_holidays(Prof, "US")
Prof <- add_seasonality(Prof, name='monthly', period=30.5, fourier.order=6)
Prof <- fit.prophet(Prof, prophet.data)

prof

checkresiduals(Prof)

forecast.data <- make_future_dataframe(Prof, periods = 168, freq = 'hour')


#plot(Prof, predict(Prof, forecast.data))

# Calculate prediction errors from forecast

Prophet.error <- validation - tail(predict(Prof, forecast.data)$yhat, 168)

# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(validation))*100

Prophet.MAE
Prophet.MAPE

######################################################################

#Neural Network

set.seed(476)
NN.Model <- nnetar(diff(energy, 24), p = 1, P = 2)


checkresiduals(NN.Model)

NN.Forecast <- forecast::forecast(NN.Model, h = 168)
plot(NN.Forecast)




Pass.Forecast <- rep(NA, 168)

for(i in 1:168){
  Pass.Forecast[i] <- energy[length(energy) - 168 + i] + NN.Forecast$mean[i]
}

Pass.Forecast <- ts(Pass.Forecast, start = 2021, frequency = 24)


# Calculate prediction errors from forecast
NN.error <- validation - Pass.Forecast

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE <- mean(abs(NN.error))
NN.MAPE <- mean(abs(NN.error)/abs(validation))*100

NN.MAE

NN.MAPE