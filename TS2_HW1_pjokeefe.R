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

#get rid of useless variables
df <- df[,c(1,6)]

#Change variable to a date time object
df$datetime_beginning_ept <- mdy_hm(df$datetime_beginning_ept, tz = Sys.timezone())

#Impute the average of previous and next observation to fix the zeros for DLS
df[c(5280:5290),]
df[5283,2] <- 904.2965

df[c(14180:14190),]
df[14187,2] <- 844.047

# create time series object
energy <- ts(df[,2], start = 2019, frequency = 24) # frequency = 24 hours * 365.25 days in a year

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

#Create the holt winter's model
HW <- hw(energy, seasonal = "additive", h = 168)
summary(HW)

#Plot the forecasts from the holt-winters model
autoplot(HW)+
  autolayer(fitted(HW),series="Fitted")+ylab("US Steel Shipments")

#nsdiffs to check for stochastic or deterministic seasonality
energy %>% nsdiffs()

#ndiffs to check for stationarity
energy %>% diff(lag = 24) %>% ndiffs()

energy %>% diff(lag = 24) %>% ggtsdisplay()

# Seasonal ARIMA with seasonal differences
#testing AR and MA terms
energy %>% 
  Arima(order=c(1,0,3), seasonal=c(2,1,3)) %>%
  residuals() %>% ggtsdisplay()

#Auto arima with seasonal differences
S.ARIMA <- auto.arima(energy, method="ML", seasonal = TRUE, D = 1, stepwise = TRUE)

summary(S.ARIMA)

#Check residuals of the model built by auto arima
energy %>% 
  Arima(order=c(0,0,2), seasonal=c(0,1,2)) %>%
  residuals() %>% ggtsdisplay()

ggAcf(S.ARIMA$residuals)
ggPacf(S.ARIMA$residuals)
