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

df <- df[,c(1,6)]

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


decomp_stl <- stl(energy, s.window = 7)
plot(decomp_stl)
autoplot(decomp_stl)

ggsubseriesplot(energy)

HW <- hw(energy, seasonal = "additive", h = 168)
summary(HW)

autoplot(HW)+
  autolayer(fitted(HW),series="Fitted")+ylab("US Steel Shipments")


