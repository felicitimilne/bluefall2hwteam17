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


model1323 <- Arima(energy, order = c(1,0,3), seasonal = c(2,1,3))

model1323 %>%
  residuals() %>% ggtsdisplay()

#Ljung-box (check if captured white noise)
index1=seq(1,15)
White.LB <- rep(NA, 15)
for(i in 1:15){
  White.LB[i] <- Box.test(model1323$residuals, lag=i, type="Ljung-Box", fitdf = 9)$p.value
  print(Box.test(model1323$residuals, lag=i, type="Ljung-Box", fitdf = 9)$p.value)
}
white.dat=data.frame(cbind(White.LB[10:15],index1[10:15]))
colnames(white.dat)=c("pvalues","Lag") 

#plot the White Noise of model
ggplot(white.dat,aes(x=factor(Lag),y=pvalues))+geom_col()+labs(title="White Noise of Model 1 ",x="Lags",y="p-values")+coord_cartesian(ylim = c(0,1))

#plot the first model's residual's white noise
train.plot=data.frame(energy)
ggplot(data=train.plot,aes(x=residuals(model1323)))+
  geom_histogram() +
  labs(title = "Distribution of the Residuals of Model 1", x = "Residual", y = "Count of Residuals")

#Check residuals of model 3
checkresiduals(model1323)



#Model 1,0,2,2,1,2

model1222 <- Arima(energy, order = c(1,0,2), seasonal = c(2,1,2))

model1222 %>%
  residuals() %>% ggtsdisplay()

#Ljung-box (check if captured white noise)
index1=seq(1,13)
White.LB <- rep(NA, 13)
for(i in 1:13){
  White.LB[i] <- Box.test(model1222$residuals, lag=i, type="Ljung-Box", fitdf = 7)$p.value
  print(Box.test(model1222$residuals, lag=i, type="Ljung-Box", fitdf = 7)$p.value)
}
white.dat=data.frame(cbind(White.LB[8:13],index1[8:13]))
colnames(white.dat)=c("pvalues","Lag") 

#plot the White Noise of model
ggplot(white.dat,aes(x=factor(Lag),y=pvalues))+geom_col()+labs(title="White Noise of Model 2 ",x="Lags",y="p-values")+coord_cartesian(ylim = c(0,1))

#plot the first model's residual's white noise
train.plot=data.frame(energy)
ggplot(data=train.plot,aes(x=residuals(model1222)))+
  geom_histogram() +
  labs(title = "Distribution of the Residuals of Model 2", x = "Residual", y = "Count of Residuals")

#Check residuals of model 2
checkresiduals(model1222)


#Model 2,0,2,2,1,2

model2222 <- Arima(energy, order = c(2,0,2), seasonal = c(2,1,2))

model2222 %>%
  residuals() %>% ggtsdisplay()

checkresiduals(model2222)



#Model 2,0,3,2,1,3

model2323 <- Arima(energy, order = c(2,0,3), seasonal = c(2,1,3))

model2323 %>%
  residuals() %>% ggtsdisplay()

checkresiduals(model2323)



#Auto arima with seasonal differences
S.ARIMA <- auto.arima(energy, method="ML", seasonal = TRUE, D = 1, stepwise = TRUE)

summary(S.ARIMA)

#Check residuals of the model built by auto arima
energy %>% 
  Arima(order=c(0,0,2), seasonal=c(0,1,2)) %>%
  residuals() %>% ggtsdisplay()

ggAcf(S.ARIMA$residuals)
ggPacf(S.ARIMA$residuals)
