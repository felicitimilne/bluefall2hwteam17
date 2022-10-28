# code for time series 2 homework 1 

# libraries
library(tidyverse)
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
df3 <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test2.csv")

train <- data.frame(rbind(df,df2, df3))

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


#Impute the average of previous and next observation to fix the zeros for DLS
train[c(5280:5290),]
train[5283,2] <- 904.2965

train[c(14180:14190),]
train[14187,2] <- 844.047

retrain <- data.frame(rbind(train, validation))

#retrain$datetime_beginning_ept <- mdy_hm(retrain$datetime_beginning_ept, tz = Sys.timezone())


# create time series object
energy <- ts(train[,2], start = 2019, frequency = 24) # frequency = 24 hours * 365.25 days in a year

energy.test <- ts(retrain[,2], start = 2019, frequency = 24)

tseq <- seq.POSIXt(from = train$datetime_beginning_ept[1], length.out = nrow(train), by = "hours")
tseq_val_week <- seq.POSIXt(from = as.POSIXct("2022-10-07 00:00:00", tz = "EST"), length.out = 168, by = "hours")
tseq_with_forecast <- c(tseq, tseq_val_week)
#datetime vector for only 2022 + validation times
begin_2022_time <- as.POSIXct("2022-01-01 00:00:00", tz = "EST")
begin_2022 <- match(begin_2022_time, train$datetime_beginning_ept)
tseq_2022 <- seq.POSIXt(from = begin_2022_time, length.out = nrow(train) - begin_2022 + 170, by = "hours")
dst_2022_time1 <- as.POSIXct("2022-03-13 01:00:00", tz = "EST")
dst_2022_time2 <- as.POSIXct("2022-03-13 03:00:00", tz = "EST")
dst_2022_1 <- match(dst_2022_time1, tseq_2022)
dst_2022_2 <- match(dst_2022_time2, tseq_2022)
tseq_2022 <- tseq_2022[c(seq(1, dst_2022_1), seq(dst_2022_2, length(tseq_2022)))]
#datetime vector from 8/1/22 + validation times, same order for each code block
begin_aug_2022_time <- as.POSIXct("2022-08-01 00:00:00", tz = "EST")
begin_aug_2022 <- match(begin_aug_2022_time, train$datetime_beginning_ept)
tseq_2022_aug <- seq.POSIXt(from = begin_aug_2022_time, length.out = nrow(train) - begin_aug_2022 + 169, by = "hours")

#make validation only datetime vector
tseq_val_week_all <- c(tseq_val_week, tseq_val_week, tseq_val_week)
tseq_val_week_all_wt <- c(tseq_val_week, tseq_val_week, tseq_val_week, tseq_val_week, tseq_val_week)
tseq_val_week_all_final <- c(tseq_val_week, tseq_val_week, tseq_val_week, tseq_val_week, tseq_val_week, tseq_val_week)

tseq_val_week <- c(tseq_val_week, tseq_val_week)
tseq_val_week <- sort(tseq_val_week)

tseq_2022_aug_test <- seq.POSIXt(from = as.POSIXct("2022-08-01 00:00:00", tz = "EST"), length.out = nrow(train) - begin_aug_2022 + 337, by = "hours")
tseq_test_week <- seq.POSIXt(from = as.POSIXct("2022-10-14 00:00:00", tz = "EST"), length.out = 168, by = "hours")
tseq_test_week_all <- c(tseq_test_week, tseq_test_week, tseq_test_week)
tseq_test_week_all_wt <- c(tseq_test_week, tseq_test_week, tseq_test_week, tseq_test_week, tseq_test_week)
tseq_test_week_all_final <- c(tseq_test_week, tseq_test_week, tseq_test_week, tseq_test_week, tseq_test_week, tseq_test_week)
tseq_test_week <- c(tseq_test_week, tseq_test_week)
tseq_test_week <- sort(tseq_test_week)

#create dummy df for ggplot
whole_dummy_df <- data.frame(cbind(seq(1:length(tseq_with_forecast)), c(rep(FALSE, (length(tseq))), rep(TRUE, 168))))
dummy_df <- data.frame(cbind(seq(1:length(tseq_2022)), c(rep(FALSE, (length(tseq_2022) - 168)), rep(TRUE, 168))))
dummy_df_aug <- data.frame(cbind(seq(1:length(tseq_2022_aug)), c(rep(FALSE, (length(tseq_2022_aug) - 168)), rep(TRUE, 168))))
dummy_df_aug_test <- data.frame(cbind(seq(1:length(tseq_2022_aug_test)), c(rep(FALSE, (length(tseq_2022_aug_test) - 168)), rep(TRUE, 168))))

colnames(whole_dummy_df) <- c("index", "forecast")
colnames(dummy_df) <- c("index", "forecast")
colnames(dummy_df_aug) <- c("index", "forecast")
colnames(dummy_df_aug_test) <- c("index", "forecast")


##################################################################

# Prophet

prophet.data <- data.frame(ds = train$datetime_beginning_ept, y = train$mw)

Prof <- prophet()
Prof <- add_country_holidays(Prof, "US")
Prof <- add_seasonality(Prof, name='monthly', period=30.5, fourier.order=6)
Prof <- fit.prophet(Prof, prophet.data)

Prof

forecast.data <- make_future_dataframe(Prof, periods = 168, freq = 'hour')

proph_pred <- tail(predict(Prof, forecast.data)$yhat, 168)
proph_model_df <- data.frame(cbind(validation$mw, proph_pred))
colnames(proph_model_df) <- c("actual", "predicted_proph")
proph_model_df <- proph_model_df %>% pivot_longer(c(actual, predicted_proph))

whole_mw_with_forecast_proph <- c(train$mw, proph_pred)
mw_with_forecast_proph <- c(train$mw[begin_2022:nrow(train)], proph_pred)
aug_mw_with_forecast_proph <- c(train$mw[begin_aug_2022:nrow(train)], proph_pred)

ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_proph, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Prophet Model Forecast for 10/7/22-10/13/22", color = "Data")

ggplot(data = proph_model_df, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs Prophet Forecast for 10/7/22-10/13/22", color = "Data")


#plot(Prof, predict(Prof, forecast.data))

# Calculate prediction errors from forecast

Prophet.error <- validation$mw - proph_pred

# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(validation$mw))*100

Prophet.MAE
Prophet.MAPE

##Test

prophet.data.test <- data.frame(ds = retrain$datetime_beginning_ept, y = retrain$mw)

Prof.test <- prophet()
Prof.test <- add_country_holidays(Prof.test, "US")
Prof.test <- add_seasonality(Prof.test, name='monthly', period=30.5, fourier.order=6)
Prof.test <- fit.prophet(Prof.test, prophet.data.test)

Prof.test

forecast.data.test <- make_future_dataframe(Prof.test, periods = 168, freq = 'hour')

proph_pred_test <- tail(predict(Prof.test, forecast.data.test)$yhat, 168)

proph_model_test_df <- data.frame(cbind(test$mw, proph_pred_test))
colnames(proph_model_test_df) <- c("actual", "predicted_proph")
proph_model_test_df <- proph_model_test_df %>% pivot_longer(c(actual, predicted_proph))

aug_test_mw_with_forecast_proph <- c(retrain$mw[begin_aug_2022:nrow(retrain)], proph_pred_test)

ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_test_mw_with_forecast_proph, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) +
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Prophet Model Test Forecast for 10/14/22-10/20/22", color = "Data")

ggplot(data = proph_model_test_df, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs Prophet Model Test Forecast for 10/14/22-10/20/22", color = "Data")

# Calculate prediction errors from forecast

Prophet.error.test <- test$mw - proph_pred_test

# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE.test <- mean(abs(Prophet.error.test))
Prophet.MAPE.test <- mean(abs(Prophet.error.test)/abs(test$mw))*100

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

nn_pred <- Pass.Forecast
nn_model_df <- data.frame(cbind(validation$mw, nn_pred))
colnames(nn_model_df) <- c("actual", "predicted_nn")
nn_model_df <- nn_model_df %>% pivot_longer(c(actual, predicted_nn))

aug_mw_with_forecast_nn <- c(train$mw[begin_aug_2022:nrow(train)], nn_pred)
whole_mw_with_forecast_nn <- c(train$mw, nn_pred)
mw_with_forecast_nn <- c(train$mw[begin_2022:nrow(train)], nn_pred)

ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_nn, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Neural Network Model Forecast for 10/7/22-10/13/22", color = "Data")

ggplot(data = nn_model_df, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs NN Model Forecast for 10/7/22-10/13/22", color = "Data")

#Pass.Forecast <- ts(Pass.Forecast, start = 2022, frequency = 24)

#validation_ts <- ts(validation[,2], start = 2022, frequency = 24)


# Calculate prediction errors from forecast
NN.error <- validation$mw - nn_pred

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE <- mean(abs(NN.error))
NN.MAPE <- mean(abs(NN.error)/abs(validation$mw))*100

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


#Pass.Forecast.test <- ts(Pass.Forecast.test, start = 2022, frequency = 24)

#test_ts <- ts(test[,2], start = 2022, frequency = 24)]

nn_pred_test <- Pass.Forecast.test

aug_test_mw_with_forecast_nn <- c(retrain$mw[begin_aug_2022:nrow(retrain)], nn_pred_test)

nn_model_test_df <- data.frame(cbind(test$mw, nn_pred_test))
colnames(nn_model_test_df) <- c("actual", "predicted_nn")
nn_model_test_df <- nn_model_test_df %>% pivot_longer(c(actual, predicted_nn))


ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_test_mw_with_forecast_nn, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Neural Network Model Test Forecast for 10/14/22-10/20/22", color = "Data")

ggplot(data = nn_model_test_df, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs NN Model Test Forecast for 10/14/22-10/20/22", color = "Data")

#combined plots

#validation
nn_model_df_temp <- nn_model_df %>% mutate(count = sort(rep(seq(1, 168), 2)))
proph_df_only <-  proph_model_df %>% filter(name == "predicted_proph") %>% mutate(count = seq(1, 168))
model_df <- data.frame(rbind(nn_model_df_temp, proph_df_only)) %>% arrange(name)

ggplot(data = model_df, aes(x = tseq_val_week_all, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000", "#dc267f"), labels = c("Actual", "Predicted NN", "Predicted Proph")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs Prophet Model vs NN Model Forecast for 10/7/22-10/13/22", color = "Data")

#test
nn_model_test_df_temp <- nn_model_test_df %>% mutate(count = sort(rep(seq(1, 168), 2)))
proph_test_df_only <-  proph_model_test_df %>% filter(name == "predicted_proph") %>% mutate(count = seq(1, 168))
model_test_df <- data.frame(rbind(nn_model_test_df_temp, proph_test_df_only)) %>% arrange(name)

ggplot(data = model_test_df, aes(x = tseq_test_week_all, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000", "#dc267f"), labels = c("Actual", "Predicted NN", "Predicted Proph")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs Prophet Model vs NN Model Test Forecast for 10/14/22-10/20/22", color = "Data")


# Calculate prediction errors from forecast
NN.error.test <- test$mw - nn_pred_test

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE.test <- mean(abs(NN.error.test))
NN.MAPE.test <- mean(abs(NN.error.test)/abs(test$mw))*100

NN.MAE.test

NN.MAPE.test

HW_mult <- hw(energy, seasonal = "multiplicative", h = 168)

mult_model_df <- data.frame(cbind(validation$mw, HW_mult$mean))
colnames(mult_model_df) <- c("actual", "predicted_hw")
mult_model_df <- mult_model_df %>% pivot_longer(c(actual, predicted_hw))

#append forecasted values to original data, only going to graph from 8/1/2022
aug_mw_with_forecast_mult <- c(train$mw[begin_aug_2022:nrow(train)], HW_mult$mean)

hwforecastmult.error = validation$mw - HW_mult$mean

hwforecastmult.MAE = mean(abs(hwforecastmult.error))
hwforecastmult.MAE

hwforecastmult.MAPE=mean(abs(hwforecastmult.error)/validation$mw) * 100
hwforecastmult.MAPE

HW_mult_test <- hw(energy.test, seasonal = "multiplicative", h = 168)

#long dataset of actual/predicted for plotting later
mult_model_test_df <- data.frame(cbind(test$mw, HW_mult_test$mean))
colnames(mult_model_test_df) <- c("actual", "predicted_hw")
mult_model_test_df <- mult_model_test_df %>% pivot_longer(c(actual, predicted_hw))

#append forecasted values to original data, only going to graph from 8/1/2022
aug_test_mw_with_forecast_mult <- c(retrain$mw[begin_aug_2022:nrow(retrain)], HW_mult_test$mean)

#time plots with colored forecast

#subset from 8/1/2022
ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_test_mw_with_forecast_mult, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Multiplicative HW Test Forecast for 10/14/22-10/20/22", color = "Data")
#actual vs predicted data only for test period
ggplot(data = mult_model_test_df, aes(x = tseq_test_week, y = value, color = factor(name))) + 
        scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
        scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
        theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
        labs(x = "Time", y = "Energy (megawatts)", title = "Multiplicative HW Test Forecast for 10/14/22-10/20/22", color = "Data")

#calculate the MAE and MAPE for the test set
hwforecastmult_test.error = test$mw - HW_mult_test$mean

hwforecastmult_test.MAE = mean(abs(hwforecastmult_test.error))
hwforecastmult_test.MAE

hwforecastmult_test.MAPE=mean(abs(hwforecastmult_test.error)/test$mw) * 100
hwforecastmult_test.MAPE

model1222 <- Arima(energy, order = c(1,0,2), seasonal = c(2,1,2))
#forecast
model1222forecast <- forecast::forecast(model1222, h = 168)

arima_model_1_df <- data.frame(cbind(validation$mw, model1222forecast$mean))
colnames(arima_model_1_df) <- c("actual", "predicted_arima")
arima_model_1_df <- arima_model_1_df %>% pivot_longer(c(actual, predicted_arima))

#append forecasted values to original data, only going to graph from 8/1/2022
aug_mw_with_forecast_arima_1 <- c(train$mw[begin_aug_2022:nrow(train)], model1222forecast_test$mean)

arima_error <- abs(model1222forecast$mean - validation$mw)
arima_mae <- mean(arima_error)
arima_mape <- mean(arima_error / validation$mw)
arima_mae
arima_mape

model1222_test <- Arima(energy.test, order = c(1,0,2), seasonal = c(2,1,2))
#forecast
model1222forecast_test <- forecast::forecast(model1222_test, h = 168)

#long dataset of actual/predicted for plotting later
arima_model_1_test_df <- data.frame(cbind(test$mw, model1222forecast_test$mean))
colnames(arima_model_1_test_df) <- c("actual", "predicted_arima")
arima_model_1_test_df <- arima_model_1_test_df %>% pivot_longer(c(actual, predicted_arima))

#append forecasted values to original data, only going to graph from 8/1/2022
aug_test_mw_with_forecast_arima_1 <- c(retrain$mw[begin_aug_2022:nrow(retrain)], model1222forecast_test$mean)

#time plots with colored forecasts

#subset from 8/1/2022
ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_test_mw_with_forecast_arima_1, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Test Forecast for 10/14/22-10/20/22", color = "Data")
#actual vs predicted data only for test period
ggplot(data = arima_model_1_test_df, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Test Forecast for 10/14/22-10/20/22", color = "Data")

arima_test_error <- abs(model1222forecast_test$mean - test$mw)
arima_test_mae <- mean(arima_test_error)
arima_test_mape <- mean(arima_test_error / test$mw)
arima_test_mae
arima_test_mape


############Averaging models
#Validation
For.Avg <- (HW_mult$mean + 
              model1222forecast$mean)/2

combined_df = data.frame(cbind(HW_mult$mean, model1222forecast$mean, 
                               proph_pred, nn_pred, validation$mw))

colnames(combined_df) <- c("HW", "ARIMA", "Prophet", "NN", "Actual")

for (i in 1:4) {
  for (j in 1:4) {
    if (j > i) {
      for (k in seq(0, 1, 0.1)) {
        cols <- colnames(combined_df)
        combined_df <- data.frame(cbind(combined_df, (k * combined_df[i] + (1 - k) * combined_df[j])))
        colnames(combined_df) <- c(cols, paste(paste(as.character(k), cols[i], sep = " "), paste(as.character(1 - k), cols[j], sep = " "), sep = " / "))
      }
    }
  }
}

mape_df <- data.frame()
mae_df <- data.frame()

for (x in 6:length(colnames(combined_df))) {
  temp_error <- combined_df[x] - combined_df$Actual
  temp_ae <- abs(temp_error)
  temp_ape <- abs(temp_error) / combined_df$Actual
  if (x == 6) {
    mape_df <- temp_ape
    mae_df <- temp_ae
  }
  else {
    mape_df <- cbind(mape_df, temp_ape)
    mae_df <- cbind(mae_df, temp_ae)
  }
}

mape_list <- colMeans(mape_df, na.rm = TRUE)
mape_list <- mape_list * 100
min(mape_list)
order(mape_list)
mape_list[28] #0.5 HW / 0.5 NN
mape_list[27] #0.4 HW / 0.6 NN
mape_list[29] #0.6 HW / 0.4 NN
mape_list[26] #0.3 HW / 0.7 NN
####cutoff
mape_list[30]
mape_list[25]

mae_list <-  colMeans(mae_df, na.rm = TRUE)
min(mae_list)
order(mae_list)
mae_list[27] #0.4 / 0.6
mae_list[28] #0.5 / 0.5
mae_list[26] #0.6 / 0.4
mae_list[29] #0.3 / 0.7
####cutoff
mae_list[25]
mae_list[30]

#0.5/0.5
wt_forecast1 <- 0.5 * HW_mult$mean + 0.5 * nn_pred

wt_model_df1 <- data.frame(cbind(validation$mw, wt_forecast1))
colnames(wt_model_df1) <- c("actual", "predicted_wt1")
wt_model_df1 <- wt_model_df1 %>% pivot_longer(c(actual, predicted_wt1))

aug_mw_with_forecast_wt1 <- c(train$mw[begin_aug_2022:nrow(train)], wt_forecast1)

ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_wt1, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.5 HW / 0.5 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

ggplot(data = wt_model_df1, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.5 HW / 0.5 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

#0.4/0.6
wt_forecast2 <- 0.4 * HW_mult$mean + 0.6 * nn_pred

wt_model_df2 <- data.frame(cbind(validation$mw, wt_forecast2))
colnames(wt_model_df2) <- c("actual", "predicted_wt2")
wt_model_df2 <- wt_model_df2 %>% pivot_longer(c(actual, predicted_wt2))

aug_mw_with_forecast_wt2 <- c(train$mw[begin_aug_2022:nrow(train)], wt_forecast2)

ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_wt2, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.4 HW / 0.6 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

ggplot(data = wt_model_df2, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.4 HW / 0.6 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

#0.6/0.4
wt_forecast3 <- 0.6 * HW_mult$mean + 0.4 * nn_pred

wt_model_df3 <- data.frame(cbind(validation$mw, wt_forecast3))
colnames(wt_model_df3) <- c("actual", "predicted_wt3")
wt_model_df3 <- wt_model_df3 %>% pivot_longer(c(actual, predicted_wt3))

aug_mw_with_forecast_wt3 <- c(train$mw[begin_aug_2022:nrow(train)], wt_forecast3)

ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_wt3, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.6 HW / 0.4 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

ggplot(data = wt_model_df3, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.6 HW / 0.4 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

#0.3/0.7
wt_forecast4 <- 0.3 * HW_mult$mean + 0.7 * nn_pred

wt_model_df4 <- data.frame(cbind(validation$mw, wt_forecast4))
colnames(wt_model_df4) <- c("actual", "predicted_wt4")
wt_model_df4 <- wt_model_df4 %>% pivot_longer(c(actual, predicted_wt4))

aug_mw_with_forecast_wt4 <- c(train$mw[begin_aug_2022:nrow(train)], wt_forecast4)

ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_wt4, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.3 HW / 0.7 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

ggplot(data = wt_model_df4, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.3 HW / 0.7 NN Weighted Avg Model Forecast for 10/7/22-10/13/22", color = "Data")

wt_model_df_temp <- wt_model_df1 %>% mutate(count = sort(rep(seq(1, 168), 2)))
wt_model2_only <-  wt_model_df2 %>% filter(name == "predicted_wt2") %>% mutate(count = seq(1, 168))
wt_model3_only <-  wt_model_df3 %>% filter(name == "predicted_wt3") %>% mutate(count = seq(1, 168))
wt_model4_only <-  wt_model_df4 %>% filter(name == "predicted_wt4") %>% mutate(count = seq(1, 168))
wt_model_df <- data.frame(rbind(wt_model_df_temp, wt_model2_only, wt_model3_only, wt_model4_only)) %>% arrange(name)

ggplot(data = wt_model_df, aes(x = tseq_val_week_all_wt, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000", "#dc267f", "#000000", "#777777"), labels = c("Actual", "Predicted 0.5/0.5", "Predicted 0.4/0.6", "Predicted 0.6/0.4", "Predicted 0.3/0.7")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs Top Weighted Average Models Forecast for 10/7/22-10/13/22", color = "Data")

final_model_df_temp <- mult_model_df %>% mutate(count = sort(rep(seq(1, 168), 2)))
arima_model_only <- arima_model_1_df %>% filter(name == "predicted_arima") %>% mutate(count = seq(1, 168))
proph_model_only <- proph_model_df %>% filter(name == "predicted_proph") %>% mutate(count = seq(1, 168))
nn_model_only <- nn_model_df %>% filter(name == "predicted_nn") %>% mutate(count = seq(1, 168))
wt_model_only <- wt_model_df1 %>% filter(name == "predicted_wt1") %>% mutate(count = seq(1, 168))
final_model_df <- data.frame(rbind(final_model_df_temp, arima_model_only, proph_model_only, nn_model_only, wt_model_only)) %>%
  arrange(name)

ggplot(data = final_model_df, aes(x = tseq_val_week_all_final, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000", "#dc267f", "#000000", "#777777", "#f9bfd2"), labels = c("Actual", "Predicted ARIMA", "Predicted HW", "Predicted Prophet", "Predicted NN", "Predicted 0.5 HW / 0.5 NN")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs All Models Forecast for 10/7/22-10/13/22", color = "Data")

best_model_temp <- arima_model_1_df %>% mutate(count = sort(rep(seq(1, 168), 2)))
best_model_df <- data.frame(rbind(best_model_temp, wt_model_only)) %>% arrange(name)

ggplot(data = best_model_df, aes(x = tseq_val_week_all, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000", "#dc267f"), labels = c("Actual", "Predicted ARIMA", "Predicted 0.5 HW / 0.5 NN")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs ARIMA(1,0,2)(2,1,2)[24] vs 0.5 HW / 0.5 NN Forecast for 10/7/22-10/13/22", color = "Data")


#Tried three model weighted average: best combination was still two models
three_model_df <- data.frame()
for (i in 1:10) {
  arima1 <- 0.5 * (i / 10) * HW_mult$mean + 0.5 * (i / 10) * nn_pred + (1 - (i / 10)) * model1222forecast$mean
  
  if (i == 1) {
    three_model_df <- arima1
  }
  else {
    three_model_df <- data.frame(cbind(three_model_df, arima1))
  }
  
  arima2 <- 0.4 * (i / 10) * HW_mult$mean + 0.6 * (i / 10) * nn_pred + (1 - (i / 10)) * model1222forecast$mean
  three_model_df <- data.frame(cbind(three_model_df, arima2))
  arima3 <- 0.6 * (i / 10) * HW_mult$mean + 0.4 * (i / 10) * nn_pred + (1 - (i / 10)) * model1222forecast$mean
  three_model_df <- data.frame(cbind(three_model_df, arima3))
  
  proph1 <- 0.5 * (i / 10) * HW_mult$mean + 0.5 * (i / 10) * nn_pred + (1 - (i / 10)) * proph_pred
  three_model_df <- data.frame(cbind(three_model_df, proph1))
  proph2 <- 0.4 * (i / 10) * HW_mult$mean + 0.6 * (i / 10) * nn_pred + (1 - (i / 10)) * proph_pred
  three_model_df <- data.frame(cbind(three_model_df, proph2))
  proph3 <- 0.6 * (i / 10) * HW_mult$mean + 0.4 * (i / 10) * nn_pred + (1 - (i / 10)) * proph_pred
  three_model_df <- data.frame(cbind(three_model_df, proph3))
  colnames(three_model_df)[(6 * i - 5):(6 * i)] <- c(paste(paste(as.character(0.5 * (i / 10)), "HW", sep = " "), 
                                                           paste(as.character(0.5 * (i / 10)), "NN", sep = " "), 
                                                           paste(as.character(1 - (i / 10)), "ARIMA", sep = " "), sep = " / "), 
                                                     paste(paste(as.character(0.4 * (i / 10)), "HW", sep = " "), 
                                                           paste(as.character(0.6 * (i / 10)), "NN", sep = " "), 
                                                           paste(as.character(1 - (i / 10)), "ARIMA", sep = " "), sep = " / "),
                                                     paste(paste(as.character(0.6 * (i / 10)), "HW", sep = " "), 
                                                           paste(as.character(0.4 * (i / 10)), "NN", sep = " "), 
                                                           paste(as.character(1 - (i / 10)), "ARIMA", sep = " "), sep = " / "),
                                                     paste(paste(as.character(0.5 * (i / 10)), "HW", sep = " "), 
                                                           paste(as.character(0.5 * (i / 10)), "NN", sep = " "), 
                                                           paste(as.character(1 - (i / 10)), "Prophet", sep = " "), sep = " / "),
                                                     paste(paste(as.character(0.4 * (i / 10)), "HW", sep = " "), 
                                                           paste(as.character(0.6 * (i / 10)), "NN", sep = " "), 
                                                           paste(as.character(1 - (i / 10)), "Prophet", sep = " "), sep = " / "),
                                                     paste(paste(as.character(0.6 * (i / 10)), "HW", sep = " "), 
                                                           paste(as.character(0.4 * (i / 10)), "NN", sep = " "), 
                                                           paste(as.character(1 - (i / 10)), "Prophet", sep = " "), sep = " / "))
}


mape_df_three <- data.frame()

for (x in 1:length(colnames(three_model_df))) {
  temp_error <- three_model_df[x] - combined_df$Actual
  temp_ape <- abs(temp_error) / combined_df$Actual
  if (x == 1) {
    mape_df_three <- temp_ape
  }
  else {
    mape_df_three <- cbind(mape_df_three, temp_ape)
  }
}

mape_list_three <- (colMeans(mape_df_three, na.rm = TRUE))
mape_list_three <- mape_list_three * 100
min(mape_list_three)

#Test

#0.5/0.5
wt_forecast1_test <- 0.5 * HW_mult_test$mean + 
                   0.5 * nn_pred_test

wt_model_df1_test <- data.frame(cbind(test$mw, wt_forecast1_test))
colnames(wt_model_df1_test) <- c("actual", "predicted_wt1")
wt_model_df1_test <- wt_model_df1_test %>% pivot_longer(c(actual, predicted_wt1))

aug_mw_with_forecast_wt1_test <- c(retrain$mw[begin_aug_2022:nrow(retrain)], wt_forecast1_test)

ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_mw_with_forecast_wt1_test, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.5 HW / 0.5 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

ggplot(data = wt_model_df1_test, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.5 HW / 0.5 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

wt_model1_error <- abs(wt_forecast1_test - test$mw)
wt_model1_mae <- mean(wt_model1_error) 
wt_model1_mape <- mean(wt_model1_error / test$mw)
wt_model1_mae
wt_model1_mape

#higher mape, lower mape

#0.4/0.6
wt_forecast2_test <- 0.4 * HW_mult_test$mean + 
  0.6 * nn_pred_test

wt_model_df2_test <- data.frame(cbind(test$mw, wt_forecast2_test))
colnames(wt_model_df2_test) <- c("actual", "predicted_wt2")
wt_model_df2_test <- wt_model_df2_test %>% pivot_longer(c(actual, predicted_wt2))

aug_mw_with_forecast_wt2_test <- c(retrain$mw[begin_aug_2022:nrow(retrain)], wt_forecast2_test)

ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_mw_with_forecast_wt2_test, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.4 HW / 0.6 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

ggplot(data = wt_model_df2_test, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.4 HW / 0.6 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

wt_model2_error <- abs(wt_forecast2_test - test$mw)
wt_model2_mae <- mean(wt_model2_error) 
wt_model2_mape <- mean(wt_model2_error / test$mw)
wt_model2_mae
wt_model2_mape

#lower mae, higher mape...

#0.6/0.4
wt_forecast3_test <- 0.6 * HW_mult_test$mean + 
  0.4 * nn_pred_test

wt_model_df3_test <- data.frame(cbind(test$mw, wt_forecast3_test))
colnames(wt_model_df3_test) <- c("actual", "predicted_wt3")
wt_model_df3_test <- wt_model_df3_test %>% pivot_longer(c(actual, predicted_wt3))

aug_mw_with_forecast_wt3_test <- c(retrain$mw[begin_aug_2022:nrow(retrain)], wt_forecast3_test)

ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_mw_with_forecast_wt3_test, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.6 HW / 0.4 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

ggplot(data = wt_model_df3_test, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.6 HW / 0.4 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

wt_model3_error <- abs(wt_forecast3_test - test$mw)
wt_model3_mae <- mean(wt_model3_error) 
wt_model3_mape <- mean(wt_model3_error / test$mw)
wt_model3_mae
wt_model3_mape

#higher mae and mape

#0.3/0.7
wt_forecast4_test <- 0.3 * HW_mult_test$mean + 
  0.7 * nn_pred_test

wt_model_df4_test <- data.frame(cbind(test$mw, wt_forecast4_test))
colnames(wt_model_df4_test) <- c("actual", "predicted_wt4")
wt_model_df4_test <- wt_model_df4_test %>% pivot_longer(c(actual, predicted_wt4))

aug_mw_with_forecast_wt4_test <- c(retrain$mw[begin_aug_2022:nrow(retrain)], wt_forecast4_test)

ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_mw_with_forecast_wt4_test, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.3 HW / 0.7 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

ggplot(data = wt_model_df4_test, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs 0.3 HW / 0.7 NN Weighted Avg Model Test Forecast for 10/14/22-10/20/22", color = "Data")

wt_model4_error <- abs(wt_forecast4_test - test$mw)
wt_model4_mae <- mean(wt_model4_error) 
wt_model4_mape <- mean(wt_model4_error / test$mw)
wt_model4_mae
wt_model4_mape

#highest mae and mape

wt_model_df_temp_test <- wt_model_df1_test %>% mutate(count = sort(rep(seq(1, 168), 2)))
wt_model2_only_test <-  wt_model_df2_test %>% filter(name == "predicted_wt2") %>% mutate(count = seq(1, 168))
wt_model3_only_test <-  wt_model_df3_test %>% filter(name == "predicted_wt3") %>% mutate(count = seq(1, 168))
wt_model4_only_test <-  wt_model_df4_test %>% filter(name == "predicted_wt4") %>% mutate(count = seq(1, 168))
wt_model_df_test <- data.frame(rbind(wt_model_df_temp_test, wt_model2_only_test, wt_model3_only_test, wt_model4_only_test)) %>% arrange(name)

ggplot(data = wt_model_df_test, aes(x = tseq_test_week_all_wt, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000", "#dc267f", "#000000", "#777777"), labels = c("Actual", "Predicted 0.5/0.5", "Predicted 0.4/0.6", "Predicted 0.6/0.4", "Predicted 0.3/0.7")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs Top Weighted Average Models Test Forecast for 10/14/22-10/20/22", color = "Data")

best_model_temp_test <- arima_model_1_test_df %>% mutate(count = sort(rep(seq(1, 168), 2)))
wt_model_only_test <- wt_model_df_temp_test %>% filter(name == "predicted_wt1")
best_model_df_test <- data.frame(rbind(best_model_temp_test, wt_model_only_test)) %>% arrange(name)

ggplot(data = best_model_df_test, aes(x = tseq_test_week_all, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000", "#dc267f"), labels = c("Actual", "Predicted ARIMA", "Predicted 0.5 HW / 0.5 NN")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs ARIMA(1,0,2)(2,1,2)[24] vs 0.5 HW / 0.5 NN Test Forecast for 10/14/22-10/20/22", color = "Data")


####

final_test <- read.csv("https://github.com/felicitimilne/bluefall2hwteam17/raw/main/hrl_load_metered%20-%20test4.csv")
final_test <- final_test[,c(1, 6)]
final_test$datetime_beginning_ept <- mdy_hm(final_test$datetime_beginning_ept, tz = Sys.timezone())
final <- data.frame(rbind(train, validation, test, final_test))

energy.final <- ts(final[,2], start = 2019, frequency = 24)

tseq_2022_final_aug_test <- seq.POSIXt(from = as.POSIXct("2022-08-01 00:00:00", tz = "EST"), length.out = nrow(final) - begin_aug_2022 + 194, by = "hours")
tseq_final_test_week <- seq.POSIXt(from = as.POSIXct("2022-10-27 00:00:00", tz = "EST"), length.out = 192, by = "hours")

dummy_df_aug_final <- data.frame(cbind(seq(1:length(tseq_2022_final_aug_test)), c(rep(FALSE, (length(tseq_2022_final_aug_test) -192)), rep(TRUE, 192))))
colnames(dummy_df_aug_final) <- c("index", "forecast")

#Chosen model
arima1222_final <- Arima(energy.final, order = c(1,0,2), seasonal = c(2,1,2))
arima1222_final_forecast <- forecast::forecast(arima1222_final, h = 192)

#append forecasted values to original data, only going to graph from 8/1/2022
aug_final_mw_with_forecast_arima <- c(final$mw[(begin_aug_2022 - 1):nrow(final)], arima1222_final_forecast$mean)

#time plots with colored forecasts

#subset from 8/1/2022
ggplot(data = dummy_df_aug_final, aes(x = tseq_2022_final_aug_test, y = aug_final_mw_with_forecast_arima, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Test Forecast for 10/27/22-11/3/22", color = "Data")

final_df <- data.frame(cbind(format(tseq_final_test_week), arima1222_final_forecast$mean))
final_df <- final_df[25:192,]

colnames(final_df) <- c("time", "mw")
write_csv(final_df, "/Users/noahjohnson/Downloads/bluefall2hwteam17/Blue 17 TS2 Final Project Forecasted Values.csv")


#Second place model
aug_final_mw_with_forecast_mult <- c(final$mw[(begin_aug_2022 - 1):nrow(final)], HW_mult_final$mean)

ggplot(data = dummy_df_aug_final, aes(x = tseq_2022_final_aug_test, y = aug_final_mw_with_forecast_mult, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Multiplicative HW Test Forecast for 10/21/22-11/3/22", color = "Data")

NN.Model.final <- nnetar(diff(energy.final, 24), p = 1, P = 2)

NN.Forecast.final <- forecast::forecast(NN.Model.final, h = 192)

Pass.Forecast.final <- rep(NA, 192)

for(i in 1:24){
  Pass.Forecast.final[i] <- energy.final[length(energy.final) - 24 + i] + NN.Forecast.final$mean[i]
}

for(i in 25:192){
  Pass.Forecast.final[i] <- Pass.Forecast.final[length(Pass.Forecast.final) - 216 + i] + NN.Forecast.final$mean[i]
}


#Pass.Forecast.test <- ts(Pass.Forecast.test, start = 2022, frequency = 24)

#test_ts <- ts(test[,2], start = 2022, frequency = 24)]

nn_pred_final <- Pass.Forecast.final

aug_final_mw_with_forecast_nn <- c(final$mw[(begin_aug_2022 - 1):nrow(final)], nn_pred_final)

ggplot(data = dummy_df_aug_final, aes(x = tseq_2022_final_aug_test, y = aug_final_mw_with_forecast_nn, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "0.5 HW / 0.5 NN Test Forecast for 10/27/22-11/3/22", color = "Data")

wt_model_pred_final <- 0.5 * HW_mult_final$mean + 0.5 * nn_pred_final

final_df <- data.frame(cbind(format(tseq_final_test_week), wt_model_pred_final))
final_df <- final_df[25:192,]

colnames(final_df) <- c("time", "mw")
write_csv(final_df, "/Users/noahjohnson/Downloads/bluefall2hwteam17/Blue 17 TS2 Final Project Forecasted Values.csv")

aug_final_mw_with_forecast_wt <- c(final$mw[(begin_aug_2022 - 1):nrow(final)], wt_model_pred_final)

ggplot(data = dummy_df_aug_final, aes(x = tseq_2022_final_aug_test, y = aug_final_mw_with_forecast_wt, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  theme(legend.title = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time", y = "Energy (megawatts)", title = "Final Selected Model Test Forecast for 10/27/22-11/3/22", color = "Data")


