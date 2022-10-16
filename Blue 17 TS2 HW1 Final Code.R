#install libraries
library(tidyverse)
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
#install.packages("prophet")
library(prophet)
library(gridExtra)

###

#fundamental steps

# original data
hrl_load_metered <- read.csv("~/Downloads/Homework1_TS2/hrl_load_metered.csv")

# dropping unnecessary variables
data <- hrl_load_metered[,c(1,6)]

#imputing 0 values with point in middle of line between the two around it
for (i in 1:nrow(data)) {
  if (data$mw[i] == 0) {
    data$mw[i] = (data$mw[i + 1] + data$mw[i - 1]) / 2
  }
}

#read in validation and test sets
val <- read.csv("~/Downloads/hrl_load_metered - test1.csv")
test <- read.csv("~/Downloads/hrl_load_metered - test2.csv")

#time series object for plotting (yearly seasonality)
energy <- ts(data[,2], start = 2019 + 212/365, frequency = 8766)

#time series object for modeling (daily seasonality)
energy_model <- ts(data[,2], start = 2019 + 212/365, frequency = 24)

#time plot
energy_plot <- autoplot(energy, color = "#648fff") + 
  ggtitle("Hourly Energy Usage from 8/1/19 to 9/22/22") +
  xlab("Time") +
  ylab("Energy (megawatts)")

#create datetime sequence of timeframe
tseq <- seq.POSIXt(from = as.POSIXct("2019-08-01 00:00:00"), length.out = nrow(data), by = "hours")

#STL decomposition
energy_stl <- stl(energy, s.window = 24)
trend <- ggplot(data = data, aes(x = tseq, y = energy_stl$time.series[,"trend"])) + 
  geom_line(color = "#dc267f") + labs(x = "Time", y = "Energy (megawatts)", title = "Trend of Hourly Energy Usage") + 
  theme(plot.title = element_text(hjust = 0.5))

season <- ggplot(data = data, aes(x = tseq, y = energy_stl$time.series[,"seasonal"])) + 
  geom_line(color = "#ffb000") + labs(x = "Time", y = "Energy (megawatts)", title = "Seasonality of Hourly Energy Usage") + 
  theme(plot.title = element_text(hjust = 0.5))

error <- ggplot(data = data, aes(x = tseq, y = energy_stl$time.series[,"remainder"])) + 
  geom_line() + labs(x = "Time", y = "Energy (megawatts)", title = "Error of Hourly Energy Usage") + 
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(energy_plot, grid.arrange(trend, season, error, ncol = 1), ncol = 2)

###

#model building pre-processing

#datetime vector for whole dataset + validation times
tseq_with_forecast <- c(tseq, seq.POSIXt(from = as.POSIXct("2022-09-23 00:00:00", tz = "EST"), length.out = 168, by = "hours"))
#datetime vector for only 2022 + validation times
begin_2022 <- match("1/1/22 0:00", data$datetime_beginning_ept)
tseq_2022 <- seq.POSIXt(from = as.POSIXct("2022-01-01 00:00:00", tz = "EST"), length.out = nrow(data) - begin_2022 + 170, by = "hours")
tseq_2022 <- tseq_2022[c(seq(1, match(as.POSIXct("2022-03-13 01:00:00", tz = "EST"), tseq_2022)), seq(match(as.POSIXct("2022-03-13 03:00:00", tz = "EST"), tseq_2022), length(tseq_2022)))]
#datetime vector from 8/1/22 + validation times, same order for each code block
begin_aug_2022 <- match("8/1/22 0:00", data$datetime_beginning_ept)
tseq_2022_aug <- seq.POSIXt(from = as.POSIXct("2022-08-01 00:00:00", tz = "EST"), length.out = nrow(data) - begin_aug_2022 + 169, by = "hours")

#make validation only datetime vector
tseq_val_week <- seq.POSIXt(from = as.POSIXct("2022-09-23 00:00:00", tz = "EST"), length.out = 168, by = "hours")
tseq_val_week <- c(tseq_val_week, tseq_val_week)
tseq_val_week <- sort(tseq_val_week)

#create dummy df for ggplot
whole_dummy_df <- data.frame(cbind(seq(1:length(tseq_with_forecast)), c(rep(FALSE, (length(tseq))), rep(TRUE, 168))))
dummy_df <- data.frame(cbind(seq(1:length(tseq_2022)), c(rep(FALSE, (length(tseq_2022) - 168)), rep(TRUE, 168))))
dummy_df_aug <- data.frame(cbind(seq(1:length(tseq_2022_aug)), c(rep(FALSE, (length(tseq_2022_aug) - 168)), rep(TRUE, 168))))

###

#Holt-Winters model building

#initialize model
HW_add <- hw(energy_model, seasonal = "additive", h = 168)

#show residual chart, ACF/PACF
hw_resid_plot <- ggplot(data = whole_dummy_df[1:27576,], aes(x = tseq, y = HW_add$residuals)) + geom_point(color = "#648fff") + 
  scale_x_datetime(date_labels = "%Y") + labs(x = "Time", y = "Residuals", title = "Additive HW Residuals")
hw_acf <- ggAcf(HW_add$residuals) + labs(x = "Lag", y = "Autocorrelation", title = "ACF for Additive HW")
hw_pacf <- ggPacf(HW_add$residuals) + labs(x = "Lag", y = "Partial Autocorrelation", title = "PACF for Additive HW")

#show white noise histogram
hw_wn_hist <- ggplot(data = whole_dummy_df[1:27576,], aes(x = HW_add$residuals)) + geom_histogram(aes(y = ..density..), fill = "#648fff") + 
  geom_density(color = "#ffb000") + labs(x = "Residuals", y = "Density", title = "Residual Histogram for Additive HW")

grid.arrange(hw_resid_plot, hw_wn_hist, ncol = 2)

#show L-B test
checkresiduals(HW_add, plot = FALSE)

#add column names
colnames(whole_dummy_df) <- c("index", "forecast")
colnames(dummy_df) <- c("index", "forecast")
colnames(dummy_df_aug) <- c("index", "forecast")

#long dataset with actual/predicted for plotting later
add_model_df <- data.frame(cbind(val$mw, HW_add$mean))
colnames(add_model_df) <- c("actual", "predicted")
add_model_df <- add_model_df %>% pivot_longer(c(actual, predicted))

#append forecasted values to actual data
whole_mw_with_forecast_add <- c(data$mw, HW_add$mean)
mw_with_forecast_add <- c(data$mw[begin_2022:nrow(data)], HW_add$mean)
aug_mw_with_forecast_add <- c(data$mw[begin_aug_2022:nrow(data)], HW_add$mean)

#time plots with forecast colored

#whole dataset
ggplot(data = whole_dummy_df, aes(x = tseq_with_forecast, y = whole_mw_with_forecast_add, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Additive HW Forecast for 9/23/22-9/29/22", color = "Data")
#yearly subset
ggplot(data = dummy_df, aes(x = tseq_2022, y = mw_with_forecast_add, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Additive HW Forecast for 9/23/22-9/29/22", color = "Data")
#subset from 8/1/2022
ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_add, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Additive HW Forecast for 9/23/22-9/29/22", color = "Data")
#actual vs predicted data only over validation period
ggplot(data = add_model_df, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs Additive HW Forecast for 9/23/22-9/29/22", color = "Data")

#calculate MAE and MAPE
hwforecast.error = val$mw - HW_add$mean

hwforecast.MAE = mean(abs(hwforecast.error))
hwforecast.MAE

hwforecast.MAPE=mean(abs(hwforecast.error)/val$mw) 
hwforecast.MAPE

###

# seasonal unit-root testing
energy_model %>% nsdiffs()
# stochastic seasonality -> take 1 difference
energy_model %>% diff(lag = 24) %>% ggtsdisplay()


###

#hand-chosen ARIMA 1 model building
model1222 <- Arima(energy_model, order = c(1,0,2), seasonal = c(2,1,2))

#show residual chart, ACF/PACF
arima_1_resid_plot <- ggplot(data = whole_dummy_df[1:27576,], aes(x = tseq, y = model1222$residuals)) + geom_point(color = "#648fff") + 
  scale_x_datetime(date_labels = "%Y") + labs(x = "Time", y = "Residuals", title = "ARIMA(1,0,2)(2,1,2)[24] Residuals")
arima_1_acf <- ggAcf(model1222$residuals) + labs(x = "Lag", y = "Autocorrelation", title = "ACF for ARIMA(1,0,2)(2,1,2)[24]")
arima_1_pacf <- ggPacf(model1222$residuals) + labs(x = "Lag", y = "Partial Autocorrelation", title = "PACF for ARIMA(1,0,2)(2,1,2)[24]")

#show white noise histogram
arima_1_wn_hist <- ggplot(data = whole_dummy_df[1:27576,], aes(x = model1222$residuals)) + geom_histogram(aes(y = ..density..), fill = "#648fff") + 
  geom_density(color = "#ffb000") + labs(x = "Residuals", y = "Density", title = "Residual Histogram for ARIMA(1,0,2)(2,1,2)[24]")

grid.arrange(arima_1_resid_plot, arima_1_wn_hist, arima_1_acf, arima_1_pacf, nrow = 2, ncol = 2)

#show L-B test and white noise histogram
checkresiduals(model1222, plot = FALSE)

#forecast
model1222forecast <- forecast::forecast(model1222, h = 168)

#long dataset with actual/predicted for plotting later
arima_model_1_df <- data.frame(cbind(val$mw, model1222forecast$mean))
colnames(arima_model_1_df) <- c("actual", "predicted")
arima_model_1_df <- arima_model_1_df %>% pivot_longer(c(actual, predicted))

#append forecasted values to original data
whole_mw_with_forecast_arima_1 <- c(data$mw, model1222forecast$mean)
mw_with_forecast_arima_1 <- c(data$mw[begin_2022:nrow(data)], model1222forecast$mean)
aug_mw_with_forecast_arima_1 <- c(data$mw[begin_aug_2022:nrow(data)], model1222forecast$mean)

#time plots with forecast colored

#whole dataset
ggplot(data = whole_dummy_df, aes(x = tseq_with_forecast, y = whole_mw_with_forecast_arima_1, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data")
#yearly subset
ggplot(data = dummy_df, aes(x = tseq_2022, y = mw_with_forecast_arima_1, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data")
#subset from 8/1/2022
ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_arima_1, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data")
#actual vs predicted data only for validation period
ggplot(data = arima_model_1_df, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs ARIMA(1,0,2)(2,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data")

#calculate the MAE and MAPE
model1222forecast.error = val$mw - model1222forecast$mean

model1222forecast.MAE = mean(abs(model1222forecast.error))
model1222forecast.MAE

model1222forecast.MAPE=mean(abs(model1222forecast.error)/val$mw) 
model1222forecast.MAPE


###

#hand-chosen ARIMA 2 model building
model1323 <- Arima(energy_model, order = c(1,0,3), seasonal = c(2,1,3))

#show residual chart, ACF/PACF
arima_2_resid_plot <- ggplot(data = whole_dummy_df[1:27576,], aes(x = tseq, y = model1323$residuals)) + geom_point(color = "#648fff") + 
  scale_x_datetime(date_labels = "%Y") + labs(x = "Time", y = "Residuals", title = "ARIMA(1,0,3)(2,1,3)[24] Residuals")
arima_2_acf <- ggAcf(model1323$residuals) + labs(x = "Lag", y = "Autocorrelation", title = "ACF for ARIMA(1,0,3)(2,1,3)[24]")
arima_2_pacf <- ggPacf(model1323$residuals) + labs(x = "Lag", y = "Partial Autocorrelation", title = "PACF for ARIMA(1,0,3)(2,1,3)[24]")

#show white noise histogram
arima_2_wn_hist <- ggplot(data = whole_dummy_df[1:27576,], aes(x = model1323$residuals)) + geom_histogram(aes(y = ..density..), fill = "#648fff") + 
  geom_density(color = "#ffb000") + labs(x = "Residuals", y = "Density", title = "Residual Histogram for ARIMA(1,0,3)(2,1,3)[24]")

grid.arrange(arima_2_resid_plot, arima_2_wn_hist, arima_2_acf, arima_2_pacf, nrow = 2, ncol = 2)


#show L-B test, white noise histogram
checkresiduals(model1323, plot = FALSE)

#forecast
model1323forecast <- forecast::forecast(model1323, h = 168)

#long dataset of actual/predicted for plotting later
arima_model_2_df <- data.frame(cbind(val$mw, model1323forecast$mean))
colnames(arima_model_2_df) <- c("actual", "predicted")
arima_model_2_df <- arima_model_2_df %>% pivot_longer(c(actual, predicted))

#append forecasted values to original data
whole_mw_with_forecast_arima_2 <- c(data$mw, model1323forecast$mean)
mw_with_forecast_arima_2 <- c(data$mw[begin_2022:nrow(data)], model1323forecast$mean)
aug_mw_with_forecast_arima_2 <- c(data$mw[begin_aug_2022:nrow(data)], model1323forecast$mean)

#time plots with forecast colored

#whole dataset
ggplot(data = whole_dummy_df, aes(x = tseq_with_forecast, y = whole_mw_with_forecast_arima_2, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,3)(2,1,3)[24] Forecast for 9/23/22-9/29/22", color = "Data")
#yearly subset
ggplot(data = dummy_df, aes(x = tseq_2022, y = mw_with_forecast_arima_2, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,3)(2,1,3)[24] Forecast for 9/23/22-9/29/22", color = "Data")
#from 8/1/2022
ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_arima_2, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,3)(2,1,3)[24] Forecast for 9/23/22-9/29/22", color = "Data")
#acutal vs predicted data only for validation period
ggplot(data = arima_model_2_df, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs ARIMA(1,0,3)(2,1,3)[24] Forecast for 9/23/22-9/29/22", color = "Data")

#calculate the MAE and MAPE
model1323forecast.error = val$mw - model1323forecast$mean

model1323forecast.MAE = mean(abs(model1323forecast.error))
model1323forecast.MAE

model1323forecast.MAPE=mean(abs(model1323forecast.error)/abs(val$mw)) 
model1323forecast.MAPE

###

#automatically chosen ARIMA model building
#automodel <- auto.arima(energy_model, method="ML", seasonal = TRUE, D = 1, stepwise = TRUE)

#returns ARIMA(0,0,2)(0,1,2)[24] (run to save time)
automodel <- Arima(energy_model, order = c(0, 0, 2), seasonal = c(0, 1, 2))
#show residual plot, ACF/PACF
arima_3_resid_plot <- ggplot(data = whole_dummy_df[1:27576,], aes(x = tseq, y = automodel$residuals)) + geom_point(color = "#648fff") + 
  scale_x_datetime(date_labels = "%Y") + labs(x = "Time", y = "Residuals", title = "ARIMA(0,0,2)(0,1,2)[24] Residuals")
arima_3_acf <- ggAcf(automodel$residuals) + labs(x = "Lag", y = "Autocorrelation", title = "ACF for ARIMA(0,0,2)(0,1,2)[24]")
arima_3_pacf <- ggPacf(automodel$residuals) + labs(x = "Lag", y = "Partial Autocorrelation", title = "PACF for ARIMA(0,0,2)(0,1,2)[24]")

#show white noise histogram
arima_3_wn_hist <- ggplot(data = whole_dummy_df[1:27576,], aes(x = automodel$residuals)) + geom_histogram(aes(y = ..density..), fill = "#648fff") + 
  geom_density(color = "#ffb000") + labs(x = "Residuals", y = "Density", title = "Residual Histogram for ARIMA(0,0,2)(0,1,2)[24]")

grid.arrange(arima_3_resid_plot, arima_3_wn_hist, arima_3_acf, arima_3_pacf, nrow = 2, ncol = 2)


#show L-B test, white noise histogram
checkresiduals(automodel, plot = FALSE)

#forecast
automodelforecast <- forecast::forecast(automodel, h = 168)

#long dataset of actual/predicting for plotting later
arima_model_3_df <- data.frame(cbind(val$mw, automodelforecast$mean))
colnames(arima_model_3_df) <- c("actual", "predicted")
arima_model_3_df <- arima_model_3_df %>% pivot_longer(c(actual, predicted))

#append forecasted values with original data
whole_mw_with_forecast_arima_3 <- c(data$mw, automodelforecast$mean)
mw_with_forecast_arima_3 <- c(data$mw[begin_2022:nrow(data)], automodelforecast$mean)
aug_mw_with_forecast_arima_3 <- c(data$mw[begin_aug_2022:nrow(data)], automodelforecast$mean)

#time plots with colored forecast

#whole dataset
grid.arrange(ggplot(data = whole_dummy_df, aes(x = tseq_with_forecast, y = whole_mw_with_forecast_arima_3, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(0,0,2)(0,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data"),
ggplot(data = dummy_df, aes(x = tseq_2022, y = mw_with_forecast_arima_3, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(0,0,2)(0,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data"),
ggplot(data = dummy_df_aug, aes(x = tseq_2022_aug, y = aug_mw_with_forecast_arima_3, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(0,0,2)(0,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data"), ncol = 1)
#actual vs predicted data only for validation period
ggplot(data = arima_model_3_df, aes(x = tseq_val_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Actual vs ARIMA(0,0,2)(0,1,2)[24] Forecast for 9/23/22-9/29/22", color = "Data")

#calculate the MAE and MAPE
automodelforecast.error = val$mw - automodelforecast$mean

automodelforecast.MAE = mean(abs(automodelforecast.error))
automodelforecast.MAE

automodelforecast.MAPE=mean(abs(automodelforecast.error)/abs(val$mw)) 
automodelforecast.MAPE

###

#testing

#combine training and validation sets
train_val <- data.frame(rbind(data, val[,c(1,6)]))
energy_model_train_val <- ts(train_val[,2], start = 2019 + 212/365, frequency = 24)

#create new datetime vector for test set timeframe
tseq_2022_aug_test <- seq.POSIXt(from = as.POSIXct("2022-08-01 00:00:00", tz = "EST"), length.out = nrow(data) - begin_aug_2022 + 337, by = "hours")
tseq_test_week <- seq.POSIXt(from = as.POSIXct("2022-09-30 00:00:00", tz = "EST"), length.out = 168, by = "hours")
tseq_test_week <- c(tseq_test_week, tseq_test_week)
tseq_test_week <- sort(tseq_test_week)

#create dummy df for ggplot
dummy_df_aug_test <- data.frame(cbind(seq(1:length(tseq_2022_aug_test)), c(rep(FALSE, (length(tseq_2022_aug_test) - 168)), rep(TRUE, 168))))
colnames(dummy_df_aug_test) <- c("index", "forecast")

#retrain HW
HW_add_test <- hw(energy_model_train_val, seasonal = "additive", h = 168)

#long dataset of actual/predicted for plotting later
add_model_test_df <- data.frame(cbind(test$mw, HW_add_test$mean))
colnames(add_model_test_df) <- c("actual", "predicted")
add_model_test_df <- add_model_test_df %>% pivot_longer(c(actual, predicted))

#append forecasted values to original data, only going to graph from 8/1/2022
aug_test_mw_with_forecast_add <- c(train_val$mw[begin_aug_2022:nrow(train_val)], HW_add_test$mean)

#time plots with colored forecast

#subset from 8/1/2022
ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_test_mw_with_forecast_add, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Additive HW Test Forecast for 9/30/22-10/6/22", color = "Data")
#actual vs predicted data only for test period
grid.arrange(ggplot(data = add_model_test_df, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "Additive HW Test Forecast for 9/30/22-10/6/22", color = "Data"),
ggplot(data = arima_model_1_test_df, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Test Forecast for 9/30/22-10/6/22", color = "Data"), ncol = 1)

#calculate the MAE and MAPE for the test set
hwforecast_test.error = test$mw - HW_add_test$mean

hwforecast_test.MAE = mean(abs(hwforecast_test.error))
hwforecast_test.MAE

hwforecast_test.MAPE=mean(abs(hwforecast_test.error)/test$mw) 
hwforecast_test.MAPE

#retrain hand-picked ARIMA model 1 -> chosen ARIMA to test
model1222_test <- Arima(energy_model_train_val, order = c(1,0,2), seasonal = c(2,1,2))
#forecast
model1222forecast_test <- forecast::forecast(model1222_test, h = 168)

#long dataset of actual/predicted for plotting later
arima_model_1_test_df <- data.frame(cbind(test$mw, model1222forecast_test$mean))
colnames(arima_model_1_test_df) <- c("actual", "predicted")
arima_model_1_test_df <- arima_model_1_test_df %>% pivot_longer(c(actual, predicted))

#append forecasted values to original data, only going to graph from 8/1/2022
aug_test_mw_with_forecast_arima_1 <- c(train_val$mw[begin_aug_2022:nrow(train_val)], model1222forecast_test$mean)

#time plots with colored forecasts

#subset from 8/1/2022
ggplot(data = dummy_df_aug_test, aes(x = tseq_2022_aug_test, y = aug_test_mw_with_forecast_arima_1, color = factor(forecast), group = 1)) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Original", "Forecast")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Test Forecast for 9/30/22-10/6/22", color = "Data")
#actual vs predicted data only for test period
ggplot(data = arima_model_1_test_df, aes(x = tseq_test_week, y = value, color = factor(name))) + 
  scale_x_datetime(date_labels = "%m/%d/%Y") + geom_line() + 
  scale_color_manual(values = c("#648fff", "#ffb000"), labels = c("Actual", "Predicted")) + 
  labs(x = "Time", y = "Energy (megawatts)", title = "ARIMA(1,0,2)(2,1,2)[24] Test Forecast for 9/30/22-10/6/22", color = "Data")

#calculate the MAE and MAPE for the test set
model1222forecast_test.error = test$mw - model1222forecast_test$mean

model1222forecast_test.MAE = mean(abs(model1222forecast_test.error))
model1222forecast_test.MAE

model1222forecast_test.MAPE=mean(abs(model1222forecast_test.error)/test$mw) 
model1222forecast_test.MAPE

grid.arrange(arima_1_acf, arima_1_pacf, ncol=2)
