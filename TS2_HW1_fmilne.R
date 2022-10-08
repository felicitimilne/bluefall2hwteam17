# code for time series 2 homework 1 

# libraries
library(dplyr)
library(ggplot2)
library(ggfortify)

#load data
df <- read.csv("https://raw.githubusercontent.com/felicitimilne/bluefall2hwteam17/main/hrl_load_metered.csv")
head(df)

#reformat date column and drop hours
df$datetime_beginning_ept <- as.Date(df$datetime_beginning_ept, "%m/%d/%y")
head(df)

#roll up hourly data into daily and drop unnecessary columns
rolled_df <- df %>% 
  group_by(datetime_beginning_ept) %>% 
  summarise_at(vars(mw),  list(min, mean, max))

#rename columns
colnames(rolled_df)[colnames(rolled_df) == "fn1"] = "mw_min"
colnames(rolled_df)[colnames(rolled_df) == "fn2"] = "mw_mean"
colnames(rolled_df)[colnames(rolled_df) == "fn3"] = "mw_max"

#create time series object 
MegaWatt_mean <- ts(data = rolled_df$mw_mean, start = c(2019, 8), frequency = 12)

autoplot(MegaWatt_mean) +
  labs(title="Time Series plot for Enegry Usage", x = "Date", y="Megawatts") 

#to correct axis labels later if I can figure it out
#scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2100, 2120), 
#labels = c("-5000", "-2500", "TSS", "2500", "5000"))
#get current labels to correct plot axis (insert tricky stuff here)
#https://stackoverflow.com/questions/31223818/accessing-vector-of-axis-ticks-for-an-existing-plot-in-ggplot2
#ggbld <- ggplot_build(autoplot(MegaWatt_mean))

#split data 
training <- head(MegaWatt_mean, (length(MegaWatt_mean)-12))
test <- tail(MegaWatt_mean, 12)

#decompose
decomp_stl <- stl(training, s.window = 7)
plot(decomp_stl)