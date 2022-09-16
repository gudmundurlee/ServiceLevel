setwd("~/AGR Dynamics/ServiceLevel")
# Load relevant packages

library(readxl)
library(tidyverse)
library(forecast)
# library(ts)
library(ts.extend)
#library(fpp2)
#library(TTS)

# Read in the example data.
df <- read_excel("sale_stock_one_item.xlsx")

str(df)

as.Date(df$history_date)
  
df_train = subset(df, split == "Train")
df_test = subset(df, split == "Test")


# Clean data
plot(df_train$history_date, y = df_train$sale, type = "s")


df_ts <- ts(df$sale)
forecast_ses <- forecast::ses(df_ts, h = 8 ,level = 0.5)
forecast_ses$mean

plot(df_ts)

arima_model <- auto.arima(df_ts)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=8)
fore_arima

# Generate a data frame with the historical values + new forecasts
last_entry <- nrow(df_daily)



# Instead of removing 0 values then we include them for the forecast.

df_daily <- read_excel("Data/sales_daily_one_example.xlsx")


ts_y <- ts(df_daily$sale)
arima_y <- auto.arima(ts_y)
summary(arima_y)
fore_arima_y = forecast::forecast(arima_y, h=32)
fore_arima_y$mean
