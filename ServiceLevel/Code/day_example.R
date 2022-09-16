
library(readxl)
df_daily <- read_excel("Data/sales_daily_one_example.xlsx")

ts_daily <- ts(df_daily$sale)

# Exponential smoothing 

ses_day <- forecast::ses(ts_daily, h = 30 ,level = 0.95) 
summary(ses_day)

(demand <- ses_month$mean)
(sd_demand <- sqrt(ses_month$model$sigma2))