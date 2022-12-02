
library(readxl)
df_daily <- read_excel("Data/sales_daily_one_example.xlsx")

ts_daily <- ts(df_daily$sale)

# Exponential smoothing 

ses_day <- forecast::ses(ts_daily, h = 30 ,level = 0.95) 
summary(ses_day)

(demand <- ses_month$mean)
(sd_demand <- sqrt(ses_month$model$sigma2))

y.test <- random_ts(4,2, T)
h <- 12
y.m <- unname(tapply(y.test, (seq_along(y.test)-1) %/% 30, sum)) 
y.m <- ts(y.m, start = 2019, frequency = 12)
fit_model(y.m, h)
fit.test <- auto.arima(y.m)
f.test <- forecast::forecast(fit.test, h = h)
plot(f.test)
f_v <- f.test$mean
f_sigma2 <- f.test$model$sigma2


tt <- random_ts(4,10)
y.train <- tt[1:(1440-360-1)]
y.test <- tt[(1440-360):1440]

y.m <- ts(unname(tapply(y.train, (seq_along(y.train)-1) %/% 30, sum)))
plot(y.ts)

fit <- auto.arima(y.ts)
f_v <- forecast::forecast(fit, h = 12)
summary(f_v)
plot(f_v)

