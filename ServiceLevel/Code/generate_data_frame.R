library(forecast)

## Only use day or month data.

## Assume we are using historical month data

sim_ts <- function(days, l, period = 30){
  time <- seq(1,days,1)
  season <- cos(time/period)
  y <- rpois(days, lambda = l)*abs(season)
  df <- data.frame(Time = time, sale = y, day = time %% (period + 1))
  return(y)
}



# Generate time series with 3 years of data.
v <- sim_ts(900, 4)

plot(v, type = "l")

# plot(v, type = 'l')

# Function that changes days to months.
days_to_months <- function(v, days){
  month_sale <- unname(tapply(v, (seq_along(v)-1) %/% days, sum))
  return(month_sale)
}
# Siulate aggregate the days to months

t_df <- days_to_months(v, 30)
plot(t_df, type = 'l')
y_ts <- ts(t_df)
fit <- auto.arima(y_ts)
summary( forecast_y <- forecast::forecast(fit, h = 6) )
forecast_y$model

63.33117-1.96*sqrt(118.3)

cf <- 0.95
alpha <- 1-cf

p <- 1-alpha/2
qnorm(p)


# Function to generate months to days back

## Predict 2 months into the future:

arima_fit <- function(y, future = 4){
  y_ts <- ts(y)
  fit <- auto.arima(y_ts)
  forecast_y <- forecast::forecast(fit, h = future)
  return(forecast_y)
}
arima_fit(t_df)
##
month_to_days <- function(value, days = 30){
  daily_value <- rep(value/days, days)
  return(daily_value)
}


month_to_days(fv)

# Data frame that would calulate the estimated stock, based on demand

df_future <- function(st, d, n, lt = 7, of = 7){
  
  demand <- rep(d/n, n)
  stocks <- rep(st, n) 
  ud <- rep(0, n)
  
  arrivals <- seq(1,floor(n/lt))*lt
  ud[arrivals] = 1
  
  data <- data.frame(est_stocks = stocks, demand_n = demand, demand_total = cumsum(demand), undelivered = ud)
  data$est_stocks <- data$est_stocks - data$demand_total
  
  data$demand_op <- d*(lt+of)/n
  
  data$order <- rep(0, n)

  #data$place_order <- rep(0,n)
  stockout <- (which.min(replace(data$est_stocks, data$est_stocks<=0.0, NA)))
  
  data$new_est_stock <- rep(0,n)
  data$new_est_stock[1] <- 1
  for(i in 1:n){
    t_i <- min(i+lt+of, n)
    es_i <- min(i+of, n)
    
    #est_stock <- data$est_stocks[i] - data$demand_n[i] + order[i]
    if(data$est_stocks[i] < sum(data$demand_n[i:t_i])){
      data$order[t_i] <- d*(lt+of)/n
      #data$est_stocks[t_i:n] <- data$est_stocks[(t_i-1):(n-1)] - data$demand_n[t_i:n] + data$order[t_i:n]
      #data$est_stocks[es_i] <- data$est_stocks[es_i-1] + d*(lt+of)/n - data$demand_n[es_i]
      #data$place_order[i-(lt+of)] <- 1
      #print(es_i)
    }
  }
  
  return(data)
}
# 4 order periods => 4*14 = 56 days
# Simulate the first order period and look at the service level on the next 2

(test <- df_future(10, 18, 30))


(y_ts <- ts(t_df) )
fit <- auto.arima(y_ts)
forecast_y <- forecast::forecast(fit, h = 1)

# forecast_y
sum(rpois(30,1.986456))
(fit.mean <- forecast_y$mean)
(fit.std <- sqrt(forecast_y$model$sigma2/30))

saftey_stock(fit.std)

estStock <- function(st, d_avg){
  if(st - d_avg < 0){
    return(0)
  }
  else{
    return(st-d_avg)
  }
}
