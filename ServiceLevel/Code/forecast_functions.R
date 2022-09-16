
# Assume that the mean parameter of the poisson family is normally distributed.

hist(rpois(10000,4))

mean(rpois(1000, 22))
var(rpois(1000, 22))

CF <- seq(0.5, 0.99, 0.01)
CF

hist(rnorm(100, 4, sqrt(4)))

# Generate poisson time series 

sim_ts <- function(days, l, period = 30){
  time <- seq(1,days,1)
  season <- cos(time/90)
  y <- rpois(days, lambda = l)*season
  df <- data.frame(Time = time, sale = y, day = time %% (period + 1))
  return(df)
}


# Get simulated values for 1 period.

sim_pois <- function(x){
  return(rpois(n = 1, lambda = x))
}

# Generate lambda values

gen_lambda <- function(n, lambda){
  lambdas <- lambda + rnorm(n, lambda, sqrt(lambda))/lambda
  return(lambdas)
}

# simulates lambda values for 24 months.

run_simulation <- function(nsim,  lambda = 4, months = 24){
  
  lambdas <- gen_lambda(24, lambda)
  
  
  sim_df <- matrix(nrow = 24, ncol = nsim)
  for(i in 1:ncol(sim_df)) {
    sim_df[,i] <- sapply(lambdas, FUN = sim_pois)
    
  }
  return(sim_df)
}

#one_sim <- rowMeans(run_simulation(100))


# Model that runs a function

fit_model <- function(y){
  y_ts <- ts(y)
  fit <- auto.arima(y_ts)
  
  # arima
  #forecast_y <- forecast::forecast(fit, h = 1)
  # exponential smothing
  forecast_y <- forecast::ses(y_ts, h = 1)
  #forecast_y$model$sigma2 ## standard error
  return(forecast_y$mean)
}
output <- run_simulation(100)
## We get the 100 simulated forecasts for the lambda parameter.
## We can be more creative with the lambda parameters.
forecast <- apply(output, MARGIN = 2, fit_model)
forecast
hist(forecast)
