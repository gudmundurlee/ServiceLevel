library(forecast)
library(ggplot2)
library(dplyr)  
library(purrr)
library(reshape2)
library(ggridges)
library(viridis)
library(hrbrthemes)

# Generate data frame

d_to_m <- function(y, period = 30){
  ts.y <- ts(unname(tapply(y, (seq_along(y)-1) %/% 30, sum)) )
  return(ts.y)
}

gen_demand_data <- function(demand_a, demand_p, ss, st, of, lt){
  # Demand_p: Demand predicted
  # Demand_a: Demand actual.
  
  n <- length(demand_p)
  days <- seq(1,n,1)
  
  delivered <- rep(0,n)
  
  est_stock <- rep(0,n)
  est_stock[1] <- max(st - demand_p[1],0) #sum(demand_p[1:lt]) + st
  # Initalize 
  
  is_order <- rep(0,n) 
  is_delivery <- rep(0,n)
  
  order_days <- days[days %% of == 0] - (of - 1) #(seq(1,floor(n/of))*of) - (of - 1)
  #delivered_days <- days[days %% (of+lt) == 0] - (of -1) #(seq(1,floor(n/of))*of) - (of-1) + lt
  delivered_days <- days[days %% (lt) == 0]
  #If it is a delivery or order
  is_order[order_days] <- 1
  is_delivery[delivered_days] <- 1
  # Service Level
  sl <- rep(1,n)
  
  op <- min((lt+of),1)
  
  # initalize all values
  est_stock[1] <- est_stock[1] - demand_a[1]
  acc_est_stock <- est_stock
  # If the estimated stock does not cover the OP then order the demand for the LT.
  if( est_stock[1]  < sum(demand_p[2:op]) - sum(delivered[2:op]) ){
    delivered[lt] <- max(ceiling((demand_p[2:op]) - est_stock[1] + ss),0) #ceiling(ss) #ceiling(lt*(lambda) + ss)
  }
  
  data <- data.frame(demand_a = demand_a, demand_p = demand_p, est_stock, acc_est_stock, delivered, is_order, is_delivery)
  
  return(data)
}

cf_sl <- function(demand_a,demand_p, ss, st, lt, of){
  n <- length(demand_a)

  df <- gen_demand_data(demand_a, demand_p, ss,st, lt, of)

  output <- demand(df,2, n, ss, lt, of)
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
  return(sl)
}

cf_data <- function(year, lambda, h, lt, of){
  period <- 30
  days <- year*360
  # get y
  y <- random_ts(year,lambda)
  y_train <- y[1:(days-h*period)]
  y_test <- y[(days-h*period+1):days]
  # Fit model
  y_train <- unname(tapply(y_train, (seq_along(y_train)-1) %/% period, sum))  
  
  # extract data
  model <- fit_model(y_train, h)
  #Extract features
  
  demand_p.m <- model$pred
  sigma2 <- model$sigma2/30^2
  
  demand_a <- y_test# Transform data to daily
  demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
  
  
  # Fix some values of the order
  #cf <- c(0,0.5,0.6,0.7,0.8,0.9,0.95,0.99)
  cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  # Initalize saftey stock
  
  ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
  st <- sum(demand_p[1:lt])
  
  
  output <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of)})
  return(output)
}


# Instead of a for loop we can use the following:
demand <- function(df, i, n, ss, lt, of) {
  #demand <- function(df, i, n, ...) {  
  # df has to have names: "demand_a","demand_p","est_stock","acc_est_stock","delivered","is_order","is_delivery" 
  ii <- min(i+1,n)
  t_i <- min(i+lt+of, n)
  of_i <- min(i+of, n)
  lt_i <- min(i+lt, n)
  op_i <- min(i+(lt + of), n)
  
  if(i <= n){
    df$acc_est_stock[i] <- df$est_stock[i-1]- df$demand_a[i] + df$delivered[i]
    # Lost sale: 
    #df$est_stock[i] <-df$est_stock[i-1]- df$demand_a[i] + df$delivered[i] 
    df$est_stock[i] <-max(df$est_stock[i-1]- df$demand_a[i] + df$delivered[i],0)
    #Using Standard: 245.0000[stock] - 740.8296[demand] + 736.0000[undeliv. arrived] - 184.3050[safety stock] - 0.0000[min stock] = 55.8654[result] is greater than 0 so calculated order qty is 0. Order qty: 0
    if( ( df$est_stock[i] - ss <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {
    #if( ( df$est_stock[i] <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {  
      df$delivered[lt_i] <- max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]) - df$est_stock[i] + ss),0)
    }
    return(demand(df, i+1, n, ss, lt, of))
  }
  else {
    return(df)
  }
}

demand_calculation <- function(y_test,demand_n, sigma2, cf = 0.95, lt = 7, of = 7){
  
  period <- 30
  
  # Demands
  demand_a <- y_test
  #demand_n <- daily_demand(f_v)
  
  #Simulate the results
  
  n <- length(y_test)
  # Compute z_score
  
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  #ss <- sqrt(sigma2)*qnorm(1-alpha/2)*sqrt(of+lt)
  
  #ss <- sqrt(sigma2)*qnorm(cf)*sqrt(of+lt)
  ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
  st <- sum(demand_n[1:lt])
  
  #print(ss)
  # Generate the data famre
  
  sim_df <- gen_demand_data(demand_a, demand_n, ss, st, of, lt)
  
  op <- (lt+of)
  
  sl <- rep(1,n)
  # n days into the future
  for(i in 2:n){ 
    ii <- min(i+1,n)
    t_i <- min(i+lt+of, n)
    
    of_i <- min(i+of, n)
    lt_i <- min(i+lt, n)
    
    op_i <- min(i+op, n)
    # Simulate the demand
    #print(sim$demand_p)
    #Calculate est_stock
    sim_df$acc_est_stock[i] <- sim_df$est_stock[i-1] - sim_df$demand_a[i] + sim_df$delivered[i]
    sim_df$est_stock[i] <- max(sim_df$est_stock[i-1] - sim_df$demand_a[i] + sim_df$delivered[i],0) # Ignore negatives
    
    if(sim_df$est_stock[i]  < sum(sim_df$demand_p[ii:op_i]) - sum(sim_df$delivered[ii:op_i]) & sim_df$is_order[i] == 1){
      sim_df$delivered[lt_i] <- max(ceiling(sum(sim_df$demand_p[ii:op_i]) - sum(sim_df$delivered[ii:op_i]) - sim_df$est_stock[i] + ss),0) ## Cover lead time or order period?
      #sim_df$delivered[lt_i] <- max(ceiling(sum(sim_df$demand_p[lt_i:op_i])-sum(sim_df$delivered[lt_i:op_i]) + ss),0) ## Cover lead time or order period?
      #sim_df$delivered[lt_i] <- max(ceiling(sum(sim_df$demand_p[lt_i:op_i])-sum(sim_df$delivered[lt_i:op_i]) + ss),0) ## Cover lead time or order period?
    }
    
    sl[i] <- (sim_df$est_stock[i] > 0)
  }
  
  #actual_vs_pred <- data.frame(Actual = y_test, Predicted = demand_n)
  #ls <- list(Data = sim_df, ServiceLevel = sl, OrderFrequency = of, LeadTime = lt, ConfidenceFactor = cf, SafteyStock = ss)
  #return(mean(sl))
  return(sl)
}
demand_calculation <- function(y_test,demand_n, sigma2, cf = 0.95, lt = 7, of = 7){
  
  period <- 30
  
  # Demands
  demand_a <- y_test

  n <- length(y_test)
  # Compute z_score
  
  alpha <- 1-cf
  z_score <- 1-alpha/2
  # Initalize saftey stock
  ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
  st <- sum(demand_n[1:lt])
  
  #print(ss)
  # Generate the data famre
  
  sim_df <- gen_demand_data(demand_a, demand_n, ss, st, of, lt)
  
  op <- (lt+of)

  output <- demand(df = sim_df,i = 2, n = n, ss = ss, lt = lt, of = of)
  skip <- (n-(lt+of)):n
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
  #sl <- sl[-skip,]
  return(sl)
}
#demand_calculation(y_test,demand_n, fit$sigma2, 0)
# Function that generatos time series
ts_generator <- function(years, lambda){
  no_days <- years*360
  no_months <- no_days/30
  
  y <- rpois(no_days, lambda )
  
  # Seasonal effect
  # Maybe do this in a more clean way.
  season_d <-rep(c(
    rep(0.04,30),rep(0.04,30),rep(0.05,30), rep(0.09,30), rep(0.135,30),rep(0.15,30),
    rep(0.13,30),rep(0.125,30), rep(0.08,30), rep(0.06,30), rep(0.05,30), rep(0.05,30)
  ),years)
  
  #season <- c(0.02,0.01,0.04, 0.10, 0.14, 0.18, 0.2,0.13, 0.08, 0.05, 0.03, 0.03)
  
  yv <-  y*season_d*mean(y)
  y_m <- unname(tapply(yv, (seq_along(yv)-1) %/% 30, sum))
  y_y <- unname(tapply(y_m, (seq_along(y_m)-1) %/% 12, sum))
  
  return(ts(yv))
  
}

# Fit models
fit_model <- function(y, h){
  fit <- auto.arima(y)
  f <- forecast::forecast(fit, h = h)
  f_v <- f$mean
  f_sigma2 <- f$model$sigma2
  return(list(pred = f_v, sigma2 = f_sigma2, fit = fit))
}

# Simple fit of the data:

train_test <- function(years, lambda, h, period = 30){
  
  days <- years*360
  
  y <- ts_generator(years, lambda)
  
  # Train test split
  y_train <- y[1:(days-h*period)]
  y_test <- y[(days-h*period+1):days]
  # Fit on the training data
  model <- fit_model(y_train, h*period)
  #Extract features
  demand_p <- model$pred
  sigma2 <- model$sigma2
  
  return(list(y = y_train, demand_a = y_test, demand_p, sigma2))
}


ts_day <- function(x){
  return(rpois(30,x))
}

random_ts <- function(years, lambda = 10, plot = FALSE){
  n <- years*360
  w <- n/30
  
  # Uniform lambda values
  (lambdas <- runif(w,0,lambda))
  
  (y_mat <- sapply(lambdas, FUN = function(x){rpois(30,x)}))
  
  y_vec <- c(matrix(y_mat,nrow = 1, byrow = TRUE))
  
  ym_vec <- unname(tapply(y_vec, (seq_along(y_vec)-1) %/% 30, sum))  
  
  if(plot == TRUE){
    
    years <- length(ym_vec)/12
    
    months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec")
    
    df_plot <- data.frame(Y = ym_vec, Time = seq(1,length(ym_vec)), Months = rep(months,years), Year = seq(1,years), Labels = rep("Simulation", length(ym_vec)))
    
    plot <- ggplot() +
      geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge")
    print(plot)
  }
  return(y_vec)
}


# Function that simulates the forecasted values.

sim_fit <- function(fit,h){
  #lapply(1:5, FUN = FUN = function(x) {sim_fit(fit,h)})
  return(simulate(fit, nsim = h))
}

simulation <- function(N,years, use_days = 1, h = 12, cf = 0.95, lt = 7, of = 7, y = NULL){
  days <- years*360
  period <- 30
  
  # Get daily time series data
  if(length(y) == 0){
    y <- ts_generator(years, lambda = 10)  
  }
  
  
  # Train test split
  
  y_train <- y[1:(days-h*period)]
  y_test <- y[(days-h*period+1):days]
  
  # Fit on the training data
  
  if(use_days == 1){
    
    model <- fit_model(y_train, h*period)
    #Extract features
    demand_p <- model$pred
    sigma2 <- model$sigma2
    
    print(sprintf("Using daily values with MSE: %f",sum((y_test-demand_p)^2)))
    
    fit <- model$fit
    
    # Repeat to get new data. Each row is one simulation and column is the n+1 step.
    
    fit.sim <- t(sapply(1:N, FUN = function(x) {sim_fit(fit,h*period)}))
    fit.sim[fit.sim < 0 ] <-0
  }
  
  # If we choose to have monthly data instead
  
  if(use_days == 0){
    
    
    y_train <- unname(tapply(y_train, (seq_along(y_train)-1) %/% period, sum))  
    y_test <-  unname(tapply(y_test, (seq_along(y_test)-1) %/% period, sum)) # This is never being used
    
    # Fit monthly model
    
    model <- fit_model(y_train, h)
  
    #Extract features
    
    demand_p.m <- model$pred
    
    sigma2 <- model$sigma2/(period^2)
    print(sprintf("Using monthly values with MSE: %f",sum((y_test-demand_p.m)^2)))  
    fit <- model$fit
    # repeat
    fit.sim.m <- t(sapply(1:N, FUN = function(x) {sim_fit(fit,h)}))
    fit.sim.m[fit.sim.m < 0 ] <-0
    
    # Outputs nsim as row with and forecast in column
    fit.sim <- t(apply(fit.sim.m, MARGIN = 1, FUN = function(x){x[rep(seq_along(x), each = period)]}/period))
    
    demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
    
  }

  # Compute z_score
  (alpha <- 1-cf)
  
  (z_score <- 1-alpha/2)

  #print(sprintf("Saftey stock: %f", sqrt(sigma2)*qnorm(1-(1-cf)/2)*sqrt(of+lt)))
  print(sprintf("Saftey stock: %f", sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)))
  
  
  # Compute the errors
  p_error <- apply(fit.sim, MARGIN = 1, FUN = function(x){ demand_calculation(y_test = x, demand_n = demand_p, sigma2, cf, lt, of)})
  #a_error <- apply(fit.sim, MARGIN = 1, FUN = function(x){ demand_calculation(y_test = x, demand_n = y_test, sigma2 = sigma2) })
  # Using the predicted values, we can simulate the ordering period:

  return(p_error)
}
# Output -- One example

# Simulate bunch of Y and compare the service level for some lambda values


gen_Y <- function(years, lambda, N = 2){
  days <- years*360
  YY <- lapply(rep(years,N), FUN = random_ts, years, lambda)
  Y <- matrix(unlist(YY), ncol = N, nrow = days)
  
  return(Y)
}

output <- gen_Y(4, 10, 100)
apply(output, MARGIN = 2, sum)

lt <- 60
of <- 30
View(sapply((0:0.95), FUN = function(x){simulation(N = 10, 0, years = 4, 12, x, 7, 7, y =random_ts(4,4)) }))
test <- simulation(N = 500, use_days =  0 ,years = 4, h = 12, cf = 0, lt = lt, of = of )

test <- simulation(N = 500, use_days =  0 ,years = 4, h = 12, cf = 0.0, lt = lt, of = of,random_ts(4,5/30))
# dim(test)

OP_Service <- apply(test, MARGIN = 2, FUN = function(x){mean(unname(tapply(x, (seq_along(x)-1) %/% (lt+of), mean)))  })
hist(OP_Service)
hist(colMeans(test))
#View(OP_Service)
mean(OP_Service)

## Repeat for confidence factor:

repeat_cf <- function(N, use_days, years, cf, lt, of, lambda){
  #cf <- c(0,0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
  sim <- simulation(N = N, use_days = use_days , years = years, h = 12, cf = cf, lt = lt, of = of, y = random_ts(years,lambda))
  OP_Service <- apply(sim, MARGIN = 2, FUN = function(x){mean(unname(tapply(x, (seq_along(x)-1) %/% (lt+of), mean)))  })
  return(OP_Service)
}
# Run

## Some code in here that we can use:
comment_out <- function(){
  simulation(N = 10, use_days = 0 , years = 4, h = 12, cf = 0.5, lt = 7, of = 7, y = random_ts(4,4))
  repeat_cf(10, 0, 4, 0.5, 7,7,4)
  CF <- c(0,0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
  CF.output <- sapply(CF, FUN = function(x){repeat_cf(1000, use_days = 0, years = 4, x, lt = 7, of = 7,lambda = 5 )})
  
  CF.output <- as.data.frame(CF.output)
  names(CF.output) <- paste(CF)
  head(CF.output)
  names(melt.output)
  #ggplot(melt.output) + geom_histogram(aes(x = value, color = variable))
  #install.packages("reshape2")
  
  melt.output <- melt(CF.output)
  names(melt.output)
  
  ggplot(melt.output, aes(x = `value`,y = `variable`, fill = ..x..)) + 
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, trim = TRUE) +
    
    scale_fill_viridis(name = "Temp. [F]", option = "C") + 
    xlab("Service Level") + 
    ylab("Confidence Factor") +
    labs(title = 'Order period: 14 days') +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  
  ggplot(melt.output, aes(x = `value`,y = `variable`, height = ..density..)) + 
    #geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, trim = TRUE) +
    geom_density_ridges(stat = "density",scale = 2, rel_min_height = 0.01, trim = TRUE) +
    scale_fill_viridis(name = "Temp. [F]", option = "C") + 
    xlab("Service Level") + 
    ylab("Confidence Factor") +
    labs(title = 'Order period: 14 days') +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
  
  (one_sim <- cf_data(4, 10, 12, 7,7))
  colMeans(one_sim)
}
## Look into it

## Use this to plot nice plots
## Here we run each simulation and generate a data frame that we can plot
mult_sim_data_frame <- function(years, N, lambda = 10, h = 12, lt = 7, of  = 7){
  multiple_sim <- t(sapply(rep(years,N), FUN = function(x){colMeans(cf_data(year = x, lambda = lambda, h = h, lt, of))}))
  m.sim <- as.data.frame(multiple_sim)
  names(m.sim) <- paste(c(0,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
  m.df <- melt(m.sim)
  return(m.df)
}
# Call the function
get <- mult_sim_data_frame(4,1000, 10, h = 24, 30, 30)
m.df <- get

# Not in function form: 
# Exactly the same as the function above.
multiple_sim <- t(sapply(rep(4,1000), FUN = function(x){colMeans(cf_data(year = x, lambda = 10, h = 12, 7,7))}))
m.sim <- as.data.frame(multiple_sim)

paste(round(100*m, 2), "%", sep="")

(t.cf <- c(0,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
(alphas <- 1 - c(0,0.2,0.4,0.6,0.8,0.9,0.99))
(zetas <- 1-alphas/2)
(agr.cf <- paste(1-(1-t.cf)/2))
labs <- paste(round(100*zetas, 2), "%", sep="")
names(m.sim) <- paste(labs)
#names(m.sim) <- paste(c(0,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
m.df <- melt(m.sim)
head(m.sim)

names(m.df)
head(m.df)
ggplot(m.df, aes(x = `value`,y = `variable`, height = ..density..)) + 
  #geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, trim = TRUE) +
  #geom_density_ridges(stat = "density", alpha = 0.3,scale = 2, rel_min_height = 0.01, trim = TRUE) +
  geom_density_ridges(stat = "density", alpha = 0.3) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") + 
  xlab("Service Level") + 
  ylab("Confidence Factor") +
  #xlim(0.45, 1)+
  scale_x_continuous(breaks = seq(0.1, 1, 0.1), limits = c(0.45, 1))+
  labs(title = 'Order period: 14 days') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

colMeans(multiple_sim)


ggplot(m.df) + 
  geom_density_ridges(
      aes(x = `value`,y = `variable`, height = ..density..),
      stat = "density", alpha = 0.3,
      quantile_lines = T, quantile_fun = mean
      ) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") + 
  xlab("Service Level") + 
  ylab("Confidence Factor") +
  #xlim(0.45, 1)+
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))+
  labs(title = 'Order period: 60 days') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## Stack overflow nicer plots - Start #
library(tidyverse)
sl_mean <- get %>%
  group_by(variable) %>%
  summarize(mean_value = mean(value))
names(get)
ggplot(get) +
  geom_density_ridges(
    aes(x = `value`,y = `variable`, height = ..density..),
    quantile_lines = T, quantile_fun = mean
  ) +
  geom_text(
    data = sl_mean,
    aes(x = mean_value, y = variable, label = round(mean_value,2)),
    size = 2, nudge_z = 0.03, nudge_y = 0.35
  )
# Stack overflow nicer plots - End #


##

y.ts <- ts(random_ts(3,5/30))
plot(y.ts)
ym.ts <- d_to_m(y.ts)
plot(ts(ym.ts))
ym.ts <- d_to_m(rpois(900, 0.03))
fit <- auto.arima(ym.ts)

f.fit <- forecast(fit, h = 12)
summary(f.fit)
plot(forecast(fit, h = 12))

# ------------------------------ #
# Output with multiple different #
# ------------------------------ #

# Generate daily simulations for some lambda 


get <- random_ts(4, plot = TRUE)

calc_service_level <- function(N, use_days, years, h, cf, lt,of, y = NULL){
  run  <- simulation(N, use_days, years, h, cf, lt ,  of, y )
  sl <- mean(apply(run, MARGIN = 2, FUN = function(x){mean(unname(tapply(x, (seq_along(x)-1) %/% (lt+of), mean)))  }))
  #hist(rowMeans(test_new))
  #mean(rowMeans(test_new))
  return(sl)
}
calc_service_level(10,0,4,12,0.05,7,7)

multiple_simulation <- function(years, lambda = 10) {
  
  n <- years*360
  w <- n/30

  (lambdas <- runif(w,0,lambda))
  
  ts_day(lambdas)
  lambdas
  
}

y <- colSums(sapply(lambdas, FUN = function(x){rpois(30,x)}))

h <- 12
# Without taking saftey stock or any stock into consideration.
cf <- 0.95
cf <- seq(0,1,0.05)

(alpha <- 1-cf)

(z_score <- 1-alpha/2)

plot(cf,qnorm(z_score))

plot_one_example <- function(years, lambda, cf = 0.95, h = 12){
  
  # Initialize some values:
  days <- years*360
  period <- 30
  
  # Generate time series
  y <- ts_generator(years = years, lambda = lambda)
  # Split into train and test
  y_train <- y[1:(days-h*period)]
  y_test <- y[(days-h*period+1):days]
  
  # Get monthly periods.
  ym_train <- unname(tapply(y_train, (seq_along(y_train)-1) %/% period, sum))
  ym_test <- unname(tapply(y_test, (seq_along(y_test)-1) %/% period, sum))
  
  # Fit a model
  fit <- auto.arima(ym_train)
  f <- forecast::forecast(fit, h = h)
  #summary(f)
  #f$model$sigma2
  std_dev <- f$model$sigma2
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  #189.6052 + qnorm(0.975)*sqrt(2891.582)
  
  # Train test split
  
  # Plotting
  # For the actual data:
  (n_train <- length(ym_train))
  (n_test <- length(ym_test))
  labels <- c(rep("Actual",n_train),rep("Test",n_test))
  Y <- append(ym_train,ym_test)
  X <- seq(1,length(Y))
  
  df_plot <- data.frame(Y = Y, Time = X, Labels = labels)
  
  # For the predicted data
  Y_hat <- f$mean
  X_hat <- seq(n_train + 1,n_train + n_test,1)
  Labels_hat <- rep("Prediction", length(Y_hat))
  df_pred <- data.frame(Y = Y_hat, Time = X_hat, Labels = Labels_hat, SafteyStock = Y_hat + qnorm(z_score)*sqrt(std_dev))
  
  
  
  output <- ggplot() +
    geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge")+
    geom_line(data = df_pred, aes(Time, Y, color = Labels)) +
    geom_line(data = df_pred, aes(Time, SafteyStock), linetype = "dashed", alpha = 0.9)
  return(output)
}

plot_one_example(4, 10, cf = 0.5, h = 12)



