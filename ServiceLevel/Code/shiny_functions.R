library(forecast)
library(ggplot2)
library(dplyr)  
library(purrr)
library(reshape2)
library(ggridges)
library(viridis)
library(hrbrthemes)

lost_sale <- TRUE # Global variable
min_stock <- 0 # Gobal variable

# Function Start

# Change daily data to monthly values
to_month <- function(y){
  return( unname(tapply(y, (seq_along(y)-1) %/% 30, sum)))
}
# Change monthly data to daily values
to_days <- function(y, ndays = 30){
  return(y[rep(seq_along(y), each = ndays)]/ndays)
}

## Generate random TS
random_ts <- function(years, lambda = 10, plot = FALSE){
  n <- years*360
  w <- n/30
  lambda <- runif(1,1,100)
  # Uniform lambda values
  (lambdas <- runif(w,0,lambda))
  
  (y_mat <- sapply(lambdas, FUN = function(x){rpois(30,x)}))
  
  y_vec <- c(matrix(y_mat,nrow = 1, byrow = TRUE))
  
  ym_vec <- unname(tapply(y_vec, (seq_along(y_vec)-1) %/% 30, sum))  
  
  if(plot == TRUE){
    
    years <- length(ym_vec)/12
    
    months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec")
    
    df_plot <- data.frame(Y = ym_vec, Time = seq(1,length(ym_vec)), Months = rep(months,years), Year = seq(1,years), Labels = rep("Simulation", length(ym_vec)))
    fill <- c(rep("Train", (years-1)*12),rep("Test", 12))
    df_plot$Labels <- fill
    plot <- ggplot() +
      geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge") +
      theme_ipsum()
    print(plot)
  }
  return(y_vec)
}

# Predict future values for data.
predict_demand <- function(y, h = 12){
  fit <- auto.arima(y)
  f <- forecast::forecast(fit, h = h)
  f_v <- f$mean
  f_sigma2 <- f$model$sigma2
  return(list(pred = f_v, sigma2 = f_sigma2, fit = fit))
}

# Instead of a for loop we can use the following:
demand <- function(df, i, n, ss, lt, of, lost_sale = TRUE) {
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
    
    df$est_stock[i] <-df$est_stock[i-1]- df$demand_a[i] + df$delivered[i]
    if(lost_sale == TRUE){
      df$est_stock[i] <-max(df$est_stock[i],0)     
    }
    #Using Standard: 245.0000[stock] - 740.8296[demand] + 736.0000[undeliv. arrived] - 184.3050[safety stock] - 0.0000[min stock] = 55.8654[result] is greater than 0 so calculated order qty is 0. Order qty: 0
    #if( ( df$est_stock[i] - ss <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {
    if( ( df$est_stock[i] - ss - min_stock <  ( sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i])) )  & ( df$is_order[i] == 1 )  )  {  
      #if( ( df$est_stock[i] <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {  
      df$delivered[lt_i] <- max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]) - df$est_stock[i] + ss),0)
    }
    return(demand(df, i+1, n, ss, lt, of, lost_sale))
  }
  else {
    return(df)
  }
}

## Pre lt, of, cf

demand_data_pre <- function(demand_a, demand_p){
  
  n <- length(demand_p)
  days <- seq(1,n,1)
  delivered <- rep(0,n)
  est_stock <- rep(0,n)
  acc_est_stock <- rep(0,n)
  is_order <- rep(0,n) 
  is_delivery <- rep(0,n)
  
  data <- data.frame(demand_a = demand_a, demand_p = demand_p, est_stock, acc_est_stock, delivered, is_order, is_delivery)
  return(data)
}

# got <- lapply(xx, FUN = function(x){demand_data_pre(to_days(SIM$Actual[,x]), to_days(SIM$Predicted[[x]]$pred))})
# xx <- seq(1,length(SIM$Predicted))

## Helper Function - Gen Demand Data ##

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

# Calculate the service level with cf.

cf_sl <- function(demand_a,demand_p, ss, st, lt, of, lost_sale = TRUE){
  n <- length(demand_a)
  
  df <- gen_demand_data(demand_a, demand_p, ss,st, lt, of)
  
  output <- demand(df,2, n, ss, lt, of)
  output$ss_ratio <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss = ss,st = x)})
  
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
 
  output$sl <- sl
  return(output)
  #return(list(ServiceLevel = sl, output$est_stock))
}

# Calculate service level
calc_servicelevel <- function(demand_a,demand_p, ss, lt, of){
  
  n <- length(demand_a)
  st <- sum(demand_p[1:lt]) # Initalize starting stock
  df <- gen_demand_data(demand_a, demand_p, ss,st, lt, of)
  #print("gen demand data success")
  
  output <- demand(df,2, n, ss, lt, of, lost_sale = FALSE)
  #print("demand success")
  output$ss_ratio <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss = ss,st = x)})
  
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
  output$sl <- sl
  ss.r <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss, st)})
  #return(output)

  return(list(ServiceLevel = mean(sl), SS_r = mean(ss.r)))
}

# Calculate safetystock
calc_safetystock <- function(cf, lt, of, sigma2){
  #cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  # Initalize saftey stock
 
  ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
  return(ss)
}

## Preparation ##
cf_data <- function(year, lambda, h, lt, of, lost_sale = TRUE){
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
  
  
  output <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of, lost_sale = TRUE)})
  return(output)
}

## Safety stock ratio

ss_ratio <- function(ss, st){
  # If saftey stock is 0 then we do not need anything
  if(ss == 0){
    return(0)
  }
  if(ss >= st){
    return(1)
  }
  else {
    return(ss/st)
  }
}

## function that runs a simulation and uses the values as global values 

sim <- function(years,N = 1000, rerun = FALSE){
  # If global variables exist then we should not simulate again.
  
  if( exists("SIM") == FALSE && rerun == FALSE) {
    s.df <- sapply(rep(years,N), FUN = random_ts)
    sim.df <- apply(s.df, MARGIN = 2, FUN = to_month)
    #dim(om.test)
    # split 
    s1 <- 12*years
    s2 <- 12*(years - 1)
    split <- c(1:(s1-s2))
    train <- sim.df[split,]
    test <- sim.df[-split,]
    # Calculate predictions
    sim.pred <- apply(train, 2, FUN = predict_demand)
    SIM <<- list(Original = sim.df, Actual = test, Predicted = sim.pred)
  }
  return(SIM)
}


ss_sl_sim <- function(year,lambda, h, lt, of, lost_sale = TRUE){
  sim <- cf_data(year,lambda,h, lt, of, lost_sale = TRUE)
  tt <- ncol(sim)
  SS <- sapply(1:tt, FUN= function(x){mean(sim[,x]$ss_ratio)})
  SL <- sapply(1:tt, FUN= function(x){mean(sim[,x]$sl)})
  #return(cbind(SS,SL))
  return(list(SS = SS, SL = SL))
}

# Run the simulation

SIM <- sim(4, N = 1000 , rerun = FALSE)

calc <- lapply(xx, FUN = function(x){calc_servicelevel(demand_a = to_days(train[,x]),
                                                       demand_p = to_days(pd[[x]]$pred), 
                                                       ss = calc_safetystock( cf = cf, 
                                                                              lt = lt, 
                                                                              of = of,
                                                                              sigma2 = pd[[x]]$sigma2/30^2), 
                                                       lt = lt, 
                                                       of = lt)
})

## Trying to debug this
run_sim <- function(years, cf, lt, of, N = 1000, rerun = FALSE ){
  SIM <- sim(years, N, rerun)
  train <- SIM$Actual
  pd <- SIM$Predicted
  xx <- seq(1,length(pd))
  label <- paste(round(100*cf, 2), "%", sep="")
  calc <- lapply(xx, FUN = function(x){calc_servicelevel(demand_a = to_days(train[,x]),
                                                         demand_p = to_days(pd[[x]]$pred), 
                                                         ss = calc_safetystock( cf = cf, 
                                                                                lt = lt, 
                                                                                of = of,
                                                                                sigma2 = pd[[x]]$sigma2/30^2), 
                                                         lt = lt, 
                                                         of = lt)
  })
  res <- as.data.frame(do.call(rbind, calc))
  sl.df <- as.data.frame(unlist(res$ServiceLevel))
  names(sl.df) <- label
  return(sl.df)
}
CF <-  c(0.5,0.6,0.7,0.8,0.9,0.95,0.995)

start_time <- Sys.time()
all <- sapply(CF, FUN = function(x){
  run_sim(years = 4, cf = x, lt = 7, of = 7, N = 1000, rerun = FALSE)
})
end_time <- Sys.time()

got <- t(do.call(rbind, all))
colMeans(got)
dim(got)
