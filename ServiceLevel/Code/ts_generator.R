## Generate random time series ##
## Randomise seasonality, trend & lambda parameter ##
y <- 360
library(ggplot2)
trend <- ts(seq(from = 10, to = 110))
cycle <- ts(sin(trend)) * 0.2 * trend
tseries_h <- trend + cycle
plot.ts(tseries_h)

ts_generate <- function(yr, plot = FALSE){
  d <- 360 # days
  s <- round(runif(1)) # seasonality
  t <- round(runif(1)) # trend
  l <- runif(1,1,1000) # Monthly lambda
  c <- round(runif(1))
  s.l <- runif(1)*(l/30)
  t.l <- runif(1)*(l/30)
  y <- rpois(d*yr, l/30) + s*s.l*rep(sin(pi*seq(0,1,length.out = d)), yr) + (1-s)*s.l*rep(rep(round(1),d),yr)+ t*t.l*seq(1,d*yr)/30^2
  #if(c == 1){
  y <- y*round(runif(d*yr,0,1))  
  #}
  
  if(plot == TRUE){
    
    ym <- unname(tapply(y, (seq_along(y)-1) %/% 30, sum)) 
    l.n <- length(ym)
    
    months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec")
    title <- sprintf("l = %#.2f, s = %#.2f and t = %#.2f", round(l,2), round(s*s.l,2), round(t*t.l,2))
    #print(title)
    df_plot <- data.frame(Y = ym, Time = seq(1,l.n), Months = rep(months,yr), Year = seq(1,yr), Labels = rep("Simulation", l.n))
    fill <- c(rep("Train", (yr-1)*12),rep("Test", 12))
    df_plot$Labels <- fill
    plot <- ggplot() +
      geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge") +
      theme_ipsum() +
      labs(title = title)
    print(plot)
  }
  return(y)
}
## One output for the ts_generate()
y.test <- ts_generate(4, plot = TRUE)

## Many outsput for ts_generate()
o.test <- sapply(rep(4,1000), FUN = ts_generate)
o.test <- sapply(rep(4,1000), FUN = random_ts)

to_month <- function(y){
  return( unname(tapply(y, (seq_along(y)-1) %/% 30, sum)))
}

to_days <- function(y, ndays = 30){
  return(y[rep(seq_along(y), each = ndays)]/ndays)
}

om.test <- apply(o.test, MARGIN = 2, FUN = to_month)
dim(om.test)
split <- c(1:(48-12))
train <- om.test[split,]
test <- om.test[-split,]
## Predict Demand

predict_demand <- function(y, h = 12){
  fit <- auto.arima(y)
  f <- forecast::forecast(fit, h = h)
  f_v <- f$mean
  f_sigma2 <- f$model$sigma2
  return(list(pred = f_v, sigma2 = f_sigma2, fit = fit))
}

##
train1 <- train[,1]
test1 <- test[,1]
test_d <- to_days(test1)
pd <- apply(train, 2, FUN = predict_demand)
length(pd)

pd[[1]]

check <- function(object){
  if(exists(object) == TRUE){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
check("df")


start_time <- Sys.time()
output <- sim(4,1000,rerun = TRUE)
end_time <- Sys.time()
## Calc service sevel

output$Actual

calc_servicelevel <- function(demand_a,demand_p, ss, lt, of){
  n <- length(demand_a)
  st <- sum(demand_p[1:lt]) # Initalize starting stock
  df <- gen_demand_data(demand_a, demand_p, ss,st, lt, of)
  
  output <- demand(df,2, n, ss, lt, of)
  output$ss_ratio <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss = ss,st = x)})
  
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
  output$sl <- sl
  ss.r <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss, st)})
  #return(output)
  return(list(ServiceLevel = mean(sl), SS_r = mean(ss.r)))
}
calc_safetystock <- function(cf, lt, of, sigma2){
  #cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  # Initalize saftey stock
  
  ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
  return(ss)
}
##
pred <- pd[[1]]$pred %>% to_days
act <-  test[,1] %>% to_days

xx <- seq(1,length(pd))



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
SIM <- sim(4, N = 1000 , rerun = FALSE)

train <- SIM$Actual
pd <- SIM$Predicted
xx <- SIM(pd)

lt <- 7
of <- 7
cf <- 0.5
to_days(train[,3])
to_days(pd[[1]]$pred)
calc <- lapply(xx, FUN = function(x){calc_servicelevel(demand_a = to_days(train[,x]),
                                                       demand_p = to_days(pd[[x]]$pred), 
                                                       ss = calc_safetystock( cf = cf, 
                                                                              lt = lt, 
                                                                              of = of,
                                                                              sigma2 = pd[[x]]$sigma2/30^2), 
                                                       lt = lt, 
                                                       of = lt)
  })
run_sim <- function(years, cf, lt, of, N = 1000, rerun = FALSE ){
  output <- sim(years, N, rerun)
  train <- output$Actual
  pd <- output$Predicted
  xx <- length(pd)
  
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
  return(res)
}
try_run <- run_sim(4,0.6, 7,7,1000)

cac[[1]]$ServiceLevel

lapply(xx, FUN = function(x){mean(cac[[x]])})
# Check the output.
res <- as.data.frame(do.call(rbind, cac))
colMeans(res)
hist(unlist(res$ServiceLevel))
res
mean(unlist(res$ServiceLevel))



## -------------------------------------------------------------------------------- ##
## Make a function that will test a few forecast methods and select the best one.
## -------------------------------------------------------------------------------- ##




check_forecast <- function(y, h){
  yy <- ts(y[y > 0])
  # Normal auto.arima
  y_arima <- auto.arima(y)
  # Exponential SMoothing
  y_ses <- ses(y, h = 12)
  #y_ses$model$aic  
  lambda <- BoxCox.lambda(yy)
  y_BC <- BoxCox(yy, lambda)
  BoxCox.arima <- auto.arima(y_BC)  
  ## y_crost <- croston(yy)
}

y <- train[,501]
y_crost <- croston(y)
y_arima$aic
plot(y_crost)
summary(forecast(y_crost))

y_arima <- auto.arima(y)
y_ses <- ses(y, h = 12, alpha = 0.2)

## Exponential smoothing
y_ses$model$aic

f_arima <- forecast::forecast(y_arima)

f_arima$model$aic

summary(f_arima)
f_crost
summary(f_crost)
f_crost <- forecast::forecast(y_crost)
plot(f_arima)
plot(f_crost)
lambda <- BoxCox.lambda(y[y > 0])
lambda
y_BC <- BoxCox(y, lambda)
y_BC
plot(ts(y_BC))
