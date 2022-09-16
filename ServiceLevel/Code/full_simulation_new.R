
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
  
  return(list(daily = ts(yv), monthly = ts(y_m)))
  
}



sim_iteration <- function(y_test,demand_n, sigma2, cf = 0.95, lt = 7, of = 7){
  
  period <- 30
  
  # Demands
  demand_a <- y_test
  #demand_n <- daily_demand(f_v)
  
  #Simulate the results
  
  n <- length(y_test)
  
  ss <- sqrt(sigma2)*qnorm(1-alpha/2)*sqrt((of+lt)/30)
  st <- ss + sum(demand_n[1:lt])
  #print(ss)
  # Generate the data famre
  sim_df <- gen_demand_data(demand_a, demand_n, ss, ss, of, lt)
  
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
    
    #Calculate est_stock
    sim_df$acc_est_stock[i] <- sim_df$est_stock[i-1] - sim_df$demand_a[i] + sim_df$delivered[i]
    sim_df$est_stock[i] <- max(sim_df$est_stock[i-1] - sim_df$demand_a[i] + sim_df$delivered[i],0) # Ignore negatives
    
    if(sim_df$est_stock[i]  < sum(sim_df$demand_p[ii:op_i]) - sum(sim_df$delivered[ii:op_i]) & sim_df$is_order[i] == 1){

      sim_df$delivered[lt_i] <- max(ceiling(sum(sim_df$demand_p[lt_i:op_i])-sum(sim_df$delivered[lt_i:op_i]) + ss),0) ## Cover lead time or order period?
      
    }
    
    sl[i] <- (sim_df$est_stock[i] > 0)
  }
  
  #actual_vs_pred <- data.frame(Actual = y_test, Predicted = demand_n)
  #ls <- list(Data = sim_df, ServiceLevel = sl, OrderFrequency = of, LeadTime = lt, ConfidenceFactor = cf, SafteyStock = ss)
  return(sl)
}


repeat_sim <- function(nsim, years, lambda) {
  
  period <- 30
  days <- years*360
  h <- 6
  
  CF <- c(seq(0.5,0.9,0.1),0.95,0.99)
  lt_s <- seq(1,14,1)
  of_s <- c(1,7,14)
  
  # Define a named list of parameter values
  gs <- list( LT = lt_s,    OF = of_s, CF = CF)
  
  tuning_param <- gs %>% cross_df()
  run_sim <- matrix(nrow = nsim, ncol = 4)
  output <- matrix(nrow = nrow(tuning_param), ncol = nsim)

  for(i in 1:nsim){
      
    if(i %% 10 == 0) {
      print(sprintf("Iteration number %i starting", i))  
    }
    
    y_dat <- ts_generator(years,lambda)
    y <- y_dat$daily
    # Extract y
    v <- ts(y)
    # Generate month data
    
    y_m <- unname(tapply(v, (seq_along(v)-1) %/% period, sum))
    # Train and test 
    y_train <- y[1:(days-h*period)]
    y_test <- y[(days-h*period+1):days]
    
    # Monthly train and test
    y_m_train <- y_m[1:(length(y_m)-h)]
    y_m_test <- y_m[(length(y_m)-h+1):length(y_m)]
    
    # Forecast future values
    
    fit.m <- auto.arima(y_m_train)
    f_m <- forecast::forecast(fit.m, h = h)
    f_v <- f_m$mean
    f_sigma2 <- f_m$model$sigma2
    demand_a <- y_test
    demand_n <- daily_demand(f_v)
    
    
    for(j in 1: nrow(tuning_param)){

      temp_run <- sim_iteration(demand_a,demand_n, sigma2 = f_sigma2, cf = tuning_param$CF[j], lt = tuning_param$LT[j], of = tuning_param$OF[j])
      output[j,i] <- sum(temp_run)/length(temp_run)
      
    }
  }
  return(output)
}

test <- repeat_sim(100,3,4)
tuning_param$CF[1]

output <- rowMeans(test)

sim_result <- matrix(nrow = length(output), ncol = 4)

for(i in 1:nrow(sim_result)){
  sim_result[i,1] <- output[i]
  sim_result[i,2] <- tuning_param$CF[i]
  sim_result[i,3] <- tuning_param$LT[i]
  sim_result[i,4] <- tuning_param$OF[i]
}

simmmm <- data.frame(sl = sim_result[,1], cf = sim_result[,2], lt = sim_result[,3], of = sim_result[,4])
simmmm

s_gen <- function(x){
  (x/100)*12
}


l_gen <- function(x){
  return(rpois(1,x))
}

labmdas <- 

sapply(x, FUN = l_gen)


rpois(1,c(1,2,3,4,5))

qpois(0.95, 5)

samples <- 100000

l <- 50
Y <- rpois(samples, l)
Z <- (Y - mean(Y))/sqrt(mean(Y))

hist(Z, breaks = 30, probability =  T)
lines(density(rnorm(samples, 0,1)))
