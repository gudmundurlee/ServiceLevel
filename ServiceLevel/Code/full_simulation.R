library(dplyr)  
library(purrr)
library(ggplot2)

saftey_stock <- function(std_d, cf, op = 14) {

  alpha <- 1-cf
  p <- 1-alpha/2
  
  days_in_period <- 30 #Assume 1 month every time.
  z_factor <- qnorm(p)
  ss_low <- z_factor*std_d*sqrt(floor(op/days_in_period))
  ss_high <- z_factor*std_d*sqrt(ceiling(op/days_in_period))
  
  ss <- ss_low + ((op %% days_in_period)/days_in_period)*ss_high
  
  # Direct method.
  #ss <- qnorm(p)*std_d*op
  return(ss)
}


daily_demand <- function(fv){
  dd <- c()
  for(i in 1:length(fv)){
    dd <- append((rep(fv[i]/30,30)),dd)
  }
  return(dd)
}

gen_demand_data <- function(demand_a, demand_p, ss, st, of, lt){
  
  n <- length(demand_p)
  days <- seq(1,n,1)
  
  delivered <- rep(0,n)
  
  est_stock <- rep(0,n)
  est_stock[1] <- sum(demand_p[1:lt]) + st
  # Initalize 
  
  is_order <- rep(0,n) 
  is_delivery <- rep(0,n)
  
  order_days <- days[days %% of == 0] - (of - 1) #(seq(1,floor(n/of))*of) - (of - 1)
  delivered_days <- days[days %% (of+lt) == 0] - (of -1) #(seq(1,floor(n/of))*of) - (of-1) + lt
  
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
    delivered[lt] <- ceiling(ss) #ceiling(lt*(lambda) + ss)
  }

  data <- data.frame(demand_a = demand_a, demand_p = demand_p, est_stock, acc_est_stock, delivered, is_order, is_delivery)
  
  return(data)
}


gen_data <- function(days, l, period, h = 6, cf = 0.95, lt = 7, of = 7){
 
  period <- 30
  # Daily time series
  time <- seq(1,days,1)
  season <- cos(time/period)
  y <- rpois(days, lambda = l)*abs(season)*cumsum(runif(days)/days)
  df <- data.frame(Time = time, sale = y, day = time %% (period + 1))
  
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
  #print(f_sigma2)
  # Data frame of forecasts
  df_f <- data.frame(Forecast = f_v, Sigma = rep(sqrt(f_sigma2/period),h))
  # Demands
  demand_a <- y_test
  demand_n <- daily_demand(f_v)
  
  #Simulate the results
  
  n <- length(y_test)
  
  # We need to figure out how to determine this qty
  #d_std <- sqrt(f_sigma2/30)*(of+lt)
  #d_std <-((of+lt)*sqrt(f_sigma2)/30)
  d_std <- sqrt(f_sigma2/30)
  #d_std <- sqrt(f_sigma2)/30
  
  ss <- saftey_stock(d_std, cf, op = lt + of)
  alpha <- 1-cf
  ss <- sqrt(f_sigma2)*qnorm(1-alpha/2)*sqrt((of+lt)/30)
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
      #print(i)
      #sim_df$delivered[lt_i] <- max(ceiling(sum(sim_df$demand_p[ii:op_i])-sum(sim_df$delivered[ii:op_i]) + ss),0) ## Cover lead time or order period?
      sim_df$delivered[lt_i] <- max(ceiling(sum(sim_df$demand_p[lt_i:op_i])-sum(sim_df$delivered[lt_i:op_i]) + ss),0) ## Cover lead time or order period?

    }
    
    sl[i] <- (sim_df$est_stock[i] > 0)
  }
  
  
  actual_vs_pred <- data.frame(Actual = y_test, Predicted = demand_n)
  ls <- list(Data = sim_df, ServiceLevel = sl, OrderFrequency = of, LeadTime = lt, ConfidenceFactor = cf, SafteyStock = ss)
  return(ls)
}

run <- gen_data(900,4, 30, 6, 0.6, 1,7)
#run
#sum(run$ServiceLevel[-c(1:2)])/length(run$ServiceLevel[-c(1:2)])

main <- function(){
  (CF <- c(seq(0.5,0.9,0.1),0.95,0.99))
  
  lt_s <- seq(1,14,1)
  of_s <- c(1,7,14)#seq(1,14,1)
  
  # Define a named list of parameter values
  gs <- list( LT = lt_s,    OF = of_s, CF = CF)
  
  tuning_param <- gs %>% cross_df()
  tuning_param
  
  nsim <- nrow(tuning_param)
  t <- 100
  run_sim <- matrix(nrow = nsim, ncol = 4)
  
  for(i in 1:nrow(tuning_param)){
    output <- rep(0,t)
    for(j in 1:t){
      temp_run <- gen_data(900, 4, 30, 6, cf = tuning_param$CF[i], lt = tuning_param$LT[i], of = tuning_param$OF[i])
      output[j] <- sum(temp_run$ServiceLevel)/length(temp_run$ServiceLevel)
    }
    run_sim[i,1] <- mean(output)
    run_sim[i,2] <- tuning_param$CF[i]
    run_sim[i,3] <- tuning_param$LT[i]
    run_sim[i,4] <- tuning_param$OF[i]
  }
  
  simulation <- data.frame(ServiceLevel = run_sim[,1], ConfidenceFactor = run_sim[,2], LeadTime = run_sim[,3], OrderFrequency = run_sim[,4])  
  return(simulation)    
}
simulation <- main()

write.csv(simulation, file = "~/AGR Dynamics/ServiceLevel/Results/simulation.csv")
simulation
ggplot(data = simulation) + 
  geom_line(aes(x = LeadTime, y = ServiceLevel, col = as.factor(OrderFrequency))) + 
  facet_wrap(vars(ConfidenceFactor))

# Short op = 1:14
# Medium op = 15-30
# Long op



#fc <- predict(fit, n.ahead = 6, se.fit = TRUE)
#fc$pred
#fc$se

