
# Some functions below with repeating simulations.

sl <- demand_calculation(st, d = 30, d_std = 2.15, n = 30, lt = 7, of = 7, cf = 0.5)$est_stock

sl_score <- function(service_level, op){
  n <- length(service_level)
  periods <- floor(n/op)
  score <- unname(tapply(service_level, (seq_along(service_level)-1) %/% periods, min))
  return(score)
}

sl_score(sl, 14)

repeat_sim <- function(d, d_std, n, nsim = 100, lt = 7, of = 7, cf = 0.95){
  
  st <- ceiling((d/n)*lt)
  service_level <- rep(0,nsim)
  
  for(runs in 1:nsim){
    est_stock <- demand_calculation(st, d, d_std, n, lt, of, cf)$est_stock
    service_level[runs] <- length(est_stock[est_stock < 0])
  }
  return(service_level)
}

1-sum(repeat_sim(30, 2.15, 30, 100, cf = 0.95))/(100*30)
# Test the function.

# We have perdiction for some demand, e.g. 22 for the month => daily demand = 22/30

simulate_service_level <- function(d, d_std, n, nsim = 100, cf = 0.95, lt = 7, of = 7){
  st <- ceiling((d/n)*lt)
  ## Here we could simulate time series where we can get new data.
  df_compare <- demand_calculation(st, d = d, d_std = d_std, n = n, lt = lt, of = of, cf = cf)
  
  p_plan <- df_compare$purchase_plan
  service_level <- rep(0,nsim)
  
  for(runs in 1: nsim){
    sim_est_stock <- rep(0,n)
    sim_est_stock[1] <- st - rpois(1, d/n) + p_plan[1]
    
    for(i in 2:n){
      sim_est_stock[i] <- sim_est_stock[i-1] - rpois(1, d/n) + p_plan[i]
      
    }
    service_level[runs] <- length(sim_est_stock[sim_est_stock < 0])
    
  }
  
  return(service_level)
}
d <- 59.59067
d_std <- 2.158175
sum(test1 <- simulate_service_level(d = d, d_std = d_std, n =  30, 1000, cf = 0.99))

1-sum(test1)/(1000*30)
sum(rpois(30, d/30))
