saftey_stock <- function(std_d, cf, op = 14) {
  
  days_in_period <- 30 #Assume 1 month every time.
  z_factor <- qnorm(cf)
  ss_low <- z_factor*std_d*sqrt(floor(op/days_in_period))
  ss_high <- z_factor*std_d*sqrt(ceiling(op/days_in_period))
  
  ss <- ss_low + ((op %% days_in_period)/days_in_period)*ss_high
  
  # Direct method.
  #ss <- qnorm(cf)*std_d*sqrt(op)
  return(ss)
}



negative_to_zero <- function(x){
  if(x < 0 ) {
    return(0)
  }
  else {
    return(x)
  }
}

demand_calculation <- function( d, d_std, n, lt = 7, of = 7, cf = 0.95){
  # Initialize stocks:
  lambda <- d/min(n,30)
  lambda <- rnorm(1,d,sqrt(d))/30
  st <- 0#ceiling((d/min(n,30))*lt)
  delivered <- rep(0,n)
  est_stock <- rep(0,n)
  est_stock[1] <- st

  
  ss <- saftey_stock(d_std, cf, op = lt + of)
  
  #Initialize demand
  demand_n <- rep(d/min(n,30), n) # Since we are considering daily demand then we need to divide by 30
  demand_total <- cumsum(demand_n)
  demand_op <- d*(lt+of)/n
  # Check if we need to order
  need_order <- rep(0,n) 
  order_days <- (seq(1,floor(n/of))*of) - of#(of - 1)
  need_order[order_days] <- 1
  
  
  # Service Level
  sl <- rep(1,n)
  
  op <- (lt+of)
  # initalize all values
  est_stock[1] <- est_stock[1] - demand_n[1]
  acc_est_stock <- est_stock
  # If the estimated stock does not cover the OP then order the demand for the LT.
  if( est_stock[1]  < sum(demand_n[2:op]) - sum(delivered[2:op]) ){
    #delivered[lt] <- ceiling(lt*(lambda) + ss*lt/(op))
    delivered[lt] <- ceiling(lt*(lambda) + ss)
    #delivered[lt] <- ceiling(op*(lambda) + ss)
  }
  # n days into the future
  for(i in 2:n){ 
    ii <- min(i+1,n)
    t_i <- min(i+lt+of, n)
      
    of_i <- min(i+of, n)
    lt_i <- min(i+lt, n)
    
    op_i <- min(i+op, n)
    # Simulate the demand
    demand_n[i] <- rpois(1, lambda)
    #Calculate est_stock
    acc_est_stock[i] <- est_stock[i-1] - demand_n[i] + delivered[i]
    est_stock[i] <- max(est_stock[i-1] - demand_n[i] + delivered[i],0) # Ignore negatives
    
    if(est_stock[i]  < sum(demand_n[ii:t_i]) - sum(delivered[ii:t_i]) & need_order[i] == 1){
      #delivered[lt_i] <- max(ceiling(sum(demand_n[i:lt_i])-sum(delivered[i:lt_i]) + ss),0) ## Cover lead time or order period?
      delivered[lt_i] <- max(ceiling(sum(demand_n[ii:lt_i])-sum(delivered[ii:lt_i]) + ss),0) ## Cover lead time or order period?
      #delivered[lt_i] <- max(ceiling(sum(demand_n[i:op_i])-sum(delivered[i:op_i]) + ss),0) ## Cover lead time or order period?
    }
    
    sl[i] <- (est_stock[i] > 0)
  }
  df <- data.frame(
    est_stock = est_stock,
    acc_est_stock = acc_est_stock,
    daily_demand = demand_n, 
    demand_total = cumsum(demand_n), 
    purchase_plan = delivered,
    service_level = sl
                   )
  return(df)
}


# Function:
# Service level score given some period.

service_level_score <- function(x, p){
  #sls <- unname(tapply(x, (seq_along(x)-1) %/% (p), min))
  sls <- unname(tapply(x, (seq_along(x)-1) %/% (p), sum))
  #sls <- unname(tapply(x, (seq_along(x)-1) %/% (p), sum))/p
  return(sls)
}
demand_calculation( d = 30, d_std = 5, n = 42, lt = 7, of = 7, cf = 0.5)

repeat_sim <- function(d, d_std, periods, nsim, lt, of, cf){
  n <- (lt+of)*periods
  p <- ceiling(n/(lt+of)) # number of periods we want to check
  sl_mat <- matrix(nrow = nsim, ncol = p)
  #print(p)
  #print(dim(sl_mat))
  for(i in 1:nsim){
    sl <- demand_calculation( d = d, d_std = d_std, n = n, lt, of, cf )$service_level
    #print(service_level_score( sl, p = p))
    sl_mat[i,] <- service_level_score(sl, p = lt+of)
  }
  return(sl_mat[,-c(1,ncol(sl_mat))])
}

### Call the functions:


sim <- demand_calculation( d = 30, d_std = 5, n = 56, lt = 7, of = 7, cf = 0.95)
sim
sl <- sim$service_level
sl
service_level_score(sl,14)

d <- 100
d_std <- d*0.1

lt <- 0
of <- 1

test <- repeat_sim(d,d_std, 100, 100, lt, of, 0.95)
test
mean(rowMeans(test/(lt+of)))

rowSums(test)/(lt+of)*2



#sl <- demand_calculation( d = 30, d_std = 21.5, n = 28, lt = 7, of = 7, cf = 0.95 )$service_level

#service_level_score( sl, p = 7)


unname(tapply(sl, (seq_along(sl)-1) %/% (7), min))

output <- repeat_sim(d = 30, d_std = 30/10, periods = 4, nsim = 1000, lt = 7, of = 7, cf = 0.99)
head(output)
#rowMeans(output)
mean(rowMeans(output))
colMeans(output)
## Fix order frequency and check the lead times 



plot_data_lt <- function(d, d_std, lt, of = 7){
  
  # Confidence factor
  cfs <- append(seq(0.5,0.95, 0.05),0.99)
  # Service Level
  sls <- rep(0, length(cfs))
  lts <- rep(lt, length(cfs))
  for(i in 1:length(cfs)){
    output <- repeat_sim(d = d, d_std = d_std, periods = 8, nsim = 1000, lt = lt, of = 7, cf = cfs[i])
    sls[i] <- mean(rowMeans(output))
    
  }
  
  dat <- data.frame(ConfidenceFactor = cfs, ServiceLevel = sls, LeadTime = lts)
  return(dat)
}
d <- 30
#ratio <- c(seq(0.01,0.1,0.01),seq(0.2,1,0.4))

lts <- c(seq(1,14,1),c(21,28),c(90,180))

data_lt <- data.frame()

for(i in 1:length(lts)){
  temp <- plot_data_lt(d, d/10, lts[i])
  data_lt <- rbind(data_lt, temp)
}

library(ggplot2)
ggplot(data=data_lt, aes(x = ConfidenceFactor, y = ServiceLevel,  group=LeadTime, colour = as.factor(LeadTime))) +
  geom_line() +
  geom_point()

## Plot w.r.t the demand
plot_data <- function(d, d_std, ratio){
  
  # Confidence factor
  cfs <- append(seq(0.5,0.95, 0.05),0.99)
  # Service Level
  sls <- rep(0, length(cfs))
  # Demand ratio
  d_ratio <- rep(ratio, length(cfs))  
  
  for(i in 1:length(cfs)){
    output <- repeat_sim(d = d, d_std = d_std, periods = 4, nsim = 1000, lt = 30, of = 14, cf = cfs[i])
    sls[i] <- mean(rowMeans(output))

  }
    
  dat <- data.frame(ConfidenceFactor = cfs, ServiceLevel = sls, ratio = d_ratio)
  return(dat)
}
d <- 30
#ratio <- c(seq(0.01,0.1,0.01),seq(0.2,1,0.4))
ratio <- c(seq(0.01,0.1,0.01))
d_stds <- d*ratio

data <- data.frame()

for(i in 1:length(d_stds)){
  temp <- plot_data(d, d_stds[i], d_stds[i]/d)
  data <- rbind(data, temp)
}

head(data)
library(ggplot2)
ggplot(data=data, aes(x = ConfidenceFactor, y = ServiceLevel,  group=ratio, colour = as.factor(ratio))) +
  geom_line() +
  geom_point()

#setwd("~/AGR Dynamics/ServiceLevel/Results")
write.csv(data, file = "~/AGR Dynamics/ServiceLevel/Results/output.csv")

library(ggplot2)
ggplot(data=data, aes(x = ConfidenceFactor, y = ServiceLevel,  group=ratio, colour = as.factor(ratio))) +
  geom_line() +
  geom_point()

