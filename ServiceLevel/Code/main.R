library(forecast)
library(ggplot2)
library(dplyr)  
library(purrr)
library(reshape2)
library(ggridges)
library(viridis)
library(hrbrthemes)

## Demand Function ##


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
    #if( ( df$est_stock[i] - ss <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {
    if( ( df$est_stock[i] - ss <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i])))  & ( df$is_order[i] == 1 )  )  {  
      #if( ( df$est_stock[i] <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {  
      df$delivered[lt_i] <- max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]) - df$est_stock[i] + ss),0)
    }
    return(demand(df, i+1, n, ss, lt, of))
  }
  else {
    return(df)
  }
}

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

cf_sl <- function(demand_a,demand_p, ss, st, lt, of){
  n <- length(demand_a)
  
  df <- gen_demand_data(demand_a, demand_p, ss,st, lt, of)
  
  output <- demand(df,2, n, ss, lt, of)
  output$ss_ratio <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss = ss,st = x)})
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
  return(sl)
  #return(list(ServiceLevel = sl, output$est_stock))
}

## Helper functions ##
## Random TS ##
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
    fill <- c(rep("Train", (years-1)*12),rep("Test", 12))
    df_plot$Labels <- fill
    plot <- ggplot() +
      geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge") +
      theme_ipsum()
    print(plot)
  }
  return(y_vec)
}

# Fit models
fit_model <- function(y, h){
  fit <- auto.arima(y)
  f <- forecast::forecast(fit, h = h)
  f_v <- f$mean
  f_sigma2 <- f$model$sigma2
  return(list(pred = f_v, sigma2 = f_sigma2, fit = fit))
}

## Preparation ##
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



## Run simulations

mult_sim_data_frame <- function(years, N, lambda = 10, h = 12, lt = 7, of  = 7){
  multiple_sim <- t(sapply(rep(years,N), FUN = function(x){colMeans(cf_data(year = x, lambda = lambda, h = h, lt, of))}))
  m.sim <- as.data.frame(multiple_sim)
  #names(m.sim) <- paste(c(0.5,0.6,0.7,0.8,0.9,0.95,0.995))
  #m.df <- melt(m.sim)
  return(m.sim)
}

#m.sim <- mult_sim_data_frame(4,1000, 10, h = 24, 7, 7)

sim60 <- mult_sim_data_frame(4,1000, 10, h = 24, 30, 30)

sim14 <- mult_sim_data_frame(4,1000, 10, h = 24, 7, 7)
sim14_lost <- mult_sim_data_frame(4,1000, 10, h = 24, 7, 7)

sim2 <- mult_sim_data_frame(4,1000, 10, h = 12, 1, 1)

sim_7_7 <- mult_sim_data_frame(4,1000, 10, h = 12, 7, 7)
sim_1_13 <- mult_sim_data_frame(4,1000, 10, h = 12, 2, 12)
sim_13_1 <- mult_sim_data_frame(4,1000, 10, h = 12, 12, 2)

# Table

get_table <- function(x){
  v <- signif(colMeans(x), digits = 3)
  return(v)
}

#signif(colMeans(sim2), digits = 3)

meanTable <- as.data.frame(cbind(get_table(sim2), get_table(sim14), get_table(sim60)))
names(meanTable) <- c("Short", "Medium", "Long")

meanTable2 <- as.data.frame( cbind(get_table(sim14), get_table(sim14_lost)))
names(meanTable2) <- c("No Lost Sale", "Lost Sale" )
kable(meanTable2)
meanTable <- as.data.frame(cbind(get_table(sim_1_13), get_table(sim_7_7), get_table(sim_13_1)))
names(meanTable) <- c("LT = 2, OF = 12", "LT = 7, OF = 7", "LT = 12, OF = 2")



library(knitr)
kable(meanTable)

# Modify

# sim2 <- get_1_1$ServiceLevel
# sim14 <- get_7_7$ServiceLevel
# sim14_lost <- get_7_7_lost$ServiceLevel
# sim60 <- get_1_1$ServiceLevel

## Plots
zeta <- c(0.5,0.6,0.7,0.8,0.9,0.95,0.995)

names(sim2) <- paste(round(100*zeta, 2), "%", sep="")
names(sim14)<- paste(round(100*zeta, 2), "%", sep="")
names(sim14_lost)<- paste(round(100*zeta, 2), "%", sep="")

names(sim2)<- paste(round(100*zeta, 2), "%", sep="")
names(sim60) <- paste(round(100*zeta, 2), "%", sep="")

names(sim_7_7)<- paste(round(100*zeta, 2), "%", sep="")
names(sim_1_13)<- paste(round(100*zeta, 2), "%", sep="")
names(sim_13_1)<- paste(round(100*zeta, 2), "%", sep="")

library(reshape2)
df.14 <- melt(sim14)
df.14_lost <- melt(sim14_lost)
df.14$lost_sale <- 'Yes'
df.14_lost$lost_sale <- 'No'


m.df <- rbind(df.14,df.14_lost)

m.7_7 <- melt(sim_7_7)
m.1_13 <- melt(sim_1_13)
m.13_1 <- melt(sim_13_1)
m.7_7$Type <- "LT = 7, OF = 7"
m.1_13$Type <- "LT = 1, OF = 13"
m.13_1$Type <- "LT = 13, OF = 1"
m.df <- rbind(m.7_7,m.13_1)
head(m.df)
m.df <- melt(m.sim)
m.df <- melt(sim14)

m.df <- melt(sim2)
# Plot the result
p <- ggplot(m.df, aes(x = `value`,y = `variable`, height = ..density..)) + 
  #geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, trim = TRUE) +
  #geom_density_ridges(stat = "density", alpha = 0.3,scale = 2, rel_min_height = 0.01, trim = TRUE) +
  geom_density_ridges(stat = "density", alpha = 0.3) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") + 
  xlab("Service Level") + 
  ylab("Confidence Factor") +
  #xlim(0.45, 1)+
  scale_x_continuous(breaks = seq(0.1, 1, 0.1), limits = c(0, 1))+
  labs(title = 'Order period: 14 days') +
  theme_ipsum() +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    plot.title = element_text(size=12),
    strip.text.x = element_text(size = 8)
  )
p
# Plot the result
d1 <- melt(get$ServiceLevel)
d2 <- melt(get3$ServiceLevel)
d1$lost_sale <- 'Yes'
d2$lost_sale <- 'No'
m.df <- rbind(d1,d2)
m.df <- rbind(melt(get$ServiceLevel), melt(get$ServiceLevel))
ggplot(m.df, aes(x = `value`,y = `variable`, color = `lost_sale`, height = ..density..)) + 
  #geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, trim = TRUE) +
  #geom_density_ridges(stat = "density", alpha = 0.3,scale = 2, rel_min_height = 0.01, trim = TRUE) +
  geom_density_ridges(stat = "density", alpha = 0.3) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") + 
  xlab("Service Level") + 
  ylab("Confidence Factor") +
  #xlim(0.45, 1)+
  scale_x_continuous(breaks = seq(0.1, 1, 0.1), limits = c(.45, 1))+
  labs(title = 'Order period: 14 days') +
  theme_ipsum() +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    plot.title = element_text(size=12),
    strip.text.x = element_text(size = 8)
  )
names(m.df)
head(m.df)
m.df

df <- m.df
aggreagte(df$value, list(df$variable, df$lost_sale))
data1 <- df %>% group_by(CF = variable,lost_sale) %>%
  filter(lost_sale == 'Yes') %>%
  summarise_at(vars(value), list(SL = mean)) %>%
  select(CF, SL, lost_sale) %>%
  arrange(lost_sale)
data2 <- df %>% group_by(CF = variable,lost_sale) %>%
  filter(lost_sale == 'No') %>%
  summarise_at(vars(value), list(SL = mean)) %>%
  select(CF, SL, lost_sale) %>%
  arrange(lost_sale)

join <- inner_join(data1,data2, by = "CF") %>% select(CF, SL.x, SL.y)

names(join) <- c("CF", "SL - Lost Sale", "SL - Not Lost Sale")

df %>% 
    ggplot(aes(y = variable, x = value, fill = lost_sale)) +
    geom_density_ridges(quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x), alpha = 0.4) + 
  xlab("Service Level") + 
  ylab("Confidence Factor") +
  #xlim(0.45, 1)+
  scale_x_continuous(breaks = seq(0.1, 1, 0.1), limits = c(0.45, 1))+
  labs(title = 'Order period: 14 days', fill = 'Lost Sale') +
  theme_ipsum() +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    plot.title = element_text(size=12),
    strip.text.x = element_text(size = 8)
  )
