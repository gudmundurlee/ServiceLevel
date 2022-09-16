
agr_ss <- function(std_d, cf = 0.95, op = 14) {
  days_in_period <- 30 #Assume 1 month every time.
  z_factor <- qnorm(cf)
  ss_low <- z_factor*std_d*sqrt(floor(op/days_in_period))
  ss_high <- z_factor*std_d*sqrt(ceiling(op/days_in_period))
  ss <- ss_low + ((op %% days_in_period)/days_in_period)*ss_high
  return(ss)
}
agr_ss(5)

saftey_stock <- function(std_d, sf = 1.65, ord_per = 14) {
  
  ss <- sf*std_d*sqrt(ord_per)
  return(ss)
}
saftey_stock(5,0.95)

sff <- seq(1.0,3.5, 0.01)
y <- saftey_stock(std = 10, sf = sff)

plot(sff,y)

# MAPE function:
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

estimated_stock <- function(stock_today, stock_move) {
  est_stock <- stock_today - stock_move
  return(est_stock)
}


undelivered <- function(){}

## Demand function: 
## Input: Stock, Undelivered
## Ignore lost sale

ss_level <- function(p, lt, of){
  return(qnorm(p)*sqrt(lt + of))
}

lt_s <- seq(1,14,1)
of_s <- seq(1,14,1)
p <-c(seq(0.5,0.95, 0.05),0.99)
x <- seq(0.01, 1, 0.01)
library(dplyr)  
library(purrr)
library(ggplot2)
# Define a named list of parameter values
gs <- list( LT = lt_s,    OF = of_s, P = 0.95 , d_std = x )

tuning_param <- gs %>% cross_df()

op <- function(p){
  ss <- ss_level(p, lt = 7, of = 7)
  return(ss)
}

tuning_param$SS <- qnorm(tuning_param$P)*sqrt(tuning_param$LT+tuning_param$OF)*tuning_param$d_std
head(tuning_param)

ggplot(data=tuning_param, aes(x = LT, y = OF,  group=d_std, colour = SS)) +
#  geom_line() +
  geom_point()


h <- 6
period <- 30
test <- ts_generator(3, 5)
y_d <- test$daily
y_m <- unname(tapply(y_d, (seq_along(y_d)-1) %/% period, sum))

y_m_train <- y_m[1:(length(y_m)-6)]
y_m_test <- y_m[(length(y_m)-6+1):length(y_m)]

y_train <- y_d[1:(days-h*period)]
y_test <- y_d[(days-h*period+1):days]

plot(test$daily)
plot(test$monthly)

fit.m <- auto.arima(y_train)

f_m <- forecast::forecast(fit.m, h = 180)
summary(f_m)
y_m_test
(f_v <- f_m$mean)
length(f_v)
length(y_test)
mean((f_v-y_test)^2)
(f_sigma2 <- f_m$model$sigma2)



#Extract features
(demand_p <- model$pred)
(f_upper <- demand_p+qnorm(0.975))

sum(score(dummy <- f_upper-fit.sim[1,]))/60

score <- function(x){
  
  x[x >= 0] <- 1
  x[x < 0] <- 0
  return(x)
}

dim(fit.sim)
mean(apply(fit.sim, MARGIN = 1, FUN = function(x){sum(score(f_upper - x))})/60)

(sigma2 <- model$sigma2)


fit <- model$fit
summary(fit)


fit.sim <- t(sapply(1:N, FUN = function(x) {sim_fit(fit,h*period)}))
fit.sim[fit.sim < 0 ] <-0


hist(fit.sim)
mean(fit.sim)

sl_score <- function(pred,actual){
  sl <- actual-pred
  
}

## Tuning Parameters

t_params <- function(cf, lt, of){
  return(simulation(N = 10, years = 3, h = 12))
}

apply(rep_sim,MARGIN = 2, mean)

tuning_parameters <- function(){
  cf_s <- c(seq(0.5,0.9,0.1),0.95,0.99)
  lt_s <- seq(1,14,1)
  of_s <- c(1,7,14)
  gs <- list( LT = lt_s,    OF = of_s, CF = cf_s)
  tuning_param <- gs %>% cross_df()
  return(tuning_param)
}

tuning_param <- tuning_parameters()

(rep_sim <- t(sapply(1:100, FUN = function(x) {sim_fit(fit,30)})))
rep_sim[rep_sim < 0 ] <- 0

dim(rep_sim)




sqrt(f_sigma2/30)*qnorm(0.975)*sqrt(of+lt)

h <- 2
m_to_day <- function(mat,i,j){
  rep(mat[i,j]/30,30)
}
month_to_days <- function(value, days = 30){
  daily_value <- rep(value/days, days)
  return(daily_value)
}
simulate(fit.d, nsim = 300, family = "poisson")
auto.arima(y, family = "poisson")
month_to_days(rep_sim[1,])
g <- (rep_sim[1,])
rep(g[seq_along(g)]/30,30)

x_value <- rep(seq(1,length(g)),each = 30)
g[x_value]/30
length(g)
(rep_sim <- t(sapply(1:100, FUN = function(x) {sim_fit(fit,h)})))

tapply(rep_sim[1,],seq_along(rep_sim[1,]), function(x){rep(rep_sim[1,]/30, 30)})

rep_sim
period <- 30
hh <- ncol(rep_sim)
xh <- rep(seq(1,hh), each = period)
xh

test <- lapply(1:hh, FUN = function(x){rep_sim[,xh]/period})
#test
#dim(test[[1]])
(demand_n <- f_v[xh]/30)

f_sigma2

n <- nrow(rep_sim)
output <- rep(0,n)

dim(rep_sim)
rep_sim[1,xh]/period
rep_sim[1,]

f_v
f_v

for(i in 1:n){
  
  demand_p <- rep_sim[i,xh]/period
  output[i] <- mean(sim_iteration(y_test = demand_p, demand_n = demand_n, sigma2 =f_sigma2, cf = 0.999, lt = 7, of = 7))
}


mean(output)

hs <- seq(1,nrow(rep_sim))
