births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency = 12, start = c(1946,1))
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

View(birthstimeseriescomponents$seasonal)

## Easy way to transform poisson to standard normal.

x.cf <- c(0,0.5,0.6,0.7,0.8,0.9,0.95,0.99)

x.alpha <- 1-x.cf
(x.z_score <- 1-x.alpha/2)

z_score <- c(0.5, 0.6, 0.7,0.8,0.9, 0.95,0.99)

2*z_score -1

s_t <- function(p, lambda, n){
  
  s_0 <- l*p
  
  return(s_0)
  
}

U_k <- function(k,a = 0,b = 100){
  n <- a+b
  kk <- seq(1,k,1)
  s.k <- sum((floor(k)-a+1)/(n+1))
  return(s.k)
}
U_k(99)
xk <- seq(1,100)
yk <- sapply(xk, FUN = U_k)

plot(xk, yk)

x <- seq(0,10,1)
plot(ppois(x, 5))

# Probability of being out of stock after n days:
d <- 7
lambda <- 1
days <- seq(1,d,1)
#daily demand
cover <- lambda*length(days)
cover
dpois(5,6)

# ppois
y <- dpois(days,cover)
y.n <- ppois(days, cover-days)
plot(days,y,type = "l")
plot(days,y.n,type = "l")
l <- 5
x.l <- seq(l-l, l+l, 1)
1/ppois(x.l,l)
plot(x.l, y = ppois(x.l,l), type = "l")

50 
rpois(1, 50/50)


p <- rpois(1000, 5)

par(mfrow = c(1,2))
hist(p)
hist(p + 5)
par(mfrow = c(1,1))

theta_pois <- function(x,lambda){
  mu <- lambda
  sigma <- sqrt(lambda)
  z <- (x-mu)/sigma
  theta <- pnorm(z, 0, 1)
  return(theta)
}

plot(cumsum(rnorm(10000)), type = "l")
mean(rnorm(10000))
sd(rnorm(10000))
yz <- rnorm(10000)*sqrt(10)+ 10
yz[yz < 0] <- 0
hist(yz)
mean(yz)
var(yz)

var(rnorm(100000)*sqrt(10) + 10)



sl_pois <- function(d){
  lambdas <- seq(1,d,1)
  y <- 1-theta_pois(d,lambdas)
  y
  (ey <- sum(y*(lambdas/d)))
  
  return((d-ey)/d)
}

d <- seq(1,14,1)
sl <- sapply(d, FUN = sl_pois)

plot(d,sl, type = "b")

plot(lambdas,y = y, type = "b")

format(y, scientific = FALSE )

cumsum(y)

#

Y.i <- function(i, lambda){
  sum.i <- sum(rpois(1,lambda))
  return(sum.i)
}
Y.i(1,100)
1- theta_pois(100,100)


plot(x = lambdas, y = y, type = "l")


gen_Y <- function(years, lambda, N = 2){
  days <- years*360
  YY <- lapply(rep(years,N), FUN = random_ts, years, lambda)
  Y <- matrix(unlist(YY), ncol = N, nrow = days)
  return(Y)
}

Y.sim <- gen_Y(4,50,100)
dim(Y.sim)
Y.sim[,1]
# Using actual

output.sim <- apply(Y.sim, MARGIN = 2, FUN = function(x){mean(simulation_use_actual(N = 100,years = 4, 0, 12, 0.0, 100,30, y = x)) })
output.sim
mean(output.sim)
hist(output.sim)
mean(simulation_use_actual(1, 4, 0, 12, cf = 0.99, lt = 7, of = 7 ,y = mat))

demand_calculation(Y.sim[1,])

simulation_use_actual <- function(N,years, use_days = 1, h = 12, cf = 0.95, lt = 7, of = 7, y = NULL){
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
    
    demand_a <- y_test
  }
  
  # If we choose to have monthly data instead
  
  if(use_days == 0){
    
    
    y_train <- unname(tapply(y_train, (seq_along(y_train)-1) %/% period, sum))  
    #y_test <-  unname(tapply(y_test, (seq_along(y_test)-1) %/% period, sum)) # This is never being used
    
    # Fit monthly model
    
    model <- fit_model(y_train, h)
    
    #Extract features
    
    demand_p.m <- model$pred
    
    sigma2 <- model$sigma2/(period^2)
    # repeat

    demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
    demand_a <- y_test
  }
  
  # Compute z_score
  (alpha <- 1-cf)
  
  (z_score <- 1-alpha/2)
  
  #print(sprintf("Saftey stock: %f", sqrt(sigma2)*qnorm(1-(1-cf)/2)*sqrt(of+lt)))
  #print(sprintf("Saftey stock: %f", sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)))
  
  
  # Compute the errors
  #p_error <- apply(fit.sim, MARGIN = 1, FUN = function(x){ demand_calculation(y_test = x, demand_n = demand_p, sigma2, cf, lt, of)})
  p_error <- demand_calculation(y_test = demand_a, demand_n = demand_p, sigma2, cf, lt,of)
  #a_error <- apply(fit.sim, MARGIN = 1, FUN = function(x){ demand_calculation(y_test = x, demand_n = y_test, sigma2 = sigma2) })
  # Using the predicted values, we can simulate the ordering period:
  
  return(p_error)
}
