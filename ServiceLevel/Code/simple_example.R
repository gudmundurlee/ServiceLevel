
(cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99))
(alpha <- 1-cf)
(z_score <- 1-alpha/2)

dnorm(0, 0, 1)

p <- 1-(1-0)/2
i <- qnorm(0.75)
i <- qnorm(0.975)
integrate(dnorm, -i,i)




ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm) + 
  stat_function(fun = dnorm, 
                xlim = c(-i,i),
                geom = "area", fill = "blue",alpha = 0.3) +
  theme_ipsum() 
## Another way 
size <- 1.968389
mu <- 4.0783
prob <- size/(size+mu)
#nbin <- rnbinom(100000, mu = 1.6157, size = 0.593632)


n <- length(yy)/12
nn <- n-1
y_tr <- yy[1:(1*12)] # train
y_te <- yy[(1*12+1):(2*12)] # test
library(forecast)
fit <- auto.arima(y_tr)
f <- forecast::forecast(fit, h = 12)
mean((f$mean-y_te)^2)
y_te

ts_cv <- function(y, h = 12){
  n <- length(y)/h 
  nn <- n-1
  score <- rep(0,nn)
  for(i in 1:nn){
    y_tr <- y[1:(i*h)]
    y_te <- y[(i*h+1):((i+1)*h)]
    fit <- auto.arima(y_tr)
    f <- forecast::forecast(fit, h)
    # Instead we can calculate the average service level
    score[i] <- mean((f$mean-y_te)^2)
  }
  return(score)
}
ts_cv(yy, h = 12)

split <- function(i, h){
  train <- c(1:(i*h))
  val <- c((i*h+1):((i+1)*h))
  return(list(train = train, val = val))
}
split(2, 12)
splits[[1]]
splits[[4]]

splits<- lapply(1:5, FUN = split, h = 12) # 1 to n

print_splits <- function(l){
  print(l$train)
  print(l$val)
}

cv_splits <- function(l){
  y_tr <- yy[l$train]
  y_te <- yy[l$val]
  fit <- auto.arima(y_tr)
  f <- forecast::forecast(fit, h = 12)
  # Here call the function that returns the service level score
  return(mean((f$mean-y_te)^2))
}


lapply(splits, FUN = cv_splits)

gett <- ts_cv(yy)
sqrt(gett)

nbin <- rnbinom(360*6, size = size, prob = prob)
hist(nbin)
mean(nbin)
mean(nbin)*30
var(nbin)
breaks <- qnorm(c(0, .05,.95, 1))

yy <- ts(to_month(nbin))
plot(yy)
ggplot(data.frame(x = c(-2, 2)), aes(x)) +
  scale_fill_brewer("x") +
  stat_function(
    n = 512,
    fun = dnorm,
    geom = "area",
    colour = "gray30",
    aes(
      fill = after_stat(x) |> cut(!!breaks),
      group = after_scale(fill)
    )
  )

## Poisson(mu) => Normal(mu,mu) if mu > 10
## 


xt <- function(OP, lambda = 10 ){
  x0 <- OP*lambda
  x1 <- x0 - sum(rpois(OP,lambda))
  return(x1)
}

xn <- sapply(rep(10,50000), FUN = function(x) {xt(OP = x)})

hist(xn)

length(xn[xn <= 0])/length(xn)

stock_ratio <- function(mu, op, cf = 0.95) {
  alpha <- 1-cf
  z <- 1-alpha/2
  upp <- sqrt(mu)*qnorm(z)*sqrt(op)
  low <- mu + upp
  return(upp/low)
}

stock_ratio(10,14)

sr <- seq(1,100,5) 
sr
g <- sapply(sr, FUN = function(x){stock_ratio(30,op = x, cf = 0.2)})

cf_stock_ratio <- function(cf){
  days <- seq(1,100)
  return(sapply(days, FUN = function(x) {stock_ratio(50,x, cf = cf)}))
}
cfs <- matrix(c(0,0.2,0.4, 0.6, 0.8, 0.9,0.95,0.99), nrow = 1)
dim(cfs)
out <- apply(cfs, MARGIN = 2, FUN = cf_stock_ratio)
df.out <- as.data.frame(out)
cfs.lab <- 1-(1-cfs)/2
names(df.out) <- paste(cfs.lab*100, "%", sep ="")#paste(cfs.lab)
head(df.out)
library(reshape2)



df.plot <- melt(df.out)
head(df.plot)
dim(df.plot)
X <- rep(seq(1,100),8)
df.plot$OP <- X
names(df.plot) <- c("CF", "SS_ratio", "OP")
library(ggplot2)
ggplot(df.plot) + geom_line(aes(x = OP, y = SS_ratio, color = CF), size = 1)+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 0.8)) +
  scale_x_continuous(breaks = seq(0, 100,5 ), limits = c(1,50))+
  ylab("Safety Stock Ratio") + xlab("Order Period") +
  theme_ipsum() +
  labs(title = "Safety Stock as ratio of total stock") +
  guides(color = guide_legend(reverse=TRUE))
### Generate random time series end ##
rpois(100,1)
qpois(0.5,1)

probs <- seq(0,1,0.01)
plot(x = probs, qpois(probs,10))
xx <- seq(0,15,1)
plot(xx, dpois(xx,10))

100*dpois(xx,10)
# Quantiles of a poission distribution
# Fx(x) := P(X <= x)
# Let Y be poisson(lambda)
# n be the number of days
# For simplicity, assume we sell on average 1 so lambda = n.

n <- 10
lambda <- 1*n
# So after i days we have lambda = i
x <- seq(0,n+lambda)
y <- ppois(x,lambda)
y
plot(x, ppois(x,lambda), type = "l")

ppois(1,10)
ppois(0.5,10)

fx <- function(n,lambda){
  #lambda <- n
  x.n <- seq(0,n)
  y.n <- ppois(x.n, lambda)
  return(y.n)
}
# ppois(q,l) =>  Pr(X <= x)

ppois(1.5,1)

pmf <- function(k,lambda){
  upp <- (lambda^k)*exp(-lambda)
  low <- factorial(k)
  return(upp/low)
}
pmf(1,1)
ppois(30,30)
ppois(1,1)
signif(ppois(seq(0,30),30)*100,2)
x <- c(1,5,10,15,20,25,30)
xx <- seq(1,30)
plot(xx,cumsum(ppois(xx,30)), type = "l")
yy <- cumsum(ppois(xx,30))
yy <- ppois(xx,30)

ex_df <- data.frame(Days = xx, Probability = yy)
ggplot(ex_df) + geom_line(aes(x = Days, y  = Probability)) +
scale_x_continuous(breaks = seq(1, 31, 5))+
  scale_y_continuous(breaks = seq(0, 1, 0.1))+
  
  labs(title = 'Probability of demand higher than stock.',color='Stock') +
  theme_ipsum() +
  theme(
    
    plot.title = element_text(size=12),
    strip.text.x = element_text(size = 8)
  )

x <- c(1,10,20,30)
df <- sapply(x, FUN = function(x){fx(30,x)})
df <- as.data.frame(df)
names(df) <- paste(x)
#df$days <- seq.int(nrow(df))
library(reshape2)
plot.df <- melt(df)
plot.df$days <- rep(seq(1,31),length(x))
plot.df

#names(plot.df) <- c("Stock", "Probability", "Days")
names(plot.df) <- c("lambda", "Probability", "Days")
#plot.df$Stock <- as.factor(plot)

ggplot(plot.df) + geom_line(aes(x = Days, y = Probability, color = as.factor(lambda)), size = 1.5) + 
scale_x_continuous(breaks = seq(1, 31, 5))+
scale_y_continuous(breaks = seq(0, 1, 0.1))+
  labs(title = 'Probability of demand higher than stock.',color='Starting Stock') +
  theme_ipsum() +
  theme(
    
    plot.title = element_text(size=12),
    strip.text.x = element_text(size = 8)
  )

