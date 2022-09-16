
(cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99))
(alpha <- 1-cf)
(z_score <- 1-alpha/2)

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
x
df <- sapply(x, FUN = function(x){fx(30,x)})
df <- as.data.frame(df)
names(df) <- paste(x)
#df$days <- seq.int(nrow(df))
df
plot.df <- melt(df)
plot.df$days <- rep(seq(1,31),length(x))
plot.df

#names(plot.df) <- c("Stock", "Probability", "Days")
names(plot.df) <- c("lambda", "Probability", "Days")
#plot.df$Stock <- as.factor(plot)

ggplot(plot.df) + geom_line(aes(x = Days, y = Probability, color = as.factor(lambda)), size = 1.5) + 
scale_x_continuous(breaks = seq(1, 31, 5))+
scale_y_continuous(breaks = seq(0, 1, 0.1))+
  labs(title = 'Probability of demand higher than stock.',color='Stock') +
  theme_ipsum() +
  theme(
    
    plot.title = element_text(size=12),
    strip.text.x = element_text(size = 8)
  )
