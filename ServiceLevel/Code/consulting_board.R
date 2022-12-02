
library(ggplot2)
library(knitr)
## What is a confidence factor
## Agr defines the confidence factors to be

cf <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95, 0.99)
hist(rnorm(1000, 10, sqrt(10)))
min(rnorm(1000, 10, sqrt(10)))

l <- 10
p1 <- data.frame(y = rnorm(100000,l, sqrt(l)))
p2 <- data.frame(y = rpois(100000,l))

p1$type = 'Normal'
p2$type = 'Poisson'

yn <- rnorm(360*3,0,1)
#ynm <- ts(unname(tapply(yn, (seq_along(yn)-1) %/% 30, sum)), frequency =12, start = 2020)

#plot(ynm)
plot_ts <- function(y){
  yn <- unname(tapply(y, (seq_along(y)-1) %/% 30, sum))
  years <- length(yn)/12
  months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec")
  df_plot <- data.frame(Y = yn, 
                        Time = seq(1,length(yn)), 
                        Months = rep(months,years), 
                        Year = seq(1,years), 
                        Labels = rep("Simulation", length(yn)))
  fill <- c(rep("Train", (years-1)*12),rep("Test", 12))
  df_plot$Labels <- fill
  plot <- ggplot() +s
    geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge") +
    theme_ipsum()
  return(plot)
}
#y <- ts(rnorm(120,0,3) + 20*sin(2*pi*(1:120)/12), frequency=12)
y <- rpois(360*4, l/30) + rep(l*sin(2*pi*(1:360)/12),4)
y <- rnorm(360*4, l,sqrt(l)) + l*sin(2*pi*(1:360)/12)
#y <- rnorm(360*4, 10, sqrt(10))
runif(100, 0,10)
y <- rpois(360*4,0.001) + rep(sin(pi*seq(0,1, length.out = 360)),4) + seq(1,360*4)/30^2
sin(pi/4*(1:360)/12)
rpois(360,0.05)
plot_ts(y)
y <- rpois(360*4, 0.1)
mean(y)
var(y)

df <- rbind(p1,p2)

ggplot(df, aes(x = y, fill = type, color = type)) + geom_histogram(position = "identity", alpha = 0.3, bins = 20)

xx <- seq(1,20,0.1)
yy <- dnorm(0, xx, sqrt(xx))
dnorm(0,0.1,sqrt(0.1))
plot(xx,yy, type = "l")

hist(rpois(10000, 10))
plot(density(rnorm(1000, 10, sqrt(10))))
# AGR wans to know the coverage:
z_score <- 1-(1-cf)/2
(upper <- qnorm(z_score))
cf_data_frame <- data.frame(CF = cf, Coverage = z_score)

kable(t(cf_data_frame))
get <- gen_demand_data(rpois(100,20),rnorm(100,20,sqrt(20)), 10, 20,7,7 )
get_demand <- demand(get,2,100,10,7,7)

10/get_demand$est_stock

qnorm(0.95,0,1)

1-(1-1)/2

normal <- function(mu, sigma, x){
  1/(sigma*sqrt(2*pi))*exp(-((x-mu)/sigma)^2)
}

normal_expr <- function(){
  expression(N~bgroup('(',paste(x, '; ',mu, ',', sigma),')') == frac(1, sigma~sqrt(2*pi)) ~ 
               exp~bgroup('[',-~bgroup('(',frac(x-mu,sigma),')')^2,']'))
  
}

normal_shade <- function(mu, sigma, x, z){
  y <- normal(mu=mu, sigma=sigma, x)
  cl <- mu + qnorm(z,mu, sigma)
  y[x < -cl | x > cl] <- NA
  return(y)
}

#qnorm(0.5)
mu <- 0
sigma <- 1
z <- 0.75



ggplot(data.frame(x=c(-3,3)), aes(x=x, color=g)) + 
  stat_function(data=data.frame(x=c(-4, 4), g = factor(1)), fun=normal, geom='line', 
                args=list(mu = 0, sigma = 1)) +   

  stat_function(data=data.frame(x = c(-4, 4), g = factor(1)), fun=normal_shade, geom = 'area', fill = 'red', alpha = 0.2,
                args=list(mu = 0, sigma = 1,z = 0.75)) +
  scale_x_continuous(breaks=seq(from=-4, to = 4, by=1)) +
  #ylab(normal_expr()) + 
  ylab(expression(N(mu == 0, sigma == 1))) +
  coord_cartesian(ylim=c(0, 0.4)) +
  scale_color_manual('',values=c('blue','blue' ), 
                     labels=c(expression(N(mu == 0, sigma==1)))) +
  theme(panel.background = element_rect(fill='white'),
        #panel.background has a gray-like color by default
        panel.border=element_rect(fill=NA), 
        #panel.border puts in fill by default
        legend.background = element_blank(),
        legend.box = 'vertical',
        legend.position=c(0.85,0.85),
        legend.text.align=0
  ) 


#

# Instead of a for loop we can use the following:

# df has to have names: "demand_a","demand_p","est_stock","acc_est_stock","delivered","is_order","is_delivery" 
demand <- function(df, i, n, ss, lt, of) {
  
  ii <- min(i+1,n)
  of_i <- min(i+of, n)
  lt_i <- min(i+lt, n)
  op_i <- min(i+(lt + of), n)
  
  if(i <= n){
    df$acc_est_stock[i] <- df$est_stock[i-1]- df$demand_a[i] + df$delivered[i]
    if(lost_sale == TRUE){
      df$est_stock[i] <- df$est_stock[i-1]- df$demand_a[i] + df$delivered[i] 
    }
    else{
      df$est_stock[i] <-max(df$est_stock[i-1]- df$demand_a[i] + df$delivered[i],0)
    }
    #Using Standard: [est_stock] - [demand_p] + [delivered] - [safety stock] - [min stock] = [result] 
    if( ( df$est_stock[i] - ss <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i])) )  & ( df$is_order[i] == 1 )  )  {
      df$delivered[lt_i] <- max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]) - df$est_stock[i] + ss),0)
    }
    return(demand(df, i+1, n, ss, lt, of))
  }
  else {
    return(df)
  }
}
