

library(seastests)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

is_seasonal_regression <- function(y){
  n <- length(y)
  
  x <- seq(1,n)
  season <-  as.factor(((x-1) %% 12)+1)
  df <- data.frame(y = y, Time = x, Month = season)
  fit0 <- lm(y~Time, data = df)
  fit1 <- lm(y~Time+Month, data = df)
  

  
  s_test <- anova(fit0, fit1)

  p_value <- s_test$`Pr(>F)`[2]
  
  if(p_value < 0.05){
    print(sprintf("P-value is %f so there we reject the hypothesis that the models are the same therefore we have a seasonal effect", p_value))
    
  }
  else {
    print(sprintf("P-value is %f so there we cannot reject the hypothesis that the models are the same therefore no seasonal effect", p_value))
  }
  
  return(df)
}


df <- is_seasonal_regression(births)
fit0 <- lm(y~Time, data = df)
fit1 <- lm(y~Time+Month, data = df)

summary(fit0)
summary(fit1)

s_test <- anova(fit0, fit1)
s_test$`Pr(>F)`[2]
plot(ts(births))

# Some old data.

y <- ts(births, frequency = 12, start = c(1946,1))
y
plot(y)
isSeasonal(y.t, test = "combined")
isSeasonal(y.t, test = "qs")
isSeasonal(y.t, test = "fried")
isSeasonal(y.t, test = "kw")
isSeasonal(y.t, test = "seasdum")
isSeasonal(y.t, test = "welch")


y.t <- ts(d_to_m(random_ts(5)), frequency =  12)
y.t
plot(y.t)
is_seasonal_regression(y.t)

birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

View(birthstimeseriescomponents$seasonal)
t1 <- ts(rnorm(120, 10,10), frequency=12, start = (2018))

t2 <- ts(rnorm(1200, 10,10), frequency=7)
plot(t1)
t1
plot(t2)

output <- isSeasonal(birthstimeseries)
qs(birthstimeseries)
kw(birthstimeseries)
output
isSeasonal(t2)

check_residuals(t1)

testt <- ocsb(t1)
testt$model

y <- rnorm(120,10,10)
x <- seq(1,length(y))
season <- as.factor(rep(seq(1,12),10))

fit0 <- lm(y~x)
fit1 <- lm(y~x+season)

y_new <- birthstimeseries
n <- length(y_new)/12
x <- seq(1,length(y_new))
season <- as.factor(rep(seq(1,12),n))

fit0 <- lm(y_new~x)
fit1 <- lm(y_new~x+season)


summary(fit0)
summary(fit1)
anova(fit0,fit1)
anova(fit1,fit0)
