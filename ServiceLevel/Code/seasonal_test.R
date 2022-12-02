

library(seastests)
library(readxl)
library(tidyverse)
#births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
test <- decompose(y.test)
plot(decompose(y.test))
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
    #print(sprintf("P-value is %f so there we reject the hypothesis that the models are the same therefore we have a seasonal effect", p_value))
    return(TRUE)
  }
  else {
    #print(sprintf("P-value is %f so there we cannot reject the hypothesis that the models are the same therefore no seasonal effect", p_value))
    return(FALSE)
  }
  
  #return(df)
}

# Test the function

#df <- is_seasonal_regression(births)


agr <- read_excel("AGR Dynamics/ServiceLevel/Data/agr_demo_month.xlsx")
names(agr)
y.ts <- ts(item, frequency = 12, start = c(2020,1), end = c(2021,12))

plot(y.ts)
plot(decompose(y.ts))

item1 <- agr %>% select(.,sale) %>% filter(agr$item_id == 1)
y1 <- unlist(item1)
x1 <- seq(1,length(y1))
(season <-  as.factor(((x1-1) %% 12)+1))
df <- data.frame(y = y1, Time = x1, Month = season)
summary(lm(y~Time+Month, data = df))


lm_seasonality <- function(item){
  y <- select(agr,sale) %>% filter(agr$item_id == item)
  y.ts <- ts(y, frequency = 12)
  if(length(y.ts) >= 24 & sd(y.ts) > 0){
    return(is_seasonal_regression(unlist(y)))
  }
  else {
    return(FALSE)
  }
}

isSeasonal_lm <- function(y){
  #y <- select(agr,sale) %>% filter(agr$item_id == item)
  y.ts <- ts(y, frequency = 12)
  if(length(y.ts) >= 24 & sd(y.ts) > 0){
    return(is_seasonal_regression(unlist(y)))
  }
  else {
    return(FALSE)
  }
}


check_seasonality <- function(item){
  #print(item)
  y <- select(agr,sale) %>% filter(agr$item_id == item)
  y.ts <- ts(y, frequency = 12)
  if(length(y.ts) >= 24 & sd(y.ts) > 0){
    #print(item)
    return(isSeasonal(y.ts, test = "seasdum"))
  }
  else{
    return(FALSE)
  }
}



seasonal_test <- function(item){
  y <- select(agr, sale) %>% filter(agr$item_id == item)
  y.ts <- ts(y, frequency = 12)
  if(length(y.ts) >= 24 & sd(y.ts) > 0){
  t.c <- try(
      isSeasonal(y.ts, test = "combined"), silent = T, outFile = FALSE
      )
  t.qs <- try(
      isSeasonal(y.ts, test = "qs"), silent = T, outFile = FALSE
      )
  t.f <- try(
      isSeasonal(y.ts, test = "fried"), silent = T, outFile = FALSE
      )
  t.kw <- try(
      isSeasonal(y.ts, test = "kw"), silent = T, outFile = FALSE
      )
  t.s <- try(
      isSeasonal(y.ts, test = "seasdum"), silent = T, outFile = FALSE
      )
  t.w <- try(
      isSeasonal(y.ts, test = "welch"), silent = , outFile = FALSE
      )
  t.lm <- try(
      isSeasonal_lm(y) , silent = T, outFile = FALSE
  
    )

  s_df <- data.frame(Item = item, Combined = t.c, QS = t.f, Fried = t.f, KW = t.kw, Seasdum = t.s, Welch = t.w, Regression = t.lm)
  return(s_df)
  }
  else {
    return(data.frame(Item = item, Combined = FALSE, QS = FALSE, Fried = FALSE, KW = FALSE, Seasdum = FALSE, Welch = FALSE, Regression = FALSE))
  }
}

seasonal_test2 <- function(item){
  y <- select(agr, sale) %>% filter(agr$item_id == item)
  y.ts <- ts(y, frequency = 12)
  if(length(y.ts) >= 24 & sd(y.ts) > 0){
    t.c <- tryCatch(isSeasonal(y.ts, test = "combined"), error=function(e) FALSE)
    t.qs <- tryCatch(isSeasonal(y.ts, test = "qs"), error=function(e) FALSE)
    t.f <- tryCatch(isSeasonal(y.ts, test = "fried"), error=function(e) FALSE)
    t.kw <- tryCatch(isSeasonal(y.ts, test = "kw"), error=function(e) FALSE)
    t.s <-  tryCatch(isSeasonal(y.ts, test = "seasdum"), error=function(e) FALSE)
    t.w <-  tryCatch(isSeasonal(y.ts, test = "welch"), error=function(e) FALSE)
    t.lm <-  tryCatch(isSeasonal_lm(y.ts), error=function(e) FALSE)
    s_df <- data.frame(Item = item, Combined = t.c, QS = t.f, Fried = t.f, KW = t.kw, Seasdum = t.s, Welch = t.w, Regression = t.lm)
    return(s_df)
  }
  else {
    return(data.frame(Item = item, Combined = FALSE, QS = FALSE, Fried = FALSE, KW = FALSE, Seasdum = FALSE, Welch = FALSE, Regression = FALSE))
  }
}
item1 <- agr %>% select(.,sale) %>% filter(agr$item_id == 1)
item <- agr %>% select(.,sale) %>% filter(agr$item_id == 8)

plot(ts(item1))
y.test <- ts(item1, frequency = 12)
cs <- combined_test(y.test)
qs(y.test)

isSeasonal(y.test, "kw")
isSeasonal(y.test, "fried")
isSeasonal(y.test, "welch")
isSeasonal(y.test, "seasdum")
isSeasonal(y.test, "qs")

fried(y.test)
summary(cs)
# Run this without any problems
start_time = Sys.time()
y.seasonal <- sapply(x.item, FUN = seasonal_test2)
end_time = Sys.time()

result <- t(y.seasonal)
is_seasonal_regression(y.t)

birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

View(birthstimeseriescomponents$seasonal)
library(aTSA)
x <- rnorm(100)
trend.test(x,plot = TRUE) # no trend

x <- 5*(1:100)/100
x <- x + arima.sim(list(order = c(1,0,0),ar = 0.4),n = 100)
plot(x)

trend.test(x,plot = TRUE) # increasing trend

old <- function(){
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
}

