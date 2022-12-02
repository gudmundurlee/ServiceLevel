library(readxl)
agr_demo <- read_excel("AGR Dynamics/ServiceLevel/Data/agr_demo.xlsx")


# Change the types
agr_demo$item_id <- as.integer(agr_demo$item_id)
agr_demo$time_id <- as.integer(agr_demo$time_id)
agr_demo$history_date <- as.Date(agr_demo$history_date)
agr_demo$year <- as.factor(agr_demo$year)
agr_demo$month <- as.factor(agr_demo$month)
agr_demo$sale_value <- as.integer(agr_demo$sale_value)
agr_demo$stock_value <- as.integer(agr_demo$stock_value)
agr_demo$stock_delivered <- as.integer(agr_demo$stock_delivered)
agr_demo$confidence_factor <- as.numeric(agr_demo$confidence_factor)
agr_demo$order_frequency_days <- as.integer(agr_demo$order_frequency_days)
agr_demo$lead_time_days <- as.integer(agr_demo$lead_time_days)
agr_demo$type <- as.factor(agr_demo$type)

#
summary(agr_demo)
str(agr_demo)

ii <- select(agr_demo,-type) %>% filter(item_id == 1 & sale_value >0)
dim(ii)
items <- unique(agr_demo$item_id,)


days <- seq(1,360)
of <- 2
lt <- 12
days[days %% lt == 0]

(order_days <- days[days %% of == 0] - (of - 1) )#(seq(1,floor(n/of))*of) - (of - 1)

#delivered_days <- days[days %% (of+lt) == 0] - (of -1) #(seq(1,floor(n/of))*of) - (of-1) + lt
(delivered_days <- days[days %% (lt) == 0])
order_days
#If it is a delivery or order
is_order[order_days] <- 1
is_delivery[delivered_days] <- 1


# Forecasting functions
y <- ts(ii$sale_value)
plot(y)
ym <- ts(unname(tapply(y, (seq_along(y)-1) %/% 12, sum)),frequency = 12)
ym
y <- ts(rnorm(120,0,3) + 20*sin(2*pi*(1:120)/12), frequency=12)
y <- ts(rpois(120, 20) + 20*sin(2*pi*(1:120)/12), frequency=12)
plot(y)
auto.arima(y)
rnorm(120,0,3)
nn <- 360
y <- ts(rpois(nn, 20) + 20*sin(2*pi*(1:nn)/12), frequency=12)

y.d <- decompose(y)
head(y.d)
plot(y.d)
CVar(y,k = 10)
plot(ym)
fit1 <- tslm(y ~ trend + season)
fit2 <- tslm(y ~ trend)
summary(fit1)
summary(fit2)
anova(fit1,fit2)

days <- seq(1,360)
tesstt <- cf_data(4,10,12, 2,12)
tesstt
order <- days[days %% 13 == 0] - (13-1)
length(order)
library(forecast)

check_forecast <- function(y, h){
  yy <- ts(y[y > 0])
  y_crost <- croston(yy)
  y_arima <- auto.arima(y)
  
  lambda <- BoxCox.lambda(yy)
  y_BC <- BoxCox(yy, lambda)
}
y_crost <- croston(y)
y_arima <- auto.arima(y)
f_arima <- forecast::forecast(y_arima)
f_crost <- forecast::forecast(y_crost)
plot(f_arima)
plot(f_crost)
lambda <- BoxCox.lambda(y.t[y.t > 0])
lambda
y_BC <- BoxCox(y.t, lambda)
y_BC
#select(agr_demo,-type) %>% filter(item_id == 1 & sale_value >0)

library(readxl)
library(seastests)

agr <- read_excel("AGR Dynamics/ServiceLevel/Data/agr_demo_month.xlsx")
names(agr)

agr %>% select(.,sale) %>% filter(agr$item_id == 1)

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

x <- unique(agr$item_id)
x.seasonal <- sapply(x, FUN = check_seasonality)
df.seasonal <- data.frame(item_id = x,isSeasonal = x.seasonal)
df.seasonal
isSeasonal(y.ts,test ="seasdum")
y <- select(agr,sale) %>% filter(agr$item_id == 2939)

y.ts <- ts(y, frequency = 12)
plot(y.ts)
isSeasonal(y.ts, test = "seasdum")
#dec.ttt <- decompose(ttt)
#isSeasonal(ttt)

#