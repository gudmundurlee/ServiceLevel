
library(readxl)
#df_month <- read_excel("sale_month.xlsx")
df_pois <- read_excel("Data/poisson_month.xlsx")
head(df_pois)
# Read split the data

ts_month <- ts(df_pois$total)

# Exponential smoothing 

ses_month <- forecast::ses(ts_month, h = 1 ,level = 0.95) 
summary(ses_month)

(demand <- 14*(ses_month$mean/30))
(sd_demand <- sqrt(14*ses_month$model$sigma2)/30) ## I think this is alright

sd_demand*1.65*sqrt(14)

hist(rnorm(50000, demand, sd_demand))

cf <- seq(0.5,0.99, 0.01)
s_stocks <- qnorm(cf)*sd_demand*sqrt(14)
optimal_stock <- s_stocks + sum(demand)
n_stock <- length(optimal_stock)
rate <- c()
nsim <- 100000
for(i in 1:n_stock){
  sim_sales <- rnorm(nsim, demand, sd_demand)
  rate[i] <- length(sim_sales[sim_sales > optimal_stock[i]])
}
ratio <- 1-(rate/nsim)
table <- cbind(cf,ratio)
write.csv(table, file = "table.csv")
# Simulation of the next sales.
mu.y <- ses_month$mean
sd.y <- sqrt(ses_month$model$sigma2)

hist(rnorm(10000, mu.y, sd.y))

# Arima

arima_model <- auto.arima(ts_month)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=1)
fore_arima


# Function that simulates poisson time data. 

ts_poisson <- function(ts_lambda, days){
  sim_pois <- rpois(ts_lambda, days)
  return(sum(sim_pois))
}
# Actual sales
actual <- df_pois$total
# Mean vector
lambdas <- df_pois$lambda
# Days of each month
days <- df_pois$days

# Generate a simulaton vector of means
sim <- c()

# run simulation
for(i in 1:nrow(df_pois)) {
  sim[i] <- ts_poisson(lambdas[i], days[i])
  
}
sim
# MAPE 
mape(actual, sim)
