library(forecast)
library(ggplot2)
library(dplyr)  
library(purrr)
library(reshape2)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(reshape2)
library(knitr)
## Demand Function ##

lost_sale <- TRUE # Global variable
min_stock <- 0 # Gobal variable

# Instead of a for loop we can use the following:
demand <- function(df, i, n, ss, lt, of, lost_sale = TRUE) {
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

    df$est_stock[i] <-df$est_stock[i-1]- df$demand_a[i] + df$delivered[i]
    if(lost_sale == TRUE){
      df$est_stock[i] <-max(df$est_stock[i],0)     
    }
    #Using Standard: 245.0000[stock] - 740.8296[demand] + 736.0000[undeliv. arrived] - 184.3050[safety stock] - 0.0000[min stock] = 55.8654[result] is greater than 0 so calculated order qty is 0. Order qty: 0
    #if( ( df$est_stock[i] - ss <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {
    if( ( df$est_stock[i] - ss - min_stock <  ( sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i])) )  & ( df$is_order[i] == 1 )  )  {  
      #if( ( df$est_stock[i] <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {  
      df$delivered[lt_i] <- max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]) - df$est_stock[i] + ss),0)
    }
    return(demand(df, i+1, n, ss, lt, of, lost_sale = lost_sale))
  }
  else {
    return(df)
  }
}

# ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 2, N = 2, lost_sale = TRUE)
# ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 2, N = 1, lost_sale = FALSE)

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
  if( est_stock[1] - ss  < sum(demand_p[2:op]) - sum(delivered[2:op]) ){
    delivered[lt] <- max(ceiling((demand_p[2:op]) - est_stock[1] + ss),0) #ceiling(ss) #ceiling(lt*(lambda) + ss)
  }
  
  data <- data.frame(demand_a = demand_a, demand_p = demand_p, est_stock, acc_est_stock, delivered, is_order, is_delivery)
  
  return(data)
}

# Calculate the service level with cf.

cf_sl <- function(demand_a,demand_p, ss, st, lt, of, lost_sale = TRUE){
  n <- length(demand_a)
  df <- gen_demand_data(demand_a, demand_p, ss,st, lt, of)
  
  output <- demand(df,2, n, ss, lt, of, lost_sale )
  output$ss_ratio <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss = ss,st = x)})
  
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
  output$sl <- sl
  return(output)
  #return(list(ServiceLevel = sl, output$est_stock))
}

## Helper functions ##
## Random TS ##
random_ts <- function(years, lambda = 10, plot = FALSE){
  n <- years*360
  w <- n/30
  lambda <- runif(1,1,100)
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
cf_data <- function(year, lambda, h, lt, of, lost_sale = TRUE){
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
  
  
  output <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of, lost_sale  = lost_sale)})
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

lt_of <- function(order_period){
  if(order_period == 1){
    min <- 1
    max <- 7
  }
  else if(order_period == 2){
    min <- 7
    max <- 14
  }
  else if(order_period == 3){
    min <- 14
    max <- 30 
  }
  my_sample <- cumsum(runif(2))
  my_sample <- c(0, my_sample/max(my_sample))
  lt_of_sim <- round(diff(my_sample) * runif(1, min, (max-1))) + 1
  return(lt_of_sim)
}


ss_sl_sim <- function(year,lambda, h, lt, of, lost_sale = TRUE){
  sim <- cf_data(year,lambda, h, lt, of, lost_sale = lost_sale)
  tt <- ncol(sim)
  SS <- sapply(1:tt, FUN= function(x){mean(sim[,x]$ss_ratio)})
  SL <- sapply(1:tt, FUN= function(x){mean(sim[,x]$sl)})
  #return(cbind(SS,SL))
  return(list(SS = SS, SL = SL))
}



# Random order period
## order_period = c("short", "medium", "long")
ss_sl_sim_OP <- function(year,lambda, h, order_period = 1, lost_sale = TRUE){
  OP <- lt_of(order_period)
  lt <- OP[1]
  of <- OP[2]
  sim <- cf_data(year,lambda, h, lt, of, lost_sale = lost_sale)
  tt <- ncol(sim)
  SS <- sapply(1:tt, FUN= function(x){mean(sim[,x]$ss_ratio)})
  SL <- sapply(1:tt, FUN= function(x){mean(sim[,x]$sl)})
  #return(cbind(SS,SL))
  return(list(SS = SS, SL = SL))
}

#ss_sl_sim(4, 10, 12, 7,7)

#test_output <- lapply(rep(4,10), FUN = function(x){ss_sl_sim(x,10,12,7,7)})
#test_output
#x.length <- length(test_output)

#ss_mat <- t(sapply(1:x.length, FUN =function(x){test_output[[x]][["SS"]]}))
#sl_mat <- t(sapply(1:x.length, FUN =function(x){test_output[[x]][["SL"]]}))
#colMeans(sl_mat)
#sl_mat

ss_sl_sim_repeat <- function(year, lambda, h, lt, of, N, lost_sale = TRUE){
  list_output <- lapply(rep(4,N), FUN = function(x){ss_sl_sim(x,lambda,h,lt,of, lost_sale)})  
  ll <- length(list_output) #Should be the same length as N
  ss_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SS"]]})))
  sl_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SL"]]})))
  names(ss_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  names(sl_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  return(list(SafteyStock = ss_mat, ServiceLevel = sl_mat))
}


ss_sl_sim_repeat_OP <- function(year, lambda, h,  order_period = 1, N = 10, lost_sale = TRUE){
  list_output <- lapply(rep(4,N), FUN = function(x){ss_sl_sim_OP(x,lambda,h,order_period, lost_sale)})  
  ll <- length(list_output) #Should be the same length as N
  ss_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SS"]]})))
  sl_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SL"]]})))
  names(ss_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  names(sl_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  return(list(SafteyStock = ss_mat, ServiceLevel = sl_mat))
}

start_time <- Sys.time()

random_OP <- ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 1, N = 1000, lost_sale = TRUE)
random_OP2 <- ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 2, N = 1000, lost_sale = TRUE)
random_OP3 <- ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 3, N = 1000, lost_sale = TRUE)


random_OP_l <- ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 1, N = 1000, lost_sale = FALSE)
random_OP2_l <- ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 2, N = 1000, lost_sale = FALSE)
random_OP3_l <- ss_sl_sim_repeat_OP(year = 4,lambda = 10, h = 12,order_period = 3, N = 1000, lost_sale = FALSE)


end_time <- Sys.time()
# get$ServiceLevel
# get3 <- ss_sl_sim_repeat(4,10,12,7,7,6, FALSE)


# Long order periods

long1 <- ss_sl_sim_repeat(6, lambda = 10, h = 36, lt = 180, of = 180, N = 500) # 1 year OP
long2 <- ss_sl_sim_repeat(6, lambda = 10, h = 36, lt = 135, of = 135, N = 500) # 9 months
long3 <- ss_sl_sim_repeat(6, lambda = 10, h = 36, lt = 90, of = 90, N = 500) # 6 months
long4 <- ss_sl_sim_repeat(6, lambda = 10, h = 36, lt = 60, of = 60, N = 500) # 4 months
long5 <- ss_sl_sim_repeat(6, lambda = 10, h = 36, lt = 45, of = 45, N = 500) # 3 months
long6 <- ss_sl_sim_repeat(6, lambda = 10, h = 36, lt = 30, of = 30, N = 500) # 2 months


print_result(long1)
print_result(long2)
print_result(long3)
print_result(long4)
print_result(long5)
print_result(long6)


rbind(melt(get$ServiceLevel), melt(get3$ServiceLevel))
melt(get$ServiceLevel)
melt(get3$ServiceLevel)

res <- melt(get$ServiceLevel)

print_result <- function(res){
  CF <-colnames(res$SafteyStock)
  SS <- signif(unname(colMeans(res$SafteyStock)),2)
  SL <- signif(unname(colMeans(res$ServiceLevel)),2)
  return(kable(cbind(CF,SS,SL)))
}

print_result(random_OP)
print_result(random_OP2)
print_result(random_OP3)

print_result(random_OP_l)
print_result(random_OP2_l)
print_result(random_OP3_l)


debgugging <- function(){
  length(db)/360
  3*360
  
  db.tr <- db[1:1080]
  db.te <- db[-c(1:1080)]
  pred <- fit_model(db.tr, 24)
  period <- 30
  y_train <-  unname(tapply(db.tr, (seq_along(db.tr)-1) %/% period, sum))  
  model <- fit_model(y_train, 24)
  demand_p.m <- model$pred
  sigma2 <- model$sigma2/30^2
  
  demand_a <- db.te# Transform data to daily
  demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
  lt <- 90
  of <- 30
  st <- sum(demand_p[1:lt])
  ss <- sqrt(sigma2)*qnorm(1-(1-0.95)/2)*sqrt(of+lt)
  db_data <- gen_demand_data(demand_a, demand_p, ss = ss, st = st, of = of, lt = lt)
  db_data_post <- demand(db_data, 2, n = length(demand_a), ss = ss, lt = lt, of = of)
  db_data_post 
}


# get_1_1 <- ss_sl_sim_repeat(4,10,24,1,1,1000)
# get_7_7 <- ss_sl_sim_repeat(4,10,24,7,7,1000)
# lost_sale <- FALSE
# get_7_7_lost <- ss_sl_sim_repeat(4,10,12,7,7,1000)
# get2_lost <- ss_sl_sim_repeat(4,10,12,1,1,1000)
# get60_lost <- ss_sl_sim_repeat(4,10,24,30,30,1000)

# get_30_30 <- ss_sl_sim_repeat(4,10,24,30,30,1000)
# lost_sale <- TRUE
# colMeans(get_1_1$ServiceLevel)
# colMeans(get_1_1$SafteyStock)
# colMeans(get_30_30$SafteyStock)

## Effect of order periods ##
meanTable <- as.data.frame(cbind(get_table(get2_lost$ServiceLevel), 
                                 get_table(get_7_7_lost$ServiceLevel), 
                                 get_table(get60_lost$ServiceLevel)))
names(meanTable) <- c("Short", "Medium", "Long")

kable(meanTable)

## Plot results 
plot_results <- function(res, e = "SL"){
  
  
  zeta <- c(0.5,0.6,0.7,0.8,0.9,0.95,0.995)
  if(e == "SL"){
    df <- res$ServiceLevel
    xlabel <- "Service Level"
  }
  else {
    df <- res$SafetyStock
    xlabel <- "Safety Stock"
  }
  names(df) <- paste(round(100*zeta, 2), "%", sep="")
  df <- melt(df)
  p <- ggplot(df, aes(x = `value`,y = `variable`, height = ..density..)) + 
    #geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, trim = TRUE) +
    #geom_density_ridges(stat = "density", alpha = 0.3,scale = 2, rel_min_height = 0.01, trim = TRUE) +
    geom_density_ridges(stat = "density", alpha = 0.3) +
    scale_fill_viridis(name = "Temp. [F]", option = "C") + 
    xlab(xlabel) + 
    ylab("Confidence Factor") +
    #xlim(0.45, 1)+
    scale_x_continuous(breaks = seq(0.1, 1, 0.1), limits = c(0.5, 1))+
    labs(title = 'Order period: 14 days') +
    theme_ipsum() +
    theme(
      legend.position="right",
      panel.spacing = unit(0.1, "lines"),
      plot.title = element_text(size=12),
      strip.text.x = element_text(size = 8)
    )
  return(p)
}
plot_results(random_OP3)
ttt <- get_30_30$SafteyStock
names(ttt) <- paste(round(100*zeta, 2), "%", sep="")
ttt<- melt(ttt)
ggplot(ttt, aes(x = `value`,y = `variable`, height = ..density..)) + 
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
hist(get_30_30$SafteyStock[,7])
#library(export)
#library(officer)
#library(rvg)
# Create a new powerpoint document
#doc <- read_pptx()
#doc <- add_slide(doc, 'Title and Content', 'Office Theme')

# Add the plot
#doc <- officer::ph_with(value = p) 

# Write the document to a file
#print(doc, target = 'plot.pptx')

(OP <- seq(1,13))
l.rt <- OP/(OP +rev(OP))

LT.ratio <- t(sapply(OP, FUN = function(x){colMeans(ss_sl_sim_repeat(4,10,12,lt = 14-x,of = x, N = 100 )$ServiceLevel)}))

#LT.df <- as.data.frame(LT.ratio)
#row.names(LT.df) <- OP/(OP +rev(OP))
LT.df <- melt(LT.ratio)
LT.df$LT_Ratio <- rep(l.rt, 7)

names(LT.df) <- c("RunID", "CF", "SL", "LT_Ratio")



sapply(OP, FUN = function(x){14-x})
length(OP)
x.x <- seq(2,30)

seq(2,30,30/2)
seq(1,30-1,floor(30/2))
t <- seq(2,30)
sapply(t, FUN = function(x) {seq(1,x-1,floor(x/3))})

OP_sim <- function(year, lambda, h, N, OP){
  OP.x <- seq(1,OP-1, floor(OP/3))
  LT.ratio <- t(sapply(OP.x, FUN = function(x){colMeans(ss_sl_sim_repeat(year,lambda,h,lt = OP-x,of = x, N = N )$ServiceLevel)}))
  return(LT.ratio)
}
OP.x <- seq(2,30)
# res <- lapply(OP.x, FUN = function(x){OP_sim(4,10,12,100, OP = x)})

melt(res[[5]])
res[[10]]
LT <- c()
n.l <- length(res)

for(i in 1:n.l){
  OP <- i+1
  xx <- seq(1,OP-1,floor(OP/3))
  temp <- melt(res[[i]])
  temp$OP <- OP
  temp$LT <- OP - rep(xx,7)
  temp$LT.ratio <- 1-(rep(xx, 7)/OP)
  LT <- rbind(LT, temp)
}
LT <- as.data.frame(LT)
names(LT) <- c("RunId", "CF", "SL", "OP", "LT", "LT_ratio")

LT <- transform(LT, group = cut(LT_ratio, breaks = c(0,0.25,0.5,0.75,1), labels = c("0-25%", "25-50%", "50-75%", "75-100%")))

ggplot(LT) + 
  #geom_line(aes(x = OP, y = SL, color = CF)) +
  geom_point(aes(x = OP, y = SL), size = 1) + 
  theme_bw() + 
  #facet_wrap(~group, scales = "free")
  facet_grid(CF~group)

ggplot(LT) + 
  #geom_point(aes(x = OP, y = LT_ratio, color = SL), size = 1) + 
  geom_point(aes(x = OP, y = SL, size = LT_ratio, color = LT_ratio), alpha = 0.5) +
  scale_color_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.2)) + 
  guides(color= guide_legend(), size=guide_legend())+
  scale_size_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.2)) +
  #scale_x_continuous(breaks=seq(from=2, to = 30, by= 5))+
  labs(y = "Service Level", x = "Order Period Days", color = "LT/OP", size = "LT/OP") + 
  theme_bw() +
  facet_grid(~CF)

ggplot(LT) + 
  #geom_point(aes(x = OP, y = LT_ratio, color = SL), size = 1) + 
  geom_point(aes(x = OP, y = SL, size = LT_ratio, color = CF), alpha = 0.5) +
  #scale_color_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.2)) + 
  guides(color= guide_legend(), size=guide_legend())+
  scale_size_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.2)) +
  #scale_x_continuous(breaks=seq(from=2, to = 30, by= 5))+
  labs(y = "Service Level", x = "Order Period Days", color = "LT/OP", size = "LT/OP") + 
  theme_bw() 
  #facet_grid(~CF)




## Run simulations

mult_sim_data_frame <- function(years, N, lambda = 10, h = 12, lt = 7, of  = 7){
  multiple_sim <- t(sapply(rep(years,N), FUN = function(x){colMeans(cf_data(year = x, lambda = lambda, h = h, lt, of))}))
  m.sim <- as.data.frame(multiple_sim)
  #names(m.sim) <- paste(c(0.5,0.6,0.7,0.8,0.9,0.95,0.995))
  #m.df <- melt(m.sim)
  return(m.sim)
}


