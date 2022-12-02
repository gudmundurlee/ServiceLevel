

library(seastests)
library(readxl)
library(tidyverse)
sale_frequency <- read_excel("AGR Dynamics/ServiceLevel/sale_frequency.xlsx")

sales_plot <- function(item){
  
  p.df <- sale_frequency %>% filter(sale_frequency$item_id == item & value != 0)
  
  plt <- ggplot(p.df) + geom_bar(aes(x=value, y=count), stat="identity")
  return(plt)
}

sales_plot(15)

df <- read_excel("AGR Dynamics/ServiceLevel/agr_63_demo_new.xlsx")
str(df)
item <- df %>% select(.,sale) %>% filter(df$item_id == 1)
y_start <- df %>% select(.,year) %>% filter(df$item_id == 1) %>% min
m_start <- df %>% select(.,month) %>% filter(df$item_id == 1 & df$year == y_start) %>% min 
item_to_ts <- function(item){
  
  y <- df %>% select(.,sale) %>% filter(df$item_id == item)
  y_start <- df %>% select(.,year) %>% filter(df$item_id == item) %>% min
  m_start <- df %>% select(.,month) %>% filter(df$item_id == item & df$year == y_start) %>% min 
  y.ts <- ts(y, frequency =  12, start = c(y_start, m_start))
}
y1 <- item_to_ts(7)
plot(decompose(y1))
## Analysis of time series
stats::filter(y1)

f <- c(0.5, rep_len(1,5),0.5)/7
d.y <- decompose(y1, type = "additive", filter = NULL)
plot(d.y)

yy <- y1-d.y$seasonal - d.y$trend

plot(yy)
yy

library(forecast)
fit.ts <- tslm(y1~trend + season)
fit.tss <- tslm(y1~trend:season)
summary(fit.tss)
fit.s <- tslm(y1~season)
fit.t <- tslm(y1~trend)
fit.0 <- tslm(y1~1)
summary(fit)
anova(fit.0,fit.s)
anova(fit.s, fit.ts)
## Seasonal test of a time series with frequency 12


is_seasonal <- function(y, item){
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

seasonal_regression <- function(y){
  n <- length(y)
  
  x <- seq(1,n)
  season <-  as.factor(((x-1) %% 12)+1)
  df <- data.frame(y = y, Time = x, Month = season)
  fit0 <- lm(y~Time, data = df)
  fit1 <- lm(y~Time+Month, data = df)
  
  
  
  s_test <- anova(fit0, fit1)
  
  p_value <- s_test$`Pr(>F)`[2]
  
  return(p_value)

}

is_seasonal_item <- function(item){
  y.ts <- item_to_ts(item)
  if(length(y.ts) >= 24 & sd(y.ts) > 0){
    t.c <- tryCatch(unname(combined_test(y.ts)$Pval[3]), error = function(e) 1)
    t.qs <- tryCatch(unname(qs(y.ts)$Pval), error = function(e) 1)
    t.f <- tryCatch(unname(fried(y.ts)$Pval), error = function(e) 1)
    t.kw <- tryCatch(unname(kw(y.ts)$Pval), error = function(e) 1)
    t.s <-  tryCatch(unname(seasdum(y.ts)$Pval), error = function(e) 1)
    t.w <-  tryCatch(unname(welch(y.ts)$Pval), error = function(e) 1)
    t.lm <-  tryCatch(seasonal_regression(y.ts), error=function(e) 1)
    s_df <- data.frame(Item = item, Combined = t.c, QS = t.f, Fried = t.f, KW = t.kw, Seasdum = t.s, Welch = t.w, Regression = t.lm)
    return(s_df)
  }
  else {
    return(data.frame(Item = item, Combined = 1, QS = 1, Fried = 1, KW = 1, Seasdum = 1, Welch = 1, Regression = 1))
  }
}
items <- unique(df$item_id)
# Run this without any problems
start_time = Sys.time()
seasonal_items <- t(sapply(items, FUN = is_seasonal_item))
end_time = Sys.time()
head(seasonal_items,40)

plot(item_to_ts(24))

s_items_agr <- df %>% select(., item_id, seasonality) %>% unique


s_df <- as.data.frame(seasonal_items)
# dataframe 1
ss_df <- data.frame(
  Item = unlist(s_df$Item),
  Combined = unlist(s_df$Combined),
  QS = unlist(s_df$QS),
  Fried = unlist(s_df$Fried),
  KW = unlist(s_df$KW),
  Seasdum = unlist(s_df$Seasdum),
  Welch = unlist(s_df$Welch),
  Regression = unlist(s_df$Regression)
)

head(ss_df)
sapply(ss_df$Combined , FUN = p_calc)
# Dataframe 2
sp_df <- data.frame(
  Item = unlist(s_df$Item),
  AGR_Seasonal = sapply(s_items_agr$seasonality,FUN = is.logical_strings),
  Combined = sapply(unlist(s_df$Combined), FUN = p_calc),
  QS = sapply(unlist(s_df$QS), FUN = p_calc),
  Fried = sapply(unlist(s_df$Fried), FUN = p_calc),
  KW = sapply(unlist(s_df$KW), FUN = p_calc),
  Seasdum = sapply(unlist(s_df$Seasdum), FUN = p_calc),
  Welch = sapply(unlist(s_df$Welch), FUN = p_calc),
  Regression = sapply(unlist(s_df$Regression), FUN = p_calc)
)


sp_df$seasonal_count <- rowSums(sp_df[,3:9])
head(sp_df,24)
is.logical_strings <- function(x){
  if(x == "False"){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

p_calc <- function(x,p = 0.01){
  if(is.nan(x) == TRUE | is.na(x) == TRUE) {
    return(FALSE)
  }
  else if(x < p) {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
apply(ss_df[,-1],2, FUN = p_calc)
data.frame(ss_df[,1], apply(ss_df[,-1], 2, FUN = p_calc))

p_values <- seasonal_items[,-1]
p_values[is.na(p_values)] <- 1
#p_values[p_values < 0.01] <- TRUE
#p_values[p_values >= 0.01] <- FALSE
output <- cbind(s_items_agr, p_values)
head(output, 40)
s_items_agr
output
library("writexl")
write_xlsx(sp_df,"AGR Dynamics/ServiceLevel/output.xlsx")
write.csv(sp_df,"AGR Dynamics/ServiceLevel/output2.csv", row.names = FALSE)
