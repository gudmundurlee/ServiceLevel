# library
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

# data
df <- read_excel("~/AGR Dynamics/ServiceLevel/Data/plot_data_test_sql.xlsx")
str(df)
df$mth_name <- factor(df$mth_name, levels = month.abb)

df_sub <- df[df$value < 100,]
df_sub
ggplot(df_sub, aes(x = `value`, y = `mth_name`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Distribution of sales') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


lambda <- 1
rnorm(1, mean = lambda, sd = sqrt(lambda))

x <- rnorm(100000, lambda, sqrt(lambda))

Z <- function(lambda){
  zz <- rnorm(1, lambda, sqrt(lambda))
  return(zz)
}

# We can estimate a normal distribution with a poisson distribution

x <- seq(-5,5,0.01)
#
plot(x =x, y = dnorm(x), type = 'l')
# d
# Cumilitvie function
plot(x = x, pnorm(x), type = 'l')
pnorm(0)
px <- seq(0,1,0.001)
plot(x = px, y = qnorm(px),type = 'l')
## Stack overflow:
my_time <- seq(0,20,0.1)
my_data <- (sin(my_time))
decomp <- stl(ts(my_data, frequency =  201/(20/(2*pi))), s.window = 25, l.window = 64)

df <- cbind(time = my_time, as.data.frame(decomp$time.series))

library(ggplot2)

ggplot(df, aes(time, seasonal)) +
  geom_line(aes(color = "Seasonal")) +
  geom_line(aes(y = trend, color = "Trend")) +
  geom_line(aes(y = remainder, color = "Remainder"))


sqrt(120)
sqrt(30)
sqrt(120/30)
sqrt(120)/sqrt(30)
