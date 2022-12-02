demo <- read_excel("AGR Dynamics/ServiceLevel/Safety Stock/demo.xlsx")

y <- demo$value

y.ts <- ts(y, start = c(2017,3), frequency  =12)

plot(y.ts)


library(smooth)
library(greybox)

e.y <- es(y.ts)
es(y.ts, h = 12, holdout = TRUE, silent = FALSE)
