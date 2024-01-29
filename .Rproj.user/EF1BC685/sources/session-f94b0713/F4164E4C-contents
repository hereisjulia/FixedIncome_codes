library(data.table)
library(jrvFinance)

# Q1
Q1data <- fread("./datas/TeamAssignment2_Q1.csv")

settle <- as.Date("2023-12-15")
freq <- 2

# need to calculate yield to maturity first.

bond.duration(settle,
              Q1data$maturity[1],
              Q1data$coupon[1],
              freq,yield,"ACT/ACT",modified = TRUE,
              comp.freq = freq)

# 需要計算的: ytm, market value of each bond to get the weights.




