library(data.table)
library(jrvFinance)
library(ggplot2)
library(tidyverse)

zcb.price <- function(zcb.yield,ttm,comp.freq=2) {
  return( 100/(1+zcb.yield/comp.freq)^(comp.freq*ttm) )
}

zcb.yield <- function(zcb.price,ttm,comp.freq=2) {
  return( comp.freq * ( (100/zcb.price)^(1/(comp.freq*ttm)) - 1 ) )
}

# Q2

# 2a) What are the estimated parameters? [Hint: replace the gfun4() function using the above equation. Then try the starting value of (0.01,0,0,0,0,0) for the six parameters.]

Q2_bonds <- fread("./datas/TeamAssignment2_Q2_bond.csv")
settle <- as.IDate("2023-12-15")
Q2_bonds$ttm <- as.numeric(Q2_bonds$maturity - settle)/365
freq <- 2

# get full price
accint <- NA
for (i in 1:nrow(Q2_bonds)){
  accint[i] <- bond.TCF(settle, Q2_bonds$maturity[i], Q2_bonds$coupon[i], freq = 2, "ACT/ACT")$accrued
  Q2_bonds$pfull[i] <- Q2_bonds$pclean[i]+accint[i]
}

bonds <- Q2_bonds[maturity>settle,]

# get discount factor, then use zcb.yield to get spot rate
D <- rep(0, nrow(bonds))
D[1] <- bonds$pfull[1]/(100 + bonds$coupon[1] *100 /freq)

for (i in 2:nrow(bonds)){
  pfull <- bonds$pfull[i]
  coupon <- bonds$coupon[i]
  D[i] <- (pfull - sum(D[1:(i-1)])*coupon*100/freq)/(100 + coupon*100/freq)
}
bonds$disfac <- D
bonds$spot_rate <- zcb.yield(bonds$disfac*100, bonds$ttm)

q2a_model <- lm(spot_rate ~ ttm + I(ttm^2) + I(ttm^3) + I(ttm^4) + I(ttm^5), data = bonds)
parm <- q2a_model$coefficients
parm

gfun4 <- function(ttm, parm){
  tmp <- parm[1] + parm[2]*ttm + parm[3]*ttm^2 + parm[4]*ttm^3 + parm[5]*ttm^4 + parm[6]*ttm^5
  return(tmp)
}

ssr <- function(parm, data){
  for (i in 1:nrow(data)){
    cfi <- bond.TCF(settle, data$maturity[i], data$coupon[i], freq, "ACT/ACT")$cf
    cf.d <- coupons.dates(settle, data$maturity[i], freq)
    ttmi <- as.numeric(cf.d - settle)/365
    rates <- sapply(ttmi, gfun4, parm = parm)
    disfac <- 1/(1+rates/2)^(ttmi*2)
    data$phat[i] <- sum(disfac*cfi)
  }
  ztest <- sum((data$pfull - data$phat)^2)
  if (is.nan(ztest)) ztest <- 9e20
  return(list(data, ztest))
}

predictions <- ssr(parm, bonds)
bonds <- predictions[[1]]


# 2b) Compare the actual prices and predicted prices of the bonds based on estimated parameters in part 2a.

nonlin_reg <- data.table(ttm = bonds$ttm,
                             actual_price = bonds$pfull,
                             predict_price = bonds$phat)

# price plot
plot1 <- pivot_longer(nonlin_reg, cols = -1, names_to = "type", values_to = "price")
ggplot(plot1)+
  geom_point(aes(x = ttm, y = price, color = type))+
  labs(title = "Actual & Predicted price of Bonds")+
  theme_minimal()



# 2c) Use the estimated parameters to “predict” the prices of the coupon strips in the data file:
# TeamAssignment2_Q2_ci.csv. It contains the following information for US treasury securities:
Q2_ci <- fread("./datas/TeamAssignment2_Q2_ci.csv")
Q2_ci$ttm <- as.numeric(Q2_ci$maturity - settle)/365

Q2_ci$pred_spot_rate <- predict(q2a_model, newdata = Q2_ci[,"ttm"])
Q2_ci$pred_price <- zcb.price(Q2_ci$pred_spot_rate, Q2_ci$ttm)

plot <- pivot_longer(Q2_ci[,c(3,4,6)], cols = -1, names_to = "type", values_to = "spot_rate")
ggplot(plot)+
  geom_point(aes(x = ttm, y = spot_rate, color = type))+
  labs(title = "Actual & Predicted price of spot rate")+
  theme_minimal()


plot2 <- pivot_longer(Q2_ci[,c(2,3,5)], col = -2, names_to = "type", values_to = "price")
ggplot(plot2)+
  geom_point(aes(x = ttm, y = price, color = type))+
  labs(title = "Actual & Predicted price of CI strips")+
  theme_minimal()


# 2d) Use the estimated parameters to “predict” the prices of the principal strips in the data 
# file: TeamAssignment2_Q2_sp.csv. It contains the following information for US treasury securities:

Q2_sp <- fread("./datas/TeamAssignment2_Q2_sp.csv")
Q2_sp[, ttm := as.numeric(maturity - settle)/365]
Q2_sp$pred_spot_rate <- predict(q2a_model, newdata = Q2_sp[,"ttm"])
Q2_sp$pred_price <- zcb.price(Q2_sp$pred_spot_rate, Q2_sp$ttm)

plot3 <- pivot_longer(Q2_sp[,c(2,3,5)], col = -2, names_to = "type", values_to = "price")
ggplot(plot3)+
  geom_point(aes(x = ttm, y = price, color = type))+
  labs(title = "Actual & Predicted price of SP strips")+
  theme_minimal()