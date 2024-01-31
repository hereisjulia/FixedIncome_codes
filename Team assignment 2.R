library(data.table)
library(jrvFinance)
library(ggplot2)
library(tidyverse)


spot_rate <- function(price, ttm, comp.freq = 2){
  ttm <- as.numeric(ttm)
  return(comp.freq * ( (100/price)^(1/comp.freq/ttm) - 1 ) )
}
zcb.price <- function(zcb.yield,ttm,comp.freq=2) {
  return( 100/(1+zcb.yield/comp.freq)^(comp.freq*ttm) )
}
# Q2

# 2a) What are the estimated parameters? [Hint: replace the gfun4() function using the above equation. Then try the starting value of (0.01,0,0,0,0,0) for the six parameters.]

Q2_bonds <- fread("./datas/TeamAssignment2_Q2_bond.csv")
settle <- as.IDate("2023-12-15")
Q2_bonds$ttm <- as.numeric(Q2_bonds$maturity - settle)


Q2_bonds$spot_rate <- spot_rate(Q2_bonds$pclean, Q2_bonds$ttm)


q2a_model <- lm(spot_rate ~ ttm + I(ttm^2) + I(ttm^3) + I(ttm^4) + I(ttm^5), data = Q2_bonds)
theta <- q2a_model$coefficients
theta

# 2b) Compare the actual prices and predicted prices of the bonds based on estimated parameters in part 2a.

nonlin_yld_reg <- data.table(ttm = Q2_bonds$ttm,
                             actual_price = Q2_bonds$pclean,
                             act_spot_rate = Q2_bonds$spot_rate)
nonlin_yld_reg$pred_spot_rate <- predict(q2a_model, newdata = Q2_bonds[,"ttm"])
nonlin_yld_reg$predict_price <- zcb.price(nonlin_yld_reg$pred_spot_rate, nonlin_yld_reg$ttm)


# plot it
plot1 <- pivot_longer(nonlin_yld_reg[,c(1,2,5)], cols = -1, names_to = "type", values_to = "price")
ggplot(plot1)+
  geom_point(aes(x = ttm, y = price, color = type))+
  labs(title = "Actual & Predicted Price")+
  theme_minimal()


# 2c) Use the estimated parameters to “predict” the prices of the coupon strips in the data file:
# TeamAssignment2_Q2_ci.csv. It contains the following information for US treasury securities:
Q2_ci <- fread("./datas/TeamAssignment2_Q2_ci.csv")
Q2_ci$ttm <- as.numeric(Q2_ci$maturity - settle)

Q2_ci$pred_spot_rate <- predict(q2a_model, newdata = Q2_ci[,"ttm"])
Q2_ci$pred_price <- zcb.price(Q2_ci$pred_spot_rate, Q2_ci$ttm)

plot2 <- pivot_longer(Q2_ci[,c(2,3,5)], col = -2, names_to = "type", values_to = "price")
ggplot(plot2)+
  geom_point(aes(x = ttm, y = price, color = type))


# 2d) Use the estimated parameters to “predict” the prices of the principal strips in the data 
# file: TeamAssignment2_Q2_sp.csv. It contains the following information for US treasury securities:

Q2_sp <- fread("./datas/TeamAssignment2_Q2_sp.csv")
Q2_sp[, ttm := as.numeric(maturity - settle)]
Q2_sp$pred_spot_rate <- predict(q2a_model, newdata = Q2_sp[,"ttm"])
Q2_sp$pred_price <- zcb.price(Q2_sp$pred_spot_rate, Q2_sp$ttm)

plot3 <- pivot_longer(Q2_sp[,c(2,3,5)], col = -2, names_to = "type", values_to = "price")
ggplot(plot3)+
  geom_point(aes(x = ttm, y = price, color = type))


# 2e) Discuss how the prediction errors differ for coupon strips (in 2c) and principal strips 
# (in 2d).







#################################################
