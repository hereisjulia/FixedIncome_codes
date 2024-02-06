library(data.table)
library(nloptr)

zcb.price <- function(zcb.yield, ttm, freq = 2){
  return(100 /(1 - zcb.yield /freq) ^ (freq*ttm))
}
zcb.yield <- function(zcb.price, ttm, freq = 2){
  return(freq * ((100 /zcb.price) ^(1 /(freq*ttm))-1))
}

############
#### Q2 ####
############
## a
Q2 <- fread("./datas/midterm/Sample_midterm_Q2.csv")

Q2$disfac[1] <- Q2$Price[1]/(100+Q2$Coupon[1]*100/2)
for (i in 2:nrow(Q2)) {
  cumdf <- sum(Q2$disfac[1:i-1])    
  Q2$disfac[i] <- ( Q2$Price[i] - Q2$Coupon[i]*100/2 * cumdf ) / 
    (100 + Q2$Coupon[i]*100/2 )
}
for ( i in 1:nrow(Q2)){
  Q2$spot[i] <- 2*( (1 /Q2$disfac[i])^(1/(2 *Q2$Maturity[i]))-1) 
}

## b
Q2b <- Q2[3, 1:4]
Q2b[, Coupon := 0.05]
Q2b <- data.table(
  Period = seq(0, Q2b$Period, by = 1),
  cf = c(0, rep(100*Q2b$Coupon/2, 2), 100+100*Q2b$Coupon/2),
  disfac = c(0, Q2$disfac[1:3])
)
Q2b[, PV := cf*disfac]
sum(Q2b$PV)

############ 
#### Q4 ####
############
Q4 <- fread("./datas/midterm/Sample_midterm_Q4.csv")

funds <- 100000000
freq <- 2
target_Dmod <- 5

Q4
# portfolio A using 5 and 10 yr T notes
Wa.5yr <- (target_Dmod - Q4$ModDur[5]) / (Q4$ModDur[4] - Q4$ModDur[5])
Portfolio.A <- c(Wa.5yr, 1-Wa.5yr)

# Portfolio B includes 3 and 30 yr T securities
Wb.3yr <- (target_Dmod - Q4$ModDur[6]) / (Q4$ModDur[3] - Q4$ModDur[6])
Portfolio.B <- c(Wb.3yr, 1-Wb.3yr)

Conv.A <- sum(Portfolio.A * Q4$Convexity[c(4,5)])
Conv.B <- sum(Portfolio.B * Q4$Convexity[c(3,6)])

Conv.A > Conv.B
# pick Portfolio B due to higher convexity ()


############ 
#### Q5 ####
############

Q5 <- fread("./datas/midterm/Sample_midterm_Q5.csv")

settle <- as.IDate("2018-12-24")
Q5[, ttm := as.numeric(maturity - settle)/365]

## A
newttm <- as.numeric(as.IDate("2019-07-30") - settle)/365
spline(Q5$ttm, Q5$spot, xout = newttm, method = "natural")$y

## B
Q5_reg1 <- lm(spot ~ ttm + log(ttm), data = Q5)
Reg1 <- function(ttm){
  Z.t <- Q5_reg1$coefficients[[1]] + Q5_reg1$coefficients[[2]]*ttm + Q5_reg1$coefficients[[3]] * log(ttm)
  return(Z.t)
}
Reg1(newttm)

## C
Q5_reg2 <- lm(spot ~ ttm + I(1/ttm), data = Q5)
Reg2 <- function(ttm){
  Z.t <- Q5_reg2$coefficients[[1]] + Q5_reg2$coefficients[[2]]*ttm + Q5_reg2$coefficients[[3]] * 1/ttm
  return(Z.t)
}
Reg2(newttm)

## D
Q5_reg3 <- lm(spot ~ ttm + I(log(1+ttm)) + I(1/(1+ttm) -1), data = Q5)
Reg3 <- function(ttm){
  Z.t <- Q5_reg3$coefficients[[1]] + Q5_reg3$coefficients[[2]]*ttm + 
    Q5_reg3$coefficients[[3]] * log(1+ttm) +
    Q5_reg3$coefficients[[4]] * (1/(1+ttm) -1)
  return(Z.t)
}
Reg3(newttm)



















