# Student name:  Julia Tsai

# place the R code for each question in the space provided below.
# type the answer at the end of each question
# full credit for each question is given if the R code runs and produces the answer

######################################################
###### REMEMBER  TO UPLOAD YOUR R SUBMISSION FILE#####
######################################################

# Useful R functions
library(jrvFinance)
library(data.table)
library(ggplot2)
# useful functions in jrvFinance
# bond.price(settle,mature,coupon,freq,yield,convention,comp.freq)
# bond.yield(settle,mature,coupon,freq,price,convention,comp.freq)
# bond.TCF(settle,mature,coupon,freq,convention)
# coupons.dates(settle,mature,freq)
# bond.duration(settle,mature,coupon,freq,yield,convention,modified=FALSE,comp.freq)
# irr(cf,interval=NULL,cf.freq=1,comp.freq=1,method="default")
# 

# define DATE() function
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}
# zero coupon price function
zcb.price <- function(zcb.yield,ttm,freq=2) {
  return( 100/(1+zcb.yield/freq)^(freq*ttm) )  
}
# zero coupon yield function
zcb.yield <- function(zcb.price,ttm,freq=2) {
  return( freq * ( (100/zcb.price)^(1/(freq*ttm)) - 1 ) )  
}
# function to calculate convexity of a coupon bond
bond.convexity <- function(settle,mature,coupon,freq=2,yield,convention,comp.freq=freq) {
  z  <- as.data.frame(bond.TCF(settle,mature,coupon,freq,convention))
  cf <- z$cf
  t  <- z$t
  r  <- yield
  m  <- comp.freq
  return ( 1/sum( cf / (1+r/m)^(t*m) ) * sum( t * (t+1/m) * cf / (1 + r/m)^(t*m+2) ) )
}

#----------------------------------------------------
## Multiple choice
# Q3
Z <- function(ttm){
  Z <- 0.04 + 0.01 * I(log(1+ttm))
  return(Z)
}
CI <- fread("./datas/cstrips.csv")
CI[, ttm := as.numeric(maturity - as.IDate("2019-11-15"))/365]
CI[, Z := Z(ttm)]

ggplot(CI, aes(x = ttm, y = Z))+
  geom_point()

# Q4
settle <- DATE(2019,01,01)
mature <- DATE(2024,01,01)
couponA <- 0.04
yieldA <- 0.05
bond.duration(settle, mature, couponA, freq = 2, yieldA, "ACT/ACT", modified = TRUE, 2)

ZB <- 0.05
5 * (1/(1+ZB/2))

# Q6
PortA <- list(Dmod = 5, Conv = 15)
PortB <- list(Dmod = 6, Conv = 16)
Rchange <- 0.015

-PortA$Dmod * Rchange + 0.5 * PortA$Conv * Rchange^2
-PortB$Dmod * Rchange + 0.5 * PortB$Conv * Rchange^2

# Q7
fund <- 2000000
repo <- 0.05
fund *repo *1/360

# Q8
face <- 1000000
mature <- DATE(2024,12,26)
settle <- DATE(2024,01,19)
BDR <- 0.0525
d <- as.numeric(mature - settle)
face * (1 - BDR * d/360)

# Q11
target <- 5
Da <- 3.4
Db <- 7.2
Wa <- (target - Db)/(Da - Db)
1-Wa

#----------------------------------------------------
# start question 12
# enter your two digit code: 61
Q12 <- fread("./datas/midterm/midterm_Q2_61.csv")
settle <- DATE(2024,01,19)
# end   question 12

#----------------------------------------------------
# start question 13
Q12
pclean <- bond.price(settle, Q12$maturity, Q12$coupon, freq =2, Q12$yield, "ACT/ACT", 2)
accint <- bond.TCF(settle, Q12$maturity, Q12$coupon, 2, "ACT/ACT")$accrued
pfull <- pclean + accint
# answer:
round(pfull, 4)
# end   question 13
#----------------------------------------------------

# start question 14
convexity <- bond.convexity(settle, Q12$maturity, Q12$coupon,
               freq = 2, Q12$yield, "ACT/ACT", 2)
# answer:
round(convexity, 2)
# end   question 14
#----------------------------------------------------

# start question 15
ci <- fread("./datas/midterm/midterm_ci.csv")
sp <- fread("./datas/midterm/midterm_sp.csv")
settle <- DATE(2024,01,19)
face <- 100

Q15 <- data.table(date = coupons.dates(settle, Q12$maturity, freq = 2))
N <- nrow(Q15)
Q15$coupon <- face*Q12$coupon
Q15[, principal := c(rep(0, N-1), face)]
Q15 <- merge(Q15, ci, by.x = "date", by.y = "maturity", all.x = TRUE)
Q15 <- merge(Q15, sp, by.x = "date", by.y = "maturity", all.x = TRUE)

arb.bid <- sum(Q15$coupon * Q15$ci_pbid/100) + sum(Q15$principal[N] * Q15$sp_pbid[N]/100)

# answer:
round(arb.bid, 4)
# end   question 15
#----------------------------------------------------

# start question 16
Q16 <- fread("./datas/midterm/midterm_Q3.csv")
settle <- DATE(2024,01,19)
freq <- 2

price <- NA
for (i in 1:nrow(Q16)){
  mature <- Q16$maturity[i]
  coupon <- Q16$coupon[i]
  yield <- Q16$yield[i]
  price[i] <- bond.price(settle, mature, coupon, freq, yield, "ACT/ACT", freq)
}
Q16$pclean <- price
Q16$market.value <- Q16$face * price /100
port.market.value <- sum(Q16$market.value)
# answer:
round(port.market.value)
# end   question 16
#----------------------------------------------------

# start question 17
for (i in 1:nrow(Q16)){
  mature <- Q16$maturity[i]
  coupon <- Q16$coupon[i]
  yield <- Q16$yield[i]
  Q16$Dmod[i] <- bond.duration(settle, mature, coupon, freq, yield, "ACT/ACT", modified = TRUE, freq)
}

port.dmod <- sum(Q16$market.value/port.market.value * Q16$Dmod)
# answer:
round(port.dmod, 3)
# end   question 17
#----------------------------------------------------

# start question 18
accint <- NA
for (i in 1:nrow(Q16)){
  mature <- Q16$maturity[i]
  coupon <- Q16$coupon[i]
  yield <- Q16$yield[i]
  accint[i] <- bond.TCF(settle, mature, coupon, freq, "ACT/ACT")$accrued
}

Q16$pfull <- Q16$pclean + accint
bondsCF <- list()
for (i in 1:nrow(Q16)){
  mature <- Q16$maturity[i]
  coupon <- Q16$coupon[i]
  date <- coupons.dates(settle, mature, freq)
  cf <- bond.TCF(settle, mature, coupon, freq, "ACT/ACT")$cf
  disfac <- rep(1, length(date))
  disfac[1] <- Q16$pfull[1]/(100+Q16$coupon[1]*100/2)
  for (j in 2:length(date)) {
    cumdf <- sum(disfac[1:j-1])    
    disfac[j] <- ( Q16$pfull[i] - Q16$coupon[i]*100/2 * cumdf ) / 
      (100 + Q16$coupon[i]*100/2 )
  }
  bondsCF[[i]] <- data.table(date, cf, disfac)
  Q16$cf.pv[i] <- sum(cf *disfac)
}
YTM <- sum(Q16$cf.pv)/port.market.value
# answer:
round(YTM, 4)
# end   question 18
#----------------------------------------------------

# start question 19
ci <- fread("./datas/midterm/midterm_ci.csv")
settle <- DATE(2024,01,19)
ci[, maturity := as.Date2(maturity)]
ci[, ttm := as.numeric(maturity - settle)/365]
ci[, spot := zcb.yield(ci_pmid, ttm)]

Y.reg.ci <- lm(spot ~ ttm + I(ttm^2) + I(ttm^3) + I(ttm^4) + I(ttm^5), data = ci)
adj.r2.ci <- summary(Y.reg.ci)$adj.r.squared

# answer:
round(adj.r2.ci, 4)
# end   question 19
#----------------------------------------------------

# start question 20
sp <- fread("./datas/midterm/midterm_sp.csv")
settle <- DATE(2024,01,19)
sp[, maturity := as.Date2(maturity)]
sp[, ttm := as.numeric(maturity - settle)/365]
sp[, spot := zcb.yield(sp_pmid, ttm)]

Y.reg.sp <- lm(spot ~ ttm + I(ttm^2) + I(ttm^3) + I(ttm^4) + I(ttm^5), data = sp)
adj.r2.sp <- summary(Y.reg.sp)$adj.r.squared

# answer:
round(adj.r2.sp, 4)
# end   question 20
#----------------------------------------------------

# start question 21
mid.61 <- fread("./datas/midterm/midterm_Q2_61.csv")
settle <- DATE(2024,01,19)
face <- 100

CFs <- data.table( date = coupons.dates(settle, mid.61$maturity, freq))
N <- nrow(CFs)
CFs[, ttm := as.numeric(date - settle)/365]
CFs[, coupon := face * coupon]
CFs[, principal := c(rep(0, N-1), face)]
CFs$ci_spot <- predict(Y.reg.ci, newdata = CFs[,"ttm"])
CFs$sp_spot <- predict(Y.reg.sp, newdata = CFs[,"ttm"])
CFs[, ci.pv := zcb.price(ci_spot, ttm, freq)/100 * coupon]
CFs[, sp.pv := zcb.price(sp_spot, ttm, freq)/100 * principal]

PV.61 <- sum(CFs$ci.pv) + sum(CFs$sp.pv)

# answer:
round(PV.61, 4)
# end   question 21
#----------------------------------------------------

#       question 22
######################################################
###### REMEMBER  TO UPLOAD YOUR R SUBMISSION FILE#####
######################################################