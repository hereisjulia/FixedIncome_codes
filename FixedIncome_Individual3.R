# Student name:  Julia Tsai

library(data.table)
library(tidyverse)
library(jrvFinance)
library(quantmod)
zcb.price <- function(zcb.yield, ttm, freq = 2){
  return(100 /(1 - zcb.yield /freq) ^ (freq*ttm))
}
zcb.yield <- function(zcb.price, ttm, freq = 2){
  return(freq * ((100 /zcb.price) ^(1 /(freq*ttm))-1))
}


# sample R file to upload for individual assignment
# please place the R code for each question in the space provided below.


#----------------------------------------------------

# start question 1
settle <- as.Date("2024-01-10")
freq <- 2
CI <- fread("./datas/IndividualAssignment3_cstrips.csv")
CI[, ttm := as.numeric(maturity - as.IDate(settle))/365]
CI[, z := zcb.yield(CI$price, CI$ttm, freq)]

t1 <- as.numeric(as.Date("2024-08-15")-settle)/365
t2 <- as.numeric(as.Date("2025-02-15")-settle)/365

## invest 1 dollar from t0 to t1 to t2
fv.t1 <- (1+ CI[ttm == t1]$z/freq) ^ (freq*t1)
fv.t2 <- (1+ CI[ttm == t2]$z/freq) ^ (freq*t2)
# FV.t2 = fv.t1 * (1 + fwr/freq) ^ (freq * (t2-t1))
fwr.t1_2 <- freq * ((fv.t2/fv.t1)^(1/freq/(t2-t1))-1)


fwr.t1_2
# end   question 1
#----------------------------------------------------

# start question 2
sofr = getSymbols('SOFR',src='FRED',auto.assign=FALSE)
sofr = na.omit(sofr)
sofr = as.data.table(sofr)
names(sofr) = c('Date','SOFR')
sofr[, SOFR := SOFR/100]

t1 <- as.Date("2023-09-01")
t2 <- as.Date("2023-12-31")

if (!(t1 %in% sofr$Date)){
  tmp = data.table(Date = t1, SOFR = NA)
  sofr = rbind(sofr,tmp)
}
if (!(t2 %in% sofr$Date)){
  tmp = data.table(Date = t2, SOFR = NA)
  sofr = rbind(sofr,tmp)
}

setorder(sofr, Date)
sofr[, SOFR:= na.locf(SOFR)] # fill in the missing dates with the previous day SOFR

data <- sofr[Date >= t1 & Date <= t2]
data[, di := c(as.numeric(diff(Date)), 0)]
data[, gross_daily_rate := 1 + SOFR*di/360]
data[, cum_prod := cumprod(gross_daily_rate)]
tail(data)

D <- sum(data$di)
SOFR_average <- round((data[(.N -1),]$cum_prod[1] -1) * 100 * 360 /D, 5)
SOFR_average

Interest <- 2000000 * (SOFR_average/100 + 0.02) * as.numeric(t2-t1) / 360
round(Interest, 2)
# end   question 2
#----------------------------------------------------

# start question 3
t1 <- as.Date("2023-06-21")
t2 <- as.Date("2023-09-20")
sofr = getSymbols('SOFR',src='FRED',auto.assign=FALSE)
sofr = na.omit(sofr)
sofr = as.data.table(sofr)
names(sofr) = c('Date','SOFR')
sofr[, SOFR := SOFR/100]
if (!(t1 %in% sofr$Date)){
  tmp = data.table(Date = t1, SOFR = NA)
  sofr = rbind(sofr,tmp)
}
if (!(t2 %in% sofr$Date)){
  tmp = data.table(Date = t2, SOFR = NA)
  sofr = rbind(sofr,tmp)
}
data <- sofr[Date >= t1 & Date <= t2]
data[, di := c(as.numeric(diff(Date)), 0)]
data[, gross_daily_rate := 1 + SOFR*di/360]
data[, cum_prod := cumprod(gross_daily_rate)]
D <- sum(data$di)
R <- (data[(.N -1)]$cum_prod[1] - 1) * 360/D *100
round(R, 4) 

SR3_final <- 100 - R
round(SR3_final, 4)

# end   question 3
#----------------------------------------------------

# start question 4
sr3fut <- fread("./datas/IndividualAssignment3_sr3fut.csv")

t0 <- as.Date("2024-01-09")
t1 <- as.Date("2027-12-15")
t2 <- as.Date("2028-03-15")
SR_init <- sr3fut[StartDt == t1]$SettlePrice
Re <- 100 - SR_init
Re


# end   question 4
#----------------------------------------------------

# start question 5
t0 <- as.Date("2024-01-09")
t1 <- as.Date("2027-12-15")
t1.5 <- as.Date("2028-03-15")
t2 <- as.Date("2028-06-21")

Re1 <- Re
Re2 <- 100 - sr3fut[StartDt == as.Date("2028-03-15")]$SettlePrice

data <- data.table(R = c(Re1, Re2),
                   di = as.numeric(c( t1.5 - t1, t2 - t1.5 )))
data[, gross_daily_rate := 1 + R*di/360]
data[, cum_prod := cumprod(gross_daily_rate)]
D <- sum(data$di)

R <- (data[.N]$cum_prod - 1) *360/D

round(R, 4)
# end   question 5
#----------------------------------------------------



