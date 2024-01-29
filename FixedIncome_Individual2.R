# Student name:  Julia Tsai

# sample R file to upload for individual assignment
# please place the R code for each question in the space provided below.

library(data.table)
library(jrvFinance)
library(tidyverse)

#----------------------------------------------------

# start question 1
settle <- as.Date("2023-12-15")
maturity <- as.Date("2024-10-03")
BDR <- 0.04745
d <- as.numeric(maturity-settle)
1000000 * ( 1 - BDR * d/360)

# end   question 1
#----------------------------------------------------

# start question 2
CI <- fread("IndividualAssignment02-ci.csv")
SP <- fread("IndividualAssignment02-sp.csv")

settle <- as.Date("2023-12-15")
maturity2 <- as.Date("2052-05-15")
coupon <- 0.02875
freq <- 2

Discount <- CI[maturity %in% coupons.dates(settle, maturity2, freq)]$price /100
TCF <- bond.TCF(settle,maturity2,coupon,freq)
TCF$cf[57] <- TCF$cf[57]-100

sum(TCF$cf * Discount) + SP[maturity == maturity2][,2] - TCF$accrued

# end   question 2
#----------------------------------------------------

# start question 3
settle <- as.Date("2023-12-15")
maturity3 <- as.Date("2053-02-15")
face <- 10000000
coupon3 <- 0.03625
cleanP <- 92.2190
haircut <- 0.02
reporate <- 0.0531
freq <- 2

fullP <- cleanP + bond.TCF(settle, maturity3, coupon3, freq, "ACT/ACT")$accrued
Value_of_position <- fullP/100 *face
repo_amount <- Value_of_position * (1 - haircut)

repoint <- repo_amount * reporate * 1/360
round(repoint,2)
# end   question 3
#----------------------------------------------------

# start question 4
pension_payment <-  data.table(`Payment Date` = c("2024-05-15", "2025-05-15","2026-05-15","2027-05-15","2028-05-15"),
           `Payment Amount` = 100000)

pension_payment_PV <- CI[maturity %in% pension_payment$`Payment Date`,2]/100 * pension_payment$`Payment Amount`
sum(pension_payment_PV)
# end   question 4
#----------------------------------------------------

# start question 5
settle <- as.Date("2023-12-15")
pension_payment2 <-  data.table(`Payment Date` = c("2024-06-30", "2025-06-30","2026-06-30","2027-06-30","2028-06-30"),
                               `Payment Amount` = 100000)
pension_payment2$`Payment Date` <- as.Date(pension_payment2$`Payment Date`)
zcb.yield <- function(zcb.price,ttm,comp.freq=2) {
  return( comp.freq * ( (100/zcb.price)^(1/(comp.freq*ttm)) - 1 ) )
}
CI$spot_rate <- zcb.yield(CI$price, ttm, comp.freq = 4)

zcb.price <- function(zcb.yield,ttm,comp.freq=2) {
 return( 100/(1+zcb.yield/comp.freq)^(comp.freq*ttm) )
 }

ttm <- as.numeric(CI$maturity - as.IDate(settle))

newttm <- as.numeric(pension_payment2$`Payment Date` - settle)
pension_payment2$spot_rate <- spline(ttm, CI$spot_rate, xout = newttm, method = "natural")$y
pension_payment2$price <- zcb.price(pension_payment2$spot_rate, newttm, comp.freq = 4)
pension_payment2$PV <- pension_payment2$`Payment Amount`*pension_payment2$price /100
round(sum(pension_payment2$PV))

# end   question 5
#----------------------------------------------------



