library(readr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(jrvFinance)

Q2STRIPS <- read_csv("TeamAssignment1_Q2_strips.csv")
Q3data <- read_csv("TeamAssignment1_Q3.csv")

settlement <- as.Date("2023-12-15")

# use the coupon strips for discount rate
Q2STRIPS_ci <- filter(Q2STRIPS, type == "ci")
Q2STRIPS_ci$price <-  (Q2STRIPS_ci$pbid + Q2STRIPS_ci$pask)/2

## Interporation on Discount Factor
Q2STRIPS_ci$discount <- Q2STRIPS_ci$price/100

ttm <- as.numeric((Q2STRIPS_ci$maturity - settlement)/365)
newttm <- as.numeric((Q3data$Date - settlement)/365)
cubic_spline <- as.data.frame(spline(as.numeric(ttm), 
                                     Q2STRIPS_ci$discount, 
                                     xout = newttm,
                                     method = "natural"))
Q3data$Discount <- cubic_spline$y

### plot the interporation
names(cubic_spline) <- c("ttm", "discount")
cubic_spline$type <- "interporation"

interporation <- data.frame(
  ttm = as.numeric(ttm),
  discount = Q2STRIPS_ci$discount,
  type = "origin")
interporation <- rbind(interporation,cubic_spline)

ggplot(interporation)+
  geom_point(aes(x = ttm, y = discount, color = type))+
  labs(title = "Cubic spline on discount factors")+
  theme_minimal()


#######  3a  #######
## Which payout option, A or B, has the highest value?

optionA_value <- sum(Q3data$Amount * Q3data$Discount)
optionB_value <- 79000000

print(c(optionA_value, optionB_value))

print(paste("Payout option A has a value of", round(optionA_value,2), ".This is higher than 79000000 of option B"))


#######  3b  #######
cubic_pbid <- as.data.frame(spline(as.numeric(ttm), 
                                   Q2STRIPS_ci$pbid, 
                                   xout = newttm,
                                   method = "natural"))
cubic_bidask <- data.frame(Q3data, pbid = cubic_pbid$y)

q3b_target <- cubic_bidask[cubic_bidask$Date == "2052-06-30",]
q3b_amount <- optionA_value/q3b_target$Discount/100
q3b_sell <- q3b_amount * q3b_target$pbid

print(paste("the lottery winner receive", round(q3b_sell,2), "from selling coupon STRIPS.", "This is", round(optionA_value - q3b_sell,2), "lower than the payment PV 79862180."))

#######  3c  #######
cubic_pask <- as.data.frame(spline(as.numeric(ttm), 
                                   Q2STRIPS_ci$pask, 
                                   xout = newttm,
                                   method = "natural"))

cubic_bidask <- data.frame(Q3data, pbid = cubic_pbid$y, pask = cubic_pask$y)
cubic_bidask$Dept_fund_unit <- cubic_bidask$Amount/100
cubic_bidask$Dept_fund <- cubic_bidask$Dept_fund_unit * cubic_bidask$pask


print(paste("The payment department need to fund:", round(sum(cubic_bidask$Dept_fund),2),
            ". They will spend", round(sum(cubic_bidask$Dept_fund) - optionA_value,2), "more than the lottery PV", round(optionA_value,2)))








