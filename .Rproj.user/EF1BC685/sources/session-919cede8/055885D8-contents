library(readr)
library(ggplot2)
library(lubridate)
library(tidyverse)

Q2STRIPS <- read_csv("TeamAssignment1_Q2_strips.csv")
Q3data <- read_csv("TeamAssignment1_Q3.csv")

settlement <- as.Date("2023-12-15")

# use the coupon stips only for discount rate
Q2STRIPS_ci <- filter(Q2STRIPS, type == "ci")
Q2STRIPS_ci$price <-  (Q2STRIPS_ci$pbid + Q2STRIPS_ci$pask)/2

## Discount Factor
Q2STRIPS_ci$discount <- Q2STRIPS_ci$price/100

## Interporation
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


#######  3a #######
## Which payout option, A or B, has the highest value?

optionA_value <- sum(Q3data$Amount * Q3data$Discount)
optionB_value <- 79000000

print(c(optionA_value, optionB_value))

## Payout option A has the highest value



















