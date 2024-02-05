# sample R file to upload for individual midterm exam
library(data.table)
library(ggplot2)
library(optimx)

library(jrvFinance)
# useful functions in jrvFinance
# bond.price(settle,mature,coupon,freq,yield,convention,comp.freq)
# bond.yield(settle,mature,coupon,freq,price,convention,comp.freq)
# bond.TCF(settle,mature,coupon,freq,convention)
# bond.duration(settle,mature,coupon,freq=2,yield,convention,modified=FALSE,comp.freq=freq)
# coupons.dates(settle,mature,freq)
# irr(cf,interval,cf.freq,comp.freq,cf.t)
# 

# define DATE() function
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}

# function to calculate convexity
bond.convexity <- function(settle,mature,coupon,freq=2,yield,convention,comp.freq=freq) {
  z  <- as.data.frame(bond.TCF(settle,mature,coupon,freq,convention))
  cf <- z$cf
  t  <- z$t
  r  <- yield
  m  <- comp.freq
  return ( 1/sum( cf / (1+r/m)^(t*m) ) * sum( t * (t+1/m) * cf / (1 + r/m)^(t*m+2) ) )
}

# start question 1

# end   question 1
#----------------------------------------------------


# start question 2

# end   question 2
#----------------------------------------------------



# start question 3

# end   question 3
#----------------------------------------------------



# start question 4

# end   question 4
#----------------------------------------------------



# start question 5

# end   question 5
#----------------------------------------------------



# start question 6

# end   question 6
#----------------------------------------------------


# add question blocks if necessary
