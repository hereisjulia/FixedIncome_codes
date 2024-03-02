# Student name:  Julia Tsai

################################################################################
# Follow the "R Submission File Instructions" page on Canvas to receive 5 points:
# -use setwd() to set the working directory
# -do not include the working directory as part of the name of a file
# -do not use file.choose()
# -use load() to load RData files
# -place R code for each question in the space provided below
# -replace NA for each answer with the R code generating the answer
# -type the numerical answer on Canvas
################################################################################

# Full credit on numerical questions is given if:
# - the R code in the appropriate place for that question run
# - the R code generates the answer matching the numerical answer typed into Canvas
# - the numerical answer is correct

####################################################################################
###### REMEMBER TO UPLOAD YOUR R SUBMISSION FILE
###### On time submission via Canvas will receive a five (5) point submission credit.
###### Submission via email will not receive the five (5) point submission credit.
###### Submission via email after five minutes of the due time will not be accepted.
####################################################################################

# Useful R functions
library(jrvFinance)
library(data.table)
library(quantmod)
# useful functions in jrvFinance
# bond.price(settle,mature,coupon,freq,yield,convention,comp.freq)
# bond.yield(settle,mature,coupon,freq,price,convention,comp.freq)
# bond.TCF(settle,mature,coupon,freq,convention)
# coupons.dates(settle,mature,freq)
# bond.duration(settle,mature,coupon,freq,yield,convention,modified=FALSE,comp.freq)
# irr(cf,interval=NULL,cf.freq=1,comp.freq=1,method="default")
# 
setwd("E:/MQM_Courses/Term4/FixedIncomeSecurities/FixedIncome_codes/datas")

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
zcb.price <- function(zcb.yield,ttm,comp.freq=2) {
  return( 100/(1+zcb.yield/comp.freq)^(comp.freq*ttm) )
}
# zero coupon yield function
zcb.yield <- function(zcb.price,ttm,comp.freq=2) {
  return( comp.freq * ( (100/zcb.price)^(1/(comp.freq*ttm)) - 1 ) )
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

##########################
##### File Download  #####
##########################

load('final.RData',verbose=TRUE)   # do not use file.choose()
# this is the output 
# Loading objects:
#   Treasury_ci
#   Treasury_bond
#   sofr.frbny
#   sr3.fut
#   liab.partb
#   qtree
#   ztree
#

# Treasury_ci has two columns:
#  maturity   is the maturity date of the coupon STRIPS, in 'Date' format
#  price      is the price of the coupon STRIPS per $100 of face value

# Treasury_bond has four columns:
#   maturity   is the maturity date of the treasury bond, in 'Date' format
#   coupon     is the annual coupon rate of the treasury bond, in decimal form
#   pclean     is the clean price of the treasury bond, per $100 of face value
#   yield      is the yield-to-maturity of the treasury bond, in decimal form, semi-annual compounded

# sr3.fut has four columns:
#   MMY           is the year and month of the futures (format as YYYYMM).
#   StartDt       is the start date of the reference quarter of the SOFR 3-month futures contract
#   MatDt         is the end date of the reference quarter of the SOFR 3-month futures contract
#   SettlePrice   is the settlement price of these futures contract, on 2024-02-12.

# sofr.nyfrb has two columns:
#   Date          is the date of the SOFR, as computed by the Federal Reserve Bank of New York
#   SOFR          is the SOFR for the given date

# qtree is a 10 x 10 matrix of up probabilities

# ztree is a 10 x 10 matrix of 1-period spot rates



##################
##### Part B #####
##################

# start question 10
settle <- DATE(2024,02,12)
Treasury_ci[, ttm := as.numeric(maturity - settle)/365]
liab.partb[, ttm := as.numeric(date - settle)/365]
Treasury_ci[, spot := zcb.yield(price, ttm)]
liab.partb$spot <- spline(x=Treasury_ci$ttm, y=Treasury_ci$spot, xout=liab.partb$ttm,method="natural")$y
liab.partb[, disfac := 1/(1+spot/2) ^ (2*ttm)]
liab.partb[, pv := amount*disfac]
PV <- sum(liab.partb$pv)
liab.partb[, wi := pv/PV]
liab.partb[, wtcon := wi * ttm * (ttm + 0.5)/ (1 + spot/2)^2]
convexity <- sum(liab.partb$wtcon)

answer10 = round(convexity,3)         ### replace NA with R code generating the answer
cat("Answer Q10:", answer10,'\n')  
# end   question 10
#----------------------------------------------------
# start question 11
bond.ladder <- Treasury_bond[maturity %in% liab.partb$date,]
bond.ladder$F[12] <- liab.partb$amount[12]/(1+bond.ladder$coupon[12])
bond.ladder$cum.coupon[12] <- bond.ladder$coupon[12]*bond.ladder$F[12]

for (i in seq((nrow(liab.partb)-1),1)){
  bond.ladder$F[i] = (liab.partb$amount[i] - bond.ladder$cum.coupon[i+1])/(1+bond.ladder$coupon[i])
  bond.ladder$cum.coupon[i] = bond.ladder$cum.coupon[i+1] + bond.ladder$F[i] * bond.ladder$coupon[i]
}
tot.cost <- sum(bond.ladder$F)

answer11 = tot.cost              ### replace NA with R code generating the answer
cat("Answer Q11:", answer11,'\n')  
# end   question 11
#----------------------------------------------------

##################
##### Part C #####
##################

# start question 12
settle <- DATE(2024,02,12)
# enter your two digit code: 70

fixed.rate <- 0.045
notional <- 100000000
start.date <- DATE(2023,12,20)
end.date <- DATE(2027,12,20)

payments <- data.table(year = seq(0,4),
           paydate = seq(start.date, end.date, by = "year"))
payments$D <- NA
for (i in 2:nrow(payments)){
  payments$D[i] <- as.numeric(payments$paydate[i] - payments$paydate[i-1])
}
payments[,fixed.cf := fixed.rate * D/360 * notional]

float <- sofr.frbny[date >= start.date & date <= end.date]
float[, di := c(as.numeric(diff(date)),0)]
float[.N, di:= as.numeric(end.date - float[.N]$date)]
float[, gross_daily_rate := 1 + SOFR/100*di/360 ]
float[, cum_rate := cumprod(gross_daily_rate)]
float.cf <- (float[.N]$cum_rate - 1) * payments[2]$D/360 * notional

answer12 = round(float.cf)              ### replace NA with R code generating the answer
cat("Answer Q12:", answer12,'\n')  
# end   question 12
#----------------------------------------------------
# start question 13

sr3.fut[, forw := (100 - SettlePrice)/100 ]
sofr.frbny
Dfac = data.table(date=c(sr3.fut$StartDt[1],sr3.fut$MatDt))
Dfac[, di := c(0,as.numeric(diff(date)))]
Dfac[, forw:= c(0,sr3.fut$forw)]
Dfac[, disfac := 1]
for (i in 2:nrow(Dfac)) {
  Dfac$disfac[i] = Dfac$disfac[i-1]/(1+Dfac$forw[i]*Dfac$di[i]/360)
}
payments[, disfac := spline(Dfac$date, Dfac$disfac, xout = paydate, method="natural")$y]
payments[, pv := fixed.cf*disfac]

market.value <- sum(na.omit(payments)$pv)

answer13 = round(market.value)                   ### replace NA with R code generating the answer
cat("Answer Q13:", answer13,'\n')  
# end   question 13
#----------------------------------------------------

# start question 14


answer14 = NA                   ### replace NA with R code generating the answer
cat("Answer Q14:", answer14,'\n')  
# end   question 14
#----------------------------------------------------

##################
##### Part D #####
##################

# start question 15
load("merton_70.RData")
merton_data
B <- 500
T <- 2
V0 <- 800
set.seed(437123)




answer15 = NA                   ### replace NA with the two digit code you received
cat("Answer Q15:", answer15,'\n')  
# end   question 15
#----------------------------------------------------

# start question 16

answer16 = NA                   ### replace NA with R code generating the answer
cat("Answer Q16:", answer16,'\n')  
# end   question 16
#----------------------------------------------------

# start question 17

answer17 = NA                   ### replace NA with R code generating the answer
cat("Answer Q17:", answer17,'\n')  
# end   question 17
#----------------------------------------------------
# start question 18

answer18 = NA                   ### replace NA with R code generating the answer
cat("Answer Q18:", answer185,'\n')  
# end   question 18
#----------------------------------------------------

##################
##### Part E #####
##################

# start question 19
VasicekZBP <- function(r0, kappa, theta, sigma, years) {
  # Calculates zero coupon bond price for Vasicek model. 
  #
  # Args: 
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   years: The length or maturity of the bond.  
  #
  # Returns:
  #   A decimal price of the bond (i.e. 0.98 for 98). 
  
  b.vas <- (1-exp(-years*kappa)) / kappa
  a.vas <- (theta-sigma^2/(2*kappa^2))*(years-b.vas)+(sigma^2)/(4*kappa)*b.vas^2
  return(exp(-a.vas-b.vas*r0))
}

prt.tree <- function(tree,digit=2) {
  nt <- nrow(tree)
  # transpose tree
  trantree <- t(tree)
  nt1 <- 2*nt-1
  bintree  <- matrix(rep("",nt1*nt),nrow=nt1,ncol=nt)
  # convert to bin tree
  i1 <- nt
  for (j in 1:nt) {
    i1 <- nt-j+1
    for (i in 1:j) {
      bintree[i1,j] <- as.character(round(trantree[i,j],digit))
      i1 <- i1 + 2
    }
  }
  rownames(bintree) <- rep("",nt1)
  colnames(bintree) <- rep("",nt)
  return(noquote(bintree))
}
prt.tree(ztree)

answer19 = ztree[3,2]                 ### replace NA with R code generating the answer
cat("Answer Q19:", answer19,'\n') 
# end   question 19
#----------------------------------------------------

# start question 20
N <- 8
m <- 2   
delt  <- 0.5
deltm <- delt*m
ptree <- matrix(0,nrow=N+1, ncol=N+1)             
ptree[N+1,c(1:(N+1))] = rep(100,(N+1))


for (i in N:1) {
  i1 = i+1
  ptree[i,1:i] = (qtree[i,1:i]*(ptree[i+1,1:i])+
                    (1-qtree[i,1:i])*(ptree[i+1,2:i1]))/(1+ztree[i,1:i]/m)^deltm
}
prt.tree(ptree)

answer20 =  round(ptree[1,1],3)         ### replace NA with R code generating the answer
cat("Answer Q20:", answer20,'\n') 
# end   question 20
#----------------------------------------------------

# start question 21
N <- 6
m <- 2   
delt  <- 0.5
deltm <- delt*m
ptree <- matrix(0,nrow=N+1, ncol=N+1)             
ptree[N+1,c(1:(N+1))] = rep(100,(N+1))


for (i in N:1) {
  i1 = i+1
  ptree[i,1:i] = (qtree[i,1:i]*(ptree[i+1,1:i])+
                    (1-qtree[i,1:i])*(ptree[i+1,2:i1]))/(1+ztree[i,1:i]/m)^deltm
}
prt.tree(ptree)

answer21 = round(ptree[1,1],3)     ### replace NA with R code generating the answer
cat("Answer Q21:", answer21,'\n') 
# end   question 21
#----------------------------------------------------
# start question 22
N <- 8
m <- 2   
delt  <- 0.5
deltm <- delt*m
ptree <- matrix(0,nrow=N+1, ncol=N+1)             
ptree[N+1,c(1:(N+1))] = rep(100,(N+1))

for (i in N:1) {
  i1 = i+1
  ptree[i,1:i] = (qtree[i,1:i]*(ptree[i+1,1:i])+
                    (1-qtree[i,1:i])*(ptree[i+1,2:i1]))/(1+ztree[i,1:i]/m)^deltm
}
prt.tree(ptree)
nodes <- 1+2+3+4+5+6+7+8+9

answer22 = nodes              ### replace NA with R code generating the answer
cat("Answer Q22:", answer22,'\n') 
# end   question 22
#----------------------------------------------------

##################
##### Part F #####
##################
#       question 23
#######################################################
###### REMEMBER  TO UPLOAD YOUR R SUBMISSION FILE #####
#######################################################