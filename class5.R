library(data.table)
library(jrvFinance)

zcb.yield <- function(zcb.price,ttm,comp.freq=2) {
  return( comp.freq * ( (100/zcb.price)^(1/(comp.freq*ttm)) - 1 ) )
}
#### following the class
settle <- as.IDate("2023-12-15")
CI <- fread("./datas/IndividualAssignment02-ci.csv")

CI[, ttm    := as.numeric(maturity - settle)/365]
CI[, disfac := price/100]
CI[, spot   := zcb.yield(price, ttm)]

# calc the Dmod and convexity

CI[, Dmod  := ttm/(1+spot/2)]
CI[, Cnvx  := ttm*(ttm+1/2)/(1+spot/2)^2]

CI




#######################################
# With K bonds
nparm = nrow(data)
Dmod <- data$Dmod
wts <- rep(1/nparm, nparm)

names(wts) <- c("1y", "2y", "5y", "10y", "30y")

lb <- rep(0, nparm)
ub 

Target <- liab.Dmod

#objective function


#gradient



