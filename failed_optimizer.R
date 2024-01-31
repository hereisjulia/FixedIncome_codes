library(data.table)
library(jrvFinance)
library(ggplot2)
library(tidyverse)
library(optimx)


# gfun will gives:
## CFs
## ttm
## Z(t)
## D(ttm.n)
## Model Price = sum(Di * Cfi)\
settle <- as.Date("2023-12-15")
freq <- 2

gfun4 <- function(ttm, parm){
  tmp <- parm[1] + parm[2]*ttm + parm[3]*ttm^2 + parm[4]*ttm^3 + parm[5]*ttm^4 + parm[6]*ttm^5
  return(tmp)
}

ssr <- function(parm, data){
  for (i in 1:nrow(data)){
    cfi <- bond.TCF(settle, data$maturity[i], data$coupon[i], freq, "ACT/ACT")$cf
    cf.d <- coupons.dates(settle, data$maturity[i], freq)
    ttmi <- as.numeric(cf.d - settle)/365
    rates <- sapply(ttmi, gfun4, parm = parm)
    disfac <- 1/(1+rates/2)^(ttmi*2)
    data$phat[i] <- sum(disfac*cfi)
  }
  ztest <- sum((data$pfull - data$phat)^2)
  if (is.nan(ztest)) ztest <- 9e20
  return(list(data, ztest))
}

# add full price into data
Q2_bonds <- fread("./datas/TeamAssignment2_Q2_bond.csv")
accint <- NA
for (i in 1:nrow(Q2_bonds)){
  accint[i] <- bond.TCF(settle, Q2_bonds$maturity[i], Q2_bonds$coupon[i], freq = 2, "ACT/ACT")$accrued
  Q2_bonds$pfull[i] <- Q2_bonds$pclean[i]+accint[i]
}
Q2_bonds$ttm <- as.numeric(Q2_bonds$maturity - as.IDate(settle))/365
# 2a) What are the estimated parameters? 
# [Hint: replace the gfun4() function using the above equation. 
# Then try the starting value of (0.01,0,0,0,0,0) for the six parameters.]
q2a_model <- lm(spot_rate ~ ttm + I(ttm^2) + I(ttm^3) + I(ttm^4) + I(ttm^5), data = bonds)

parm = start
data = Q2_bonds


# use ssr
ssr(parm, data)



theta <- q2a_model$coefficients
theta


ggplot(Q2_bonds)+
  geom_point(aes(x = maturity, y = phat))+
  geom_point(aes(x = maturity, y = pfull), color = "red")

# run optimizer




# 2b) Compare the actual prices and predicted prices of the bonds 
# based on estimated parameters in part 2a.




















# use ssr2
methods.fast <- c("L-BFGS-B") #, "BFGS", "Nelder-Mead", , "nlm", "nlminb", "Rvmmin"


n <- length(bond.TCF(settle, data[.N,]$maturity, data[.N,]$coupon, freq, "ACT/ACT")$cf)
cf.mat <- matrix(0, nrow = nrow(data), ncol = n+1)
ttm.mat <- cf.mat
ncf.mat <- matrix(0, nrow = nrow(data), ncol = 1)
for (i in 1:nrow(data)){
  cfi <- bond.TCF(settle, data$maturity[i], data$coupon[i], freq, "ACT/ACT")$cf
  cf.d <- coupons.dates(settle, data$maturity[i], freq)
  ttmi <- as.numeric(cf.d - settle)/365
  ncf.mat[i] <- length(cfi)
  cf.mat[i, c(1:length(cfi))] <- cfi
  ttm.mat[i, c(1:length(ttmi))] <- ttmi
}
ssr2 <- function(parm, data) {
  for(i in 1:nrow(data)){
    ncf <- ncf.mat[i]
    cfi <- cf.mat[i, c(1:ncf)]
    ttmi <- ttm.mat[i, c(1:ncf)]
    rates <- sapply(ttmi, gfun4, parm = parm)
    disfac <- 1/(1+rates/2)^(ttmi^2)
    data$phat[i] <- sum(disfac*cfi)
  }
  ztest <- sum((data$pfull - data$phat)^2)
  if (is.nan(ztest)) ztest <- 9e20
  return(ztest)
}
ssr2(parm, data)

opt4x <- optimx(start, ssr, data = data, method = methods.fast,
                control = list(maxit  = 10000))

