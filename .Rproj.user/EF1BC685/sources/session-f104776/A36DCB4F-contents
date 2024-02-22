# Student name:  Julia Tsai

# sample R file to upload for individual assignment
# please place the R code for each question in the space provided below.
library(data.table)
library(jrvFinance)
library(ggplot2)
library(quantmod)
setwd("E:/MQM_Courses/Term4/FixedIncomeSecurities/FixedIncome_codes/datas")

load("IndividualAssignment4_sr3fut.RData")
###### question 2 start ######

settle <- as.Date("2022-12-14")
notional <- 20000000
fixed.rate <- 0.049

df1 <- data.table(year = seq(0,2,by = 1),
                  pay.date = seq(as.Date("2022-12-14"), as.Date("2024-12-14"), by = "year"))
for (i in 1:nrow(df1)){
  if (i == 1) {
    df1$D[i] <- as.numeric(df1$pay.date[i] -settle)
  } else{
    df1$D[i] <- as.numeric(df1$pay.date[i] -df1$pay.date[i-1])
  }
}
df1[, fixed.cf := fixed.rate * D/360 * notional]


answer2 = df1[pay.date == "2023-12-14",fixed.cf]                     ###### replace with the numerical answer for question 2
cat("Answer Q2:", answer2)        
###### question 2 end   ######

###### question 3 start ######

settle <- as.Date("2024-06-30")

sr3.fut[, forw := 1 - SettlePrice/100]

sofr <- getSymbols('SOFR',src='FRED',auto.assign=FALSE)

sr3.fut[3]

t1 <- sr3.fut$StartDt[3]
t2 <- settle
t1
t2
sr3.fut$MatDt[3]

data <- data.table(date = c(t1,t2),
                   sofr = sofr[[length(sofr)]])
data[, di := c(as.numeric(diff(date)),0)]
data[, gross_daily_rate := 1 + sofr/100*di/360 ]
data[, cum_rate := cumprod(gross_daily_rate) ]
Da <- sum(data$di)
Ra <- (data[(.N-1)]$cum_rate[1] -1)*360/Da*100
Re <- sr3.fut$forw[3]*100

D <- as.numeric(sr3.fut$MatDt[3] - t1)
Rb <- ( (1+Re/100* D/360) / (1+Ra/100 * Da/360) - 1 ) * 360 / (D-Da) * 100

sr3.fut$StartDt[3] = settle
sr3.fut$forw[3] = Rb/100

sr3.fut2 <- sr3.fut[-c(1,2)]

Dfac = data.table(date=c(sr3.fut2$StartDt[1],sr3.fut2$MatDt))
Dfac[, di := c(0,as.numeric(diff(date)))]
Dfac[, forw:= c(0,sr3.fut2$forw)]
Dfac[, disfac := 1]
for (i in 2:nrow(Dfac)) {
  Dfac$disfac[i] = Dfac$disfac[i-1]/(1+Dfac$forw[i]*Dfac$di[i]/360)
}
Dfac[,.(date,di,forw,disfac)]


answer3 = Dfac[date == settle, disfac]                      ###### replace with the numerical answer for question 3
cat("Answer Q3:", answer3)        
###### question 3 end   ######

###### question 4 start ######

sr3.fut

settle <- as.Date("2024-01-09")

t1 <- sr3.fut$StartDt[1]
t2 <- settle
t3 <- sr3.fut$MatDt[1]
c(t1, t2, t3)
sofr <- as.data.table(sofr)
sofr <- na.omit(sofr)
names(sofr) <- c("date", "sofr")

data <- sofr[date >= t1 & date <= t2]

data[, di := c(as.numeric(diff(date)),0)]
data[, gross_daily_rate := 1 + sofr/100*di/360 ]
data[, cum_rate := cumprod(gross_daily_rate) ]
Da <- sum(data$di)
Ra <- (data[(.N-1)]$cum_rate[1] -1)*360/Da*100
Re <- sr3.fut$forw[1]*100

D <- as.numeric(sr3.fut$MatDt[1] - t1)
Rb <- ( (1+Re/100* D/360) / (1+Ra/100 * Da/360) - 1 ) * 360 / (D-Da) * 100

sr3.fut$StartDt[1] = settle
sr3.fut$forw[1] = Rb/100

Dfac = data.table(date=c(sr3.fut$StartDt[1],sr3.fut$MatDt))
Dfac[, di := c(0,as.numeric(diff(date)))]
Dfac[, forw:= c(0,sr3.fut$forw)]
Dfac[, disfac := 1]
for (i in 2:nrow(Dfac)) {
  Dfac$disfac[i] = Dfac$disfac[i-1]/(1+Dfac$forw[i]*Dfac$di[i]/360)
}
Dfac[,.(date,di,forw,disfac)]

swap <- data.table(date=seq(from= as.Date("2024-01-09"),to= as.Date("2028-01-09"),by='year'))
swap[, disfac := as.data.frame(spline(x=Dfac$date,
                                  y=Dfac$disfac,
                                  xout=swap$date,
                                  method='natural')$y)]
swap[, di := c(0,as.numeric(diff(date)))]
swap[, dxD := di*disfac/360]

nr <- nrow(swap)
swap.rate <- (1-swap$disfac[nr])/sum(swap$dxD[2:nr])

answer4 = round(swap.rate,5)                      ###### replace with the numerical answer for question 4
cat("Answer Q4:", answer4)        
###### question 4 end   ######

###### question 5 start ######

m      <- 2       
delt   <- 0.5     
N      <- 2      
deltm  <- delt*m

Nperiod.bond <- function(N){
qtree  = matrix(0, nrow=N+1, ncol=N+1)
for (i in 1:(N+1)) {
  for (j in 1:i) {
    qtree[i,j] = 0.5
  }
}
ztree   = matrix(0, nrow=N+1, ncol=N+1)
ztree[1,1] = 0.03
ztree[2,(1:2)] = c(0.035,0.025)
if (N ==2){
ztree[3,(1:3)] = c(0.04,0.03,0.02)}
prt.tree(ztree)


ptree = matrix(0,nrow=N+1, ncol=N+1)             
ptree[N+1,c(1:(N+1))] = rep(100,(N+1))
prt.tree(ptree)

for (i in N:1) {
  i1 = i+1
  ptree[i,1:i] = (qtree[i,1:i]*(ptree[i+1,1:i])+
                    (1-qtree[i,1:i])*(ptree[i+1,2:i1]))/(1+ztree[i,1:i]/m)^deltm
}
P1 = ptree

return(P1)
}

P1 <- Nperiod.bond(1)
prt.tree(P1,2)

answer5 = round(P1[2,1],2)                      ###### replace with the numerical answer for question 5
cat("Answer Q5:", answer5)        
###### question 5 end   ######

###### question 6 start ######
P2 <- Nperiod.bond(2)
prt.tree(P2,2)

answer6 = round(P2[1,1], 2)                 ###### replace with the numerical answer for question 6
cat("Answer Q6:", answer6)        
###### question 6 end   ######

###### question 7 start ######
N <- 2
citree <- matrix(0,nrow=N+1, ncol=N+1)
C <- 100*0.02/2                                  
for (i in 2:(N+1)) {
  citree[i,c(1:i)] <- rep(C,i)
}
prt.tree(citree)
P2.C <- matrix(0,nrow=N+1, ncol=N+1)  
P2.C[N+1,c(1:(N+1))] <- rep(100,(N+1))  
for (i in N:1) {
  i1 = i+1
  P2.C[i,1:i] = (qtree[i,1:i]*(P2.C[i+1,1:i]+citree[i+1,1:i])+
                    (1-qtree[i,1:i])*(P2.C[i+1,2:i1]+citree[i+1,2:i1]))/(1+ztree[i,1:i]/m)^deltm
}
P2.C 
prt.tree(P2.C,2)

answer7 = round(P2.C[2,2], 2)                    ###### replace with the numerical answer for question 7
cat("Answer Q7:", answer7)        
###### question 7 end   ######



