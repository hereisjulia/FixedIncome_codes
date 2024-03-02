
library(data.table)
library(jrvFinance)
library(ggplot2)
library(quantmod)

t0 <- as.Date("2023-09-20")
t1 <- as.Date("2023-09-29")
t2 <- as.Date("2023-12-20")
P <- 94.63
Re <- 100-P
Re

# Calculate Ra
sofr <- as.data.table(getSymbols('SOFR',src='FRED',auto.assign=FALSE))
sofr <- sofr[index > t0 & index < t1,]
sofr[,di := c(as.numeric(diff(index)),0)]
sofr[, gross_daily_rate := 1 + SOFR/100*di/360 ]
sofr[, cum_rate := cumprod(gross_daily_rate)]
Da <- sum(sofr$di)
Ra <- (sofr[(.N-1)]$cum_rate -1)*360/Da*100
# Calculate Rb
Db <- as.numeric(t2 - t1)
Rb <- ( (1+Re/100* D/360) / (1+Ra/100 * Da/360) - 1 ) * 360 / (D-Da) * 100


# Q7b
sofr_R <- function(date1,date2,spread=0) {
  if (class(date1)=='character') {date1 = as.Date(date1)}
  if (class(date2)=='character') {date2 = as.Date(date2)}
  # download the daily SOFR data from FRED, removing all dates with NAs:
  sofr = getSymbols('SOFR',src='FRED',auto.assign=FALSE)
  sofr = na.omit(sofr)
  # Convert 'sofr' to a data.table, with 'Date' and 'SOFR' as two columns
  sofr = as.data.table(sofr)
  names(sofr) = c('Date','SOFR')
  # divide SOFR by 100 to get the interest rate into decimal form
  sofr[, SOFR := SOFR/100]
  # create dataset with SOFR for date1 to date2
  # check if date1 is in sofr$Date
  if (!(date1 %in% sofr$Date)) {
    tmp = data.table(Date=date1,SOFR=NA)
    sofr = rbind(sofr,tmp)
  }
  # check if date2 is in sofr$Date
  if (!(date2 %in% sofr$Date)) {
    tmp = data.table(Date=date2,SOFR=NA)
    sofr = rbind(sofr,tmp)
  }
  # sort sofr by Date
  setorder(sofr,Date)
  # fill in missing dates using previous non-missing data
  sofr[, SOFR := na.locf(SOFR)]
  # create data with dates starting on date1 and ending on date2
  data = sofr[Date>=date1 & Date<=date2]
  
  # find the number of calendar days for each SOFR:
  data[, di := c(as.numeric(diff(Date)),0) ]
  # calculate the gross interest for each row
  data[, gross_daily_rate := 1 + (SOFR+spread)*di/360 ]
  # calculate cumulative product of the gross interest
  data[, cum_prod := cumprod(gross_daily_rate) ]
  # D is the number of calendar days in the interest payment period
  D = sum(data$di)
  # calculate the simple (i.e. non-compounding) interest rate from date1 to date2
  R = round((data[(.N-1),]$cum_prod[1]-1)*100*360/D,5)
  return(c(R=R,D=D))
}


t0 <- as.Date("2023-07-01")
t1 <- as.Date("2023-12-31")
numbers <- sofr_R(t0, t1, spread = 0.01)
numbers[1]/100 * numbers[2]/360 * 1000000









