library(jrvFinance)
library(data.table)
library(lubridate)

DATE <- function(yyyy,mm,dd){
  date <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(date)
}


##### Q2
bond_data2 <- fread("TeamAssignment1_Q2_bonds.csv")
bond <- bond_data2[bond_data2$team[11]]
strips_data <- fread("TeamAssignment1_Q2_strips.csv")

bond_data2[, maturity := as.Date(maturity)]
strips_data[, maturity := as.Date(maturity)]
settle <- DATE(2023,12,15)

coupon_quarter <- bond$coupon / 4 * 100

#Cashflows
payment_dates <- strips_data[maturity > settle & maturity <= bond$maturity, maturity]
payment_dates <- sort(unique(payment_dates))
bond_cash_flows <- rep(coupon_quarter, length(payment_dates))
bond_cash_flows <- c(bond_cash_flows, 100)
last_payment_date <- payment_dates[length(payment_dates)]
payment_dates <- c(payment_dates, last_payment_date)

bond_cash_flow_schedule <- data.table(Date = payment_dates, CashFlow = bond_cash_flows)

#arbitrage price
combined_data <- merge(bond_cash_flow_schedule, strips_data[type == "ci"], by.x = "Date", by.y = "maturity")

sp_row <- strips_data[type == "sp" & maturity == combined_data[nrow(combined_data), Date]]

if (nrow(sp_row) == 1) {
  combined_data[nrow(combined_data), `:=` (
    type = sp_row$type,
    pbid = sp_row$pbid,
    pask = sp_row$pask
  )]
}

arbi_bid <- coupon_quarter * sum(combined_data$pbid[1:(nrow(combined_data)-1)]) / 100 + combined_data$pbid[nrow(combined_data)]
arbi_ask <- coupon_quarter * sum(combined_data$pask[1:(nrow(combined_data)-1)]) / 100 + combined_data$pask[nrow(combined_data)]
arbi_bid <- round(arbi_bid,3)
arbi_ask <- round(arbi_ask,3)

arbi_profit1 <- ifelse(bond$pbid > arbi_ask, "Yes", "No")
arbi_profit2 <- ifelse(bond$pask > arbi_bid, "Yes", "No")


#Answer
print(paste("Buy STRIPS/Sell bond: Pay", arbi_ask, "and Get", bond$pbid, "; Arbitrage profit:", arbi_profit1))
print(paste("Buy bond/Sell STRIPS: Pay", arbi_bid, "and Get", bond$pask, "; Arbitrage profit:", arbi_profit2))
