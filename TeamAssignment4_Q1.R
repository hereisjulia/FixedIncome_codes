# The following R code fetches data from Compustat and CRSP at WRDS

library(RPostgres)
library(tidyverse)
library(datasets)
library(psych)
library(ExPanDaR)
library(data.table)
library(pROC)		# this is needed for calculating the AUC

#initialize WRDS data server connection
wrds<-dbConnect(Postgres(),  
                host='wrds-pgdata.wharton.upenn.edu',
                port=9737,
                user='yt211',	            # replace $$$$ with your WRDS user name	
                "ersntY3Q2S*,76V",
                sslmode='require',
                dbname='wrds')


load('./datas/TeamAssignment4.RData')
# crsp_daily_comp_link is a data.table in the RData file

#Fetch COMPUSTAT data and merge with he linktable above
res <- dbSendQuery(wrds, "
      select gvkey, datadate, cusip, fyear, wcap, at,
             lt, ebit, re, mkvalt, sale, ni, act, lct
      
      from compa.funda

      where datadate between '1962-01-01' and '2020-12-31';")

comp <- dbFetch(res, n=-1)

dbClearResult(res)

crsp_comp<- merge(crsp_daily_comp_link, comp, by.x="gvkey", by.y="gvkey", sort = TRUE)

crsp_comp$na_count <- apply(is.na(crsp_comp), 1, sum)

crsp_comp <- crsp_comp[order(crsp_comp$gvkey,crsp_comp$datadate, crsp_comp$na_count),]

crsp_comp <- crsp_comp %>% distinct(gvkey, datadate, .keep_all = TRUE)  #There can be more than one line of data for a firm-year, leave only the one with least NAs 


# Fetch monthly CRSP data to construct indep. var.
res <- dbSendQuery(wrds, "
      select permno, date, prc, ret, shrout
      
      from crsp_a_stock.msf

      where date between '1961-01-01' and '2020-12-31';")

crsp_ms <- dbFetch(res, n=-1)
dbClearResult(res)

# Fetch monthly index data to construct indep. var.
res <- dbSendQuery(wrds, "
      select caldt, vwretd, vwretx, totval
      
      from crsp_a_indexes.msic

      where caldt between '1961-01-01' and '2020-12-31';")

crsp_msi <- dbFetch(res, n=-1)
dbClearResult(res)
names(crsp_msi)[names(crsp_msi) == "caldt"] <- "date"

# Fetch begin trading date to drop those begin trading outside 1962-2020 and construct age var.
res <- dbSendQuery(wrds, "
      select distinct permno, begexchdate
      
      from crsp_a_stock.mseexchdates;")

crsp_beg <- dbFetch(res, n=-1)
dbClearResult(res)

crsp_beg <- crsp_beg[order(crsp_beg$permno),]

crsp_comp <- merge(crsp_comp, crsp_beg, by.x="permno", by.y="permno", sort = TRUE)

 crsp_comp<- crsp_comp[ which(crsp_comp$begexchdate>='1962-01-01'
                         & crsp_comp$begexchdate<='2020-12-31'), ] #drop those begin trading before 1962 and after 2020

crsp_comp$begyear<- format(crsp_comp$begexchdate, "%Y") #extract the year a firm begin trading to compute age of a firm

crsp_ms <- merge(crsp_ms, crsp_msi, by.x = "date", by.y = "date", sort = TRUE)

crsp_ms$year <- format(crsp_ms$date,"%Y")


# run regressions on monthly returns of each firm in each year to obtain the standard deviation of the residuals
#
# the following code works, but takes a long time.
#
# system.time({
# firm_permno = unique(crsp_ms$permno)
# year_list = unique(crsp_ms$year)
# for (i in firm_permno){
#    for (j in year_list){
#       if(sum(crsp_ms$year == j & crsp_ms$permno == i & !is.na(crsp_ms$ret), na.rm=TRUE)==12){
#          tmpdata <- crsp_ms[ which(crsp_ms$permno==i& crsp_ms$year==j), ]
#          tmpmodel <- lm(tmpdata$ret ~ tmpdata$vwretd)
#          tmpsd <- sd(residuals(tmpmodel))
#          crsp_ms$sd[crsp_ms$permno ==i&crsp_ms$year==j] <- tmpsd 
#       }
#    }
# }
# })
# 
#     user   system  elapsed 
# 19261.59  2573.35 21873.11
# this code works, but took 6 hours to run on a PC !!!

sdreg <- function(x,y) {
   df <- data.frame(x=x, y=y)
   return( sd(summary(lm(y~x,data=df))$residuals) )
}
system.time({
   tmp <- data.table(crsp_ms)
   setorderv(tmp,c("permno","year"))
   tmp <- tmp[!is.na(tmp$ret),]                                     # remove NAs
   tmp1 <- tmp[, .N, by=c("permno","year")]                         # count number of monthly returns in each firm-year
   tmp1 <- tmp1[N==12,]                                             # keep only the firm-years that have 12 monthly returns
   tmp2 <- tmp[tmp1,,on=c("permno","year")]                         # extract monthly returns for firm-years with 12 monthly returns
   tmp3 <- tmp2[, .(sd=sdreg(vwretd,ret)), by=c("permno","year")]   # run regression of monthly return on market return by firm-year and keep the std dev of the residuals -- this is the sigma variable
   tmp4 <- tmp3[tmp, , on=c("permno","year")]                       # put the resulting sigma variable back into the full data set

   crsp_sd <- tmp4[,c("permno","year","sd")]%>% distinct(permno, year, .keep_all = TRUE)
   crsp_ms<-merge(crsp_ms, crsp_sd, by = c("permno","year"), sort = TRUE)
   }
)
#   user  system elapsed 
# 310.90    0.89  309.78 
# this code gives the same answers, but took only 310 seconds to run on the same PC !!!


rm(tmp, tmp1, tmp2, tmp3, tmp4)

crsp_ms$ret[is.na(crsp_ms$ret)] <- crsp_ms$vwretd[is.na(crsp_ms$ret)] #replace missing firm monthly returns with the market return of that month

crsp_msi$year <- format(crsp_msi$date,"%Y")
crsp_msi$fyear <- as.numeric(crsp_msi$year)-1  #one year lag to merge with fyear in comp
crsp_msi$month<- format(crsp_msi$date,"%m")
crsp_msi$vwretd <- log(1+crsp_msi$vwretd) #convert to log monthly return
crsp_msi <- crsp_msi[order(crsp_msi$fyear,crsp_msi$month),]
crsp_msi$vwretda <- ave(crsp_msi$vwretd, crsp_msi$fyear, FUN=cumsum) #cumulate monthly return to annual return
crsp_msi$vwretda <- exp(crsp_msi$vwretda)-1
crsp_msi <- crsp_msi[ which(crsp_msi$month == 12),] #keep only end of year market return and cap.
crsp_msi <- crsp_msi[ c("fyear", "vwretda", "totval")] #keep only year, return and market cap.

crsp_comp <- merge(crsp_comp, crsp_msi, by.x="fyear", by.y="fyear", sort = TRUE)

crsp_ms$fyear <- as.numeric(crsp_ms$year)-1  #one year lag to merge with fyear in comp
crsp_ms$month<- format(crsp_ms$date,"%m")
crsp_ms <- crsp_ms[order(crsp_ms$permno, crsp_ms$fyear,crsp_ms$month),]
crsp_ms$reta <- ave(crsp_ms$ret, crsp_ms$permno, crsp_ms$fyear, FUN=cumsum) #cumulate monthly return of firm into annual return
crsp_ms$reta <- exp(crsp_ms$reta)-1
crsp_ms <- crsp_ms[ which(crsp_ms$month == 12),] #only end of year firm level market cap.
crsp_ms_formerge <- crsp_ms[ c("fyear","permno", "reta", "sd")]

crsp_comp <- merge(crsp_comp, crsp_ms_formerge, by=c("fyear", "permno"), sort = TRUE)


crsp_me  <- crsp_ms["permno"]
crsp_me$me <- abs(crsp_ms$prc)*crsp_ms$shrout #compute firm market value 
crsp_me$fyear <- format(crsp_ms$date,"%Y")
crsp_comp <- merge(crsp_comp, crsp_me, by=c("fyear", "permno"), sort = TRUE)

# create Altman's variables

crsp_comp$wc_ta <- crsp_comp$wcap/crsp_comp$at

crsp_comp$re_ta <- crsp_comp$re/crsp_comp$at

crsp_comp$ebit_ta <- crsp_comp$ebit/crsp_comp$at

crsp_comp$me_tl <- crsp_comp$me/crsp_comp$lt/1000

crsp_comp$s_ta <- crsp_comp$sale/crsp_comp$at

# create Zmijewski's variables

crsp_comp$ni_ta <- crsp_comp$ni/crsp_comp$at

crsp_comp$tl_ta <- crsp_comp$lt/crsp_comp$at

crsp_comp$ca_cl <- crsp_comp$act/crsp_comp$lct

# create Shumway's variables

crsp_comp$ri_rm <- crsp_comp$reta - crsp_comp$vwretda

crsp_comp$size <- log(crsp_comp$me/crsp_comp$totval) 

crsp_comp$age <- log(as.numeric(format(crsp_comp$datadate,"%Y"))-as.numeric(crsp_comp$begyear)+1)

# truncate 1st and 99th percentile 

crsp_comp_trun <- treat_outliers(data.frame(crsp_comp[24:34],crsp_comp[22]), percentile = 0.01, truncate = TRUE) # truncate the first and last percentile of outliers

describe(crsp_comp_trun, skew=FALSE)

# create data for firm-years
firmdata <- crsp_comp[, c("permno","fyear","wc_ta","re_ta",      
                      "ebit_ta","me_tl","s_ta","ni_ta","tl_ta",
                      "ca_cl","sd","ri_rm","size","age")]
colnames(firmdata)[colnames(firmdata)=="sd"] <- "sigma"
colnames(firmdata)[colnames(firmdata)=="age"] <- "lage"
# reset 0.01 and 0.99 percentiles
for (i in 3:ncol(firmdata)) {
  tmp <- firmdata[,i]
  p01 <- quantile(tmp,0.01,na.rm=TRUE)
  p99 <- quantile(tmp,0.99,na.rm=TRUE)
  reset <- ifelse(tmp<p01,p01,ifelse(tmp>p99,p99,tmp))
  firmdata[,i] <- reset
}
summary(firmdata)

firmdata <- data.table(firmdata)
setorderv(firmdata, c("permno","fyear"))

# remove duplicates
firmdata[, na_count := apply(is.na(firmdata),1,sum)]
setorder(firmdata,permno,fyear,na_count)
firmdata = firmdata[,.SD[1],by=.(permno,fyear)]
firmdata[, na_count:=NULL]

