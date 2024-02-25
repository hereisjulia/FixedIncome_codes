library(tidyverse)
library(ggplot2)
library(cowplot)
library(pROC)
######## DATA CLEANSING ########
setwd("E:/MQM_Courses/Term4/FixedIncomeSecurities/FixedIncome_codes/datas")

## Step 1: create the firmdata dataset
load("firmdata.RData")

## Step 2: read in the failed firms and the bankruptcy filing years
load('TeamAssignment4.RData')

## Step 3: create the status variable

firmdata[, status := as.character("0")]
fail_firms[status == 1, fail_1 := fail.year - 1]
fail_firms[status == 1, fail_2 := fail.year - 2]

firmdata <- left_join(firmdata, fail_firms[, -2], by = "permno", copy = FALSE)
firmdata <- firmdata[is.na(fail.year) | fyear < fail_1, ]

status <- with(firmdata, permno %in% fail_firms$permno & (fyear == fail_1 | fyear == fail_2))
firmdata$status[status] <- 1
firmdata <- firmdata[, -c("fail_1", "fail_2")]

## Step 4: Create training and test datasets

setorder(firmdata,permno,fyear)
first.year <- firmdata[,.SD[1],by='permno']
train.id = first.year[fyear<=1992]$permno
test.id = first.year[fyear>1992]$permno
train = firmdata[permno %in% train.id]
train = train[fyear<1993]
test = firmdata[permno %in% test.id]
test = test[fyear>1992]

c(nrow(train), nrow(test))

#### Q1a ####
Q1A <- data.table(fy.observation = c(length(train$fyear), length(test$fyear)),
                  fail.firm = c(nrow(train[status == "1"]), nrow(test[status == "1"])),
                  dataset = c("train", "test")) 
#How many firm-year observations are in the training set? How many firms failed in the training set?
#How many firm-year observations are in the testing set? How many firms failed in the testing set?
Q1A

Q1A1 <- ggplot(Q1A, aes(dataset, fy.observation))+
  geom_col() + geom_text(aes(label = fy.observation)) + theme_minimal()
Q1A2 <- ggplot(Q1A, aes(dataset, fail.firm))+
  geom_col() + geom_text(aes(label = fail.firm)) + theme_minimal()
Q1A.plot <- plot_grid(Q1A1, Q1A2, ncol = 2, align = 'v')

#### Q1b ####
train$status <- as.numeric(train$status)
training.N <- train[order(-fyear), .SD[1], by = permno]

Alt.1 <- glm( status ~ wc_ta + re_ta + ebit_ta + me_tl + s_ta + lage, family = binomial,
              data = training.N)
pred <- predict(Alt.1, newdata=test, type="response")
Alt.1.auc <- roc(test$status, pred, algorithm=2)$auc
Alt.m <- glm( status ~ wc_ta + re_ta + ebit_ta + me_tl + s_ta + lage, family = binomial, data = train)
pred <- predict(Alt.m, newdata=test, type="response")
Alt.m.auc <- roc(test$status,pred,algorithm=2)$auc
auc <- data.table(variable = "Altman",
                  one.period = Alt.1.auc[[1]],
                  multi.period = Alt.m.auc[[1]])
# Which model has better accuracy (i.e. higher AUC)?
auc
# one-period model performs better

#### Q1c ####
Alt.1c <- glm( status ~ ni_ta + tl_ta + ca_cl + lage, family = binomial,
              data = training.N)
pred <- predict(Alt.1c, newdata=test, type="response")
Alt.1c.auc <- roc(test$status, pred, algorithm=2)$auc
Alt.mc <- glm( status ~ ni_ta + tl_ta + ca_cl + lage, family = binomial, data = train)
pred <- predict(Alt.mc, newdata=test, type="response")
Alt.mc.auc <- roc(test$status,pred,algorithm=2)$auc
auc <- rbind(auc,
             data.table(variable = "Zmijewski",
                        one.period = Alt.1c.auc[[1]],
                        multi.period = Alt.mc.auc[[1]]))
# Which model has better accuracy (i.e. higher AUC)?
auc[2]
# one-period model performs slightly better

#### Q1d ####

Alt.1d <- glm( status ~ ni_ta + tl_ta + size + ri_rm + sigma + lage, family = binomial,
               data = training.N)
pred <- predict(Alt.1d, newdata=test, type="response")
Alt.1d.auc <- roc(test$status, pred, algorithm=2)$auc
Alt.md <- glm( status ~ ni_ta + tl_ta + size + ri_rm + sigma + lage, family = binomial, data = train)
pred <- predict(Alt.md, newdata=test, type="response")
Alt.md.auc <- roc(test$status,pred,algorithm=2)$auc
auc <- rbind(auc,
             data.table(variable = "Shumay",
                        one.period = Alt.1d.auc[[1]],
                        multi.period = Alt.md.auc[[1]]))
# Which model has better accuracy (i.e. higher AUC)?
auc[3]
# multi-period model performs better
str(auc)
#### Q1e ####
#Comparing across all 6 logit models in 1b-1d, which one is the most accurate in terms of AUC?
auc1 <- auc %>% pivot_longer(-1, names_to = "period", values_to = "auc") %>% as.data.table()
auc1[order(auc1), .SD[.N]]

