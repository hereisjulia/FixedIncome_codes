
library(data.table)
library(tidyverse)
library(stringr)

setwd("E:/MQM_Courses/Term4/DataVisualization/Assignments")
auction <- fread("2024-Auction Company Extract_Extract.csv")

names(auction)

table(auction$`Item Industry Name`) %>% sort(decreasing = TRUE)

prices <- auction[, c("Current Bid", "Sold Price", "Total Sale", "Item Fee Amount")]
prices[, Gain := `Total Sale` - `Sold Price`]


item <- auction[, names(auction) %like% "Item", with =FALSE]
unique(item$`Item Family Name`)


table(item$`Item Fee Amount`) %>% sort(decreasing = TRUE)





