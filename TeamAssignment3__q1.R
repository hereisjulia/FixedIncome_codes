
library(data.table)
library(tidyverse)
library(stringr)

setwd("E:/MQM_Courses/Term4/DataVisualization/Assignments")
auction <- fread("2024-Auction Company Extract_Extract.csv")

names(auction)

table(auction$`Item Industry Name`) %>% sort(decreasing = TRUE)

item <- auction[, names(auction) %like% "Item", with =FALSE]

unique(item$`Item Fee Amount`)


table(item$`Item Fee Amount`) %>% sort(decreasing = TRUE)





