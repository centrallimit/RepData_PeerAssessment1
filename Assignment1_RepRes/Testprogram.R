library(dplyr)

setwd("~/Documents/RepData_PeerAssessment1")

data <- tbl_df(read.csv("activity.csv", header = TRUE, sep = ","))

data
