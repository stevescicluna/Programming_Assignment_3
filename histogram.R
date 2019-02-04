# set working directory
setwd("C:/Users/Steve/Dropbox/Data Science/Week_4_Assignment_3")

## 1. PLOT THE 30 DAY MORTALITY RATES FOR HEART ATTACK

# read in data from "outcome-of-care-measures.csv" file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# coerce values in column 11 to be numerics
outcome[, 11] <- as.numeric(outcome[, 11])

# draw histogram of values in column 11
# simple histogram - not customising heading or axis labels
hist(outcome[, 11])