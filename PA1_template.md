# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
library(plyr)
setwd("Coursera/RepData_PeerAssessment1")
data <- read.csv("activity.csv")
summary(data)
cleandata <- na.omit(data)

## What is mean total number of steps taken per day?
dailydata <- ddply(cleandata, "date", summarize, sumofsteps = sum(steps))

mean(dailydata$sumofsteps)
median(dailydata$sumofsteps)

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
