# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("/Users/loganfrederick/Projects/Coursera Johns Hopkins Data Science/Reproducible Research")
temp <- read.csv("RepData_PeerAssessment1/activity.csv")
```


## What is mean total number of steps taken per day?

```r
dailyStepsMean <- tapply(temp$steps, temp$date, mean, na.rm = TRUE)
dailyStepsMedian <- tapply(temp$steps, temp$date, median, na.rm = TRUE)
dailyStepSum <- tapply(temp$steps, temp$date, sum, na.rm = TRUE)
```


## What is the average daily activity pattern?

```r
hist(dailyStepSum, breaks = 15)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



## Imputing missing values

#Replace NAs with a mean from a random day
temp[is.na(temp)] <- sample(as.vector(dailyStepsMean), 1)

#New daily means and medians
newDailyStepsMean <- tapply(temp$steps,temp$date,mean,na.rm=TRUE)
newDailyStepsMedian <- tapply(temp$steps,temp$date,median,na.rm=TRUE)
newDailyStepSum <- tapply(temp$steps,temp$date,sum,na.rm=TRUE)
hist(dailyStepSum,breaks=15)


## Are there differences in activity patterns between weekdays and weekends?
