setwd("/Users/loganfrederick/Projects/Coursera Johns Hopkins Data Science/Reproducible Research")
temp <- read.csv("RepData_PeerAssessment1/activity.csv")
dailyStepsMean <- tapply(temp$steps,temp$date,mean,na.rm=TRUE)
dailyStepsMedian <- tapply(temp$steps,temp$date,median,na.rm=TRUE)
dailyStepSum <- tapply(temp$steps,temp$date,sum,na.rm=TRUE)
hist(dailyStepSum,breaks=15)


uniqIntervals <- unique(temp$interval)

intervalSum <- vector(mode = "numeric", length = 288)

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
colSums(is.na(temp))

#Replace NAs with a mean from a random day
temp[is.na(temp)] <- sample(as.vector(dailyStepsMean), 1)

#New daily means and medians
newDailyStepsMean <- tapply(temp$steps,temp$date,mean,na.rm=TRUE)
newDailyStepsMedian <- tapply(temp$steps,temp$date,median,na.rm=TRUE)
newDailyStepSum <- tapply(temp$steps,temp$date,sum,na.rm=TRUE)
hist(newDailyStepSum,breaks=15)

#
datesVec <- c(names(newDailyStepsMean))
datesVecAsWeekdays <- weekdays(datesVec)
rdata = replace(datesVecAsWeekdays, datesVecAsWeekdays=="Monday" | datesVecAsWeekdays=="Tuesday", "Weekday")
rdata = replace(rdata, datesVecAsWeekdays=="Wednesday" | datesVecAsWeekdays=="Thursday" | datesVecAsWeekdays=="Friday", "Weekday")

rdata = replace(rdata, datesVecAsWeekdays=="Saturday" | datesVecAsWeekdays=="Sunday", "Weekend")

rdata = factor(,labels=c("Weekday","Weekend"))

#used for time series stuff
i = 0
for(index in 1:nrow(temp)) {
  i=i+1
  intervalSum[i] = intervalSum[i] + temp$steps
}

#
vectorOne <- as.vector(temp[,1])
tsOne <- ts(vectorOne)
ts.plot(tsOne)