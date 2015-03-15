setwd("/Users/loganfrederick/Projects/Coursera Johns Hopkins Data Science/Reproducible Research")
temp <- read.csv("RepData_PeerAssessment1/activity.csv")

dailyStepsMean <- tapply(temp$steps,temp$date,mean,na.rm=TRUE)
dailyStepsMedian <- tapply(temp$steps,temp$date,median,na.rm=TRUE)
dailyStepSum <- tapply(temp$steps,temp$date,sum,na.rm=TRUE)

#Daily Means
print(dailyStepsMean)

#Daily Medians
print(dailyStepsMedian)

#Histogram
hist(dailyStepSum,breaks=15)

library(ggplot2)

stepsPerInterval <- data.frame(tapply(temp$steps, temp$interval, mean, na.rm=TRUE))
names(stepsPerInterval) <- "steps"
stepsPerInterval$interval <- as.integer(row.names(stepsPerInterval))
ggplot(stepsPerInterval) + aes(x=interval, y=steps) + geom_line()
which.max(stepsPerInterval$steps)

sum(is.na(temp$steps))
#Replace NAs with a mean from a random day
temp[is.na(temp)] <- sample(as.vector(dailyStepsMean), 1)

#New daily means and medians
newDailyStepsMean <- tapply(temp$steps,temp$date,mean,na.rm=TRUE)
newDailyStepsMedian <- tapply(temp$steps,temp$date,median,na.rm=TRUE)
newDailyStepSum <- tapply(temp$steps,temp$date,sum,na.rm=TRUE)

#New Daily Means
print(newDailyStepsMean)

#New Daily Medians
print(newDailyStepsMedian)

hist(newDailyStepSum,breaks=15)

temp$date <- as.Date(temp$date,"%Y-%m-%d")

avgStepInt <- aggregate(temp$steps,temp[3],mean,na.rm=TRUE)
merged_temp <- merge(x=temp,y=avgStepInt,by="interval",all.x=TRUE)
merged_temp$steps <- ifelse(is.na(merged_temp$steps),round(merged_temp$x,digits=2),merged_temp$steps)

factor <- merged_temp[,-c(4)]
factor$flag <- as.factor(ifelse(weekdays(factor$date) %in% c("Saturday","Sunday"),"Weekend","Weekdays"))
head(factor)

plot_temp <- aggregate(factor$steps,factor[c(1,4)],mean)

library(lattice)
xyplot(x ~ interval | flag,data = plot_temp,layout=c(1,2),type = "l",xlab = "Interval",ylab = "Number of steps")

#Other
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