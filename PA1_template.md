# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data

```r
  activity_monitor_data<-read.csv("activity.csv", header=TRUE, sep=",", colClass=c("numeric","character","numeric"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
  activity_monitor_data$date <- strptime(activity_monitor_data$date,"%Y-%m-%d")
  activity_monitor_data$dateAsChar<-as.character(activity_monitor_data$date)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
  no_null_activity_data <- activity_monitor_data[!is.na(activity_monitor_data$steps),]
  
  steps_per_day <- aggregate(no_null_activity_data$steps,by=list(no_null_activity_data$dateAsChar),FUN="sum", na.rm=TRUE)
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
  colnames(steps_per_day) <- c("date","steps")
  hist(steps_per_day$steps, xlab="steps per day",col="cyan", main="Histogram of average steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\
3. Calculate and report the mean and median of the total number of steps taken per day

```r
  mean_steps_per_day <- mean(steps_per_day$steps,na.rm=TRUE)
  median_steps_per_day <- median(steps_per_day$steps,na.rm=TRUE)
  cat(c("The mean of steps taken per day",mean_steps_per_day))
```

```
## The mean of steps taken per day 10766.1886792453
```

```r
  cat(c("The median of steps taken per day",median_steps_per_day))
```

```
## The median of steps taken per day 10765
```
  
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
  average_steps_per_interval<-aggregate(x = no_null_activity_data$steps, by = list(no_null_activity_data$interval), FUN = "mean")
  colnames(average_steps_per_interval)<-c("interval","steps")
  plot(average_steps_per_interval$interval,average_steps_per_interval$steps,xlab="interval",ylab="average steps",type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)\
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
  cat(c("The maximum number of steps were achieved across interval ", average_steps_per_interval[which.max(average_steps_per_interval$steps),"interval"]))
```

```
## The maximum number of steps were achieved across interval  835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
  cat(c("The total number of mising rows are ",sum(is.na(activity_monitor_data$steps))))
```

```
## The total number of mising rows are  2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

The method used is to replace the NA value for missing steps with mean of that 5-minute interval

```r
  filled_activity_data <- activity_monitor_data
  for(i in 1:nrow(activity_monitor_data)){
    if(is.na(activity_monitor_data[i,1])){
        filled_activity_data[i,1]=average_steps_per_interval[average_steps_per_interval$interval==activity_monitor_data[i,"interval"],"steps"]
    }
  }
  head(filled_activity_data)
```

```
##       steps       date interval dateAsChar
## 1 1.7169811 2012-10-01        0 2012-10-01
## 2 0.3396226 2012-10-01        5 2012-10-01
## 3 0.1320755 2012-10-01       10 2012-10-01
## 4 0.1509434 2012-10-01       15 2012-10-01
## 5 0.0754717 2012-10-01       20 2012-10-01
## 6 2.0943396 2012-10-01       25 2012-10-01
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
  steps_per_day_no_null <- aggregate(filled_activity_data$steps,by=list(filled_activity_data$dateAsChar),FUN="sum")
  colnames(steps_per_day_no_null) <- c("date","steps")
  hist(steps_per_day_no_null$steps, xlab="steps per day",col="cyan", main="Histogram of average steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\

```r
  cat(c("The mean of steps taken per day",mean(steps_per_day_no_null$steps)))
```

```
## The mean of steps taken per day 10766.1886792453
```

```r
  cat(c("The median of steps taken per day",median(steps_per_day_no_null$steps)))
```

```
## The median of steps taken per day 10766.1886792453
```

As we can see from the results above, there is no difference in the mean and median of average steps per day.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
  weekend<-c("Saturday","Sunday")
  filled_activity_data$whichDay <- weekdays(filled_activity_data$date)
  filled_activity_data$whatDay<-ifelse(filled_activity_data$whichDay %in% weekend, "weekend", "weekday")
  filled_activity_data$whatDay<-factor(filled_activity_data$whatDay)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
  #creating separate frames for weekday and weekend
  weekday_data <- filled_activity_data[filled_activity_data$whatDay=="weekday",]
  weekend_data <- filled_activity_data[filled_activity_data$whatDay=="weekend",]
  average_steps_per_interval_weekday<-aggregate(x = weekday_data$steps, by = list(weekday_data$interval), FUN = "mean")
  average_steps_per_interval_weekend<-aggregate(x = weekend_data$steps, by = list(weekend_data$interval), FUN = "mean")
  colnames(average_steps_per_interval_weekday)<-c("interval","steps")
  colnames(average_steps_per_interval_weekend)<-c("interval","steps")
  average_steps_per_interval_weekday$whatDay <- "weekday"
  average_steps_per_interval_weekend$whatDay <- "weekend"
  # merging the weekday and weekend average frames 
  meanstp<-rbind(average_steps_per_interval_weekday,average_steps_per_interval_weekend)
  meanstp$whatDay<-factor(meanstp$whatDay)
  
  library(lattice)
  png(filename = "panelplot.png",width = 480, height = 480, units = "px",bg = "transparent")
  xyplot(steps ~ interval | whatDay, data = meanstp,layout=c(1,2),type="l")
  dev.off()
```

```
## png 
##   2
```

```r
  xyplot(steps ~ interval | whatDay, data = meanstp,layout=c(1,2),type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)\
