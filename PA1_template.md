# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis

If **activity.csv** file is absent then download the Zip again

```r
if (!file.exists("activity.csv")){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl,"temp.zip")
  unzip("temp.zip", exdir=".") 
  file.remove("temp.zip")
  rm(fileUrl)
}
```

Load Data

```r
stepsData <- read.csv("activity.csv") 
```
## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day


*Histogram* time

```r
stepsPerDay <- aggregate(steps ~ date, stepsData, sum)
hist(stepsPerDay$steps, main="Histogram for Oct + Nov 2012", 
     xlab="Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

Mean & Medians:

```r
mean(stepsPerDay$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps, na.rm = T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Line graph of interval versus av. steps per interval
then Highlight & annotate

```r
aggByInterval <- aggregate(steps ~ interval, stepsData, mean)
plot(aggByInterval$interval, aggByInterval$steps, type='l', col=1, main="Average number of steps averaged over all days", xlab="Interval", ylab="Average number of steps")
most <- aggByInterval[aggByInterval$steps == max(aggByInterval$steps), ]

abline(h=most$steps,col="red")
abline(v=most$interval,col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
print(paste("Highest average number of steps was",round(most$steps),"and is at interval:",most$interval))
```

```
## [1] "Highest average number of steps was 206 and is at interval: 835"
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(stepsData$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
print("If NA then replace with mean for same interval.")
```

```
## [1] "If NA then replace with mean for same interval."
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newStepsData <- stepsData

for (i in 1:nrow(newStepsData)) {
    if(is.na(newStepsData$steps[i])) {
        newStepsData$steps[i] <- aggByInterval$steps[i %% 288 +1] 
        #24hr / 5min intervals (+1 as R arrays start at 1, not 0)
    }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

*Histogram* time

```r
newStepsPerDay <- aggregate(steps ~ date, newStepsData, sum)
hist(newStepsPerDay$steps, main="Histogram for Oct + Nov 2012 with Imputation", 
     xlab="Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)

New Mean & Medians:

```r
mean(newStepsPerDay$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(newStepsPerDay$steps, na.rm = T)
```

```
## [1] 10766.19
```

Compare the two:

```r
print("Due to my strategy, Mean is unchanged and Median has only slightly gone up to now match the mean.")
```

```
## [1] "Due to my strategy, Mean is unchanged and Median has only slightly gone up to now match the mean."
```

Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newStepsData$workingDay <- ifelse(substr(weekdays(as.Date(newStepsData$date)),1,1)=="S","weekend","weekday")
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
aggByIntervalWD <- aggregate(steps ~ interval+workingDay, newStepsData, mean)
library(lattice)
xyplot(steps ~ interval | workingDay, aggByIntervalWD, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)
