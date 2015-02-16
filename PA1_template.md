# Reproducible Research: Peer Assessment 1

Load lattice library and set locale to English.

```r
library(lattice)
Sys.setlocale("LC_TIME", "C")
```

## Loading and preprocessing the data
Load the data.

```r
# unzip file if it doesn't exist yet
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}

# read in csv file
activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day ignoring missing values.

```r
# take sum of steps factorized by date
steps <- tapply(activity$steps, activity$date, function(x) sum(x, na.rm = TRUE))
```

Make a histogram of the total number of steps taken each day.

```r
hist(steps, xlab = "Number of steps", main = "Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
steps_mean <- round(mean(steps), digits = 2)
steps_median <- median(steps)
```
mean of total number of steps taken per day: 9354.23  
median of total number of steps taken per day: 10395


## What is the average daily activity pattern?
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
# take mean of steps factorized by interval
interval_steps <- tapply(activity$steps, activity$interval, function(x) mean(x, na.rm = TRUE))
plot(interval_steps, type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
max_interval <- names(which(interval_steps == max(interval_steps)))
```
5-minute interval containing maximum number of steps: 835

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
missing_sum <- sum(is.na(activity))
```
total number of missing values in the dataset: 2304  
  
The strategy for filling in all of the missing values in the dataset is to take the mean of a 5-minute interval and assign it to a missing value in that interval.


```r
# copy dataset
activity_full <- activity

# loop over all rows and fill in missing values of steps with average of the interval
for (i in 1 : nrow(activity_full)) {
    if (is.na(activity_full$steps[i])) {
        activity_full$steps[i] <- interval_steps[as.character(activity_full$interval[i])]
    }
}
```

Make a histogram of the total number of steps taken each day.

```r
steps_full <- tapply(activity_full$steps, activity_full$date, function(x) sum(x, na.rm = TRUE))
hist(steps_full, xlab = "Number of steps", main = "Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
stepsfullmean <- mean(steps_full)
stepsfullmedian <- median(steps_full)
```
mean of total number of steps taken per day: 1.0766189\times 10^{4}  
median of total number of steps taken per day: 1.0766189\times 10^{4}

Yes, they differ from the steps where missing values were removed because less steps get taken into account. Imputing missing values has the impact of increasing the total daily number of steps.


## Are there differences in activity patterns between weekdays and weekends?
For this part the dataset with the filled-in missing values is used.  
  
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
daytype <- vector("integer")

# loop over all rows and assign daytype either 0 for weekdays or 1 for weekends
for (i in 1 : nrow(activity_full)) {
    if (is.element(weekdays(as.Date(activity_full$date[i])), c("Saturday", "Sunday"))) {
        daytype[i] <- 0
    } else {
        daytype[i] <- 1
    }
}

# create factor column daytype in data frame
activity_full$daytype <- factor(daytype, labels = c("weekend", "weekday"))
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
xyplot(activity_full$steps ~ activity_full$interval | activity_full$daytype, data = activity_full, xlab = "Interval", ylab = "Number of steps", layout = c(1, 2), type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
There is a difference in activity patterns as the number of steps on weekdays are increased in earlier intervals compared to the weekend.
