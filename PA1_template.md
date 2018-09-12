---
title: "Reproducible Research: Peer Assessment 1"
author: Angela Liu
date: Sept. 12, 2018 
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv(file="activity.csv", header=TRUE, sep=",")

dim(data)
```

```
## [1] 17568     3
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is the mean total number of steps taken per day?

### First, compute each day's total value of steps

```r
library('plyr') # load plyr package

per_day_total <- ddply(data, .(date), summarize, sum=sum(steps)) 

head(per_day_total)
```

```
##         date   sum
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

### Then, plot a histogram of the total steps per day

```r
hist(per_day_total$sum, nclass=30, main="Histogram of total number of steps per day", xlab="Total number of steps per day", ylab="Count")
```

![](PA1_template_files/figure-html/hist-per-day-1.png)<!-- -->

### Finally, compute the mean and median of the above total steps

```r
summary(per_day_total$sum) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```


## What is the average daily activity pattern?


```r
na_omit_data = na.omit(data)

per_interval_stat <- ddply(na_omit_data, .(interval), summarize, sum=sum(steps), means=mean(steps)) 

plot(per_interval_stat$interval, per_interval_stat$means, type="l", main="Average number of steps per interval over time", xlab="Time interval in 5-minute", ylab="Average number of steps per interval")
```

![](PA1_template_files/figure-html/per-interval-mean-1.png)<!-- -->


### Find the time interval that has the maximum average number of steps across days



```r
index = which.max(per_interval_stat$means)

per_interval_stat$interval[index]
```

```
## [1] 835
```


## Imputing missing values

### First find the number of rows with NA or missing values 

```r
num_rows_NA <- sum(is.na(data$steps))
```

There are 2304 rows with missing values or "NA" in the original data. 


### We will use the average number of steps across all of the rest of days with real data per every 5-minute interval to replace all of the "NA" for the corresponding interval. 


```r
imputed_data <- data

index_NA_rows <- which(is.na(data$steps), arr.ind=TRUE)

# try a for loop
for (index in index_NA_rows) {
    interval = imputed_data$interval[index]
    imputed_data$steps[index] = per_interval_stat$means[ per_interval_stat$interval == interval ]
}

# the below method only worked for the first 288 rows of data
#imputed_data$steps[index_NA_rows] = per_interval_stat$means[(imputed_data$interval[index_NA_rows] == per_interval_stat$interval)]

head(imputed_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
tail(imputed_data)
```

```
##           steps       date interval
## 17563 2.6037736 2012-11-30     2330
## 17564 4.6981132 2012-11-30     2335
## 17565 3.3018868 2012-11-30     2340
## 17566 0.6415094 2012-11-30     2345
## 17567 0.2264151 2012-11-30     2350
## 17568 1.0754717 2012-11-30     2355
```

### Plot a new histogram using the imputed data 


```r
imputed_per_day_total <- ddply(imputed_data, .(date), summarize, sum=sum(steps)) 

hist(imputed_per_day_total$sum, nclass=30, main="Histogram of total number of steps per day (Imputed)", xlab="Total number of steps per day", ylab="Count")
```

![](PA1_template_files/figure-html/impute_hist-1.png)<!-- -->


### Compute the mean and median of daily number of steps with imputed data 


```r
summary(imputed_per_day_total$sum) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

As shown by the new histogram plot and new summary statistics, by imputing the missing values with average number of steps per 5-minute interval, the new histogram has a significantly higher peak around 10766, which is the mean value of the daily number of steps.  


## Are there differences in activity patterns between weekdays and weekends?

### Then, we will create a new vector, which is a factor with two levels, "weekday" and "weekend". 


```r
day_status = weekdays(as.Date(data$date))
day_status[day_status != "Saturday" & day_status != "Sunday"] = "weekday"
day_status[day_status == "Saturday" | day_status == "Sunday"] = "weekend"
sum(day_status == "weekend")
```

```
## [1] 4608
```

```r
sum(day_status == "weekday")
```

```
## [1] 12960
```


### Then, we will add this vector as a new column to the imputed data.


```r
new_data = cbind(imputed_data, day_status)
head(new_data)
```

```
##       steps       date interval day_status
## 1 1.7169811 2012-10-01        0    weekday
## 2 0.3396226 2012-10-01        5    weekday
## 3 0.1320755 2012-10-01       10    weekday
## 4 0.1509434 2012-10-01       15    weekday
## 5 0.0754717 2012-10-01       20    weekday
## 6 2.0943396 2012-10-01       25    weekday
```

### Finally, we will create a multi-panel plot showing the per-5-minute interval average number of steps across all days, split into "weekend" view and "weekday" view. 


```r
weekday_data = new_data[new_data$day_status == "weekday", ]
weekend_data = new_data[new_data$day_status == "weekend", ]

weekday_interval_stat <- ddply(weekday_data, .(interval), summarize, sum=sum(steps), means=mean(steps)) 
weekend_interval_stat <- ddply(weekend_data, .(interval), summarize, sum=sum(steps), means=mean(steps)) 

par(mfrow=c(2,1), oma=c(1, 0, 0, 0))
plot(weekday_interval_stat$interval, weekday_interval_stat$means, type='l', main="weekday", xlab="", ylab="")
plot(weekend_interval_stat$interval, weekend_interval_stat$means, type='l', main="weekend", xlab="", ylab="")

mtext("Time Interval", side=1, outer=TRUE, cex=1.5)
mtext("Average number of steps over multiple days", side=2, outer=TRUE, line=-1, cex=1.5)
```

![](PA1_template_files/figure-html/weekday_view-1.png)<!-- -->

