---
title: "Reproducible Research: Peer Assessment 1"
author: "vcsalgado"
date: "January 18, 2021"
output: html_document:
  keep_md: true
---

## Set Global settings
##### Setup settings

```r
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
```

##### Load libraries

```r
library(dplyr)
library(ggplot2)
```

## Read and preprocessing data

```r
data_activity <- read.csv("activity.csv")
data_activity$date <- as.POSIXct(data_activity$date, "%Y-%m-%d")
```

## General data review

```r
names(data_activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(data_activity)
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
tail(data_activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
summary(data_activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day

```r
steps_by_date <- with(data_activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
names(steps_by_date) <- c("date", "steps")
```

##### 2. Make a histogram of the total number of steps taken each day

```r
hist(steps_by_date$steps, main = "Number of steps taken by day", xlab = "Steps per day", ylab="Frequency", ylim = c(0,20), breaks = seq(0,25000, by=1000))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

##### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_by_date$steps)
```

```
## [1] 9354.23
```

```r
median(steps_by_date$step)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
##### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_by_day <- aggregate(x=list(avg_steps=data_activity$steps), by=list(interval=data_activity$interval), FUN=mean, na.rm=TRUE)
```

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
label_max <- paste("Max. steps:", as.character(avg_by_day[which.max(avg_by_day$interval), ]$interval), "at", as.character(avg_by_day[which.max(avg_by_day$avg_steps), ]$interval), "Interval", sep = " ")
label_pos_y <- avg_by_day[which.max(avg_by_day$interval), ]$interval/10
label_pos_x <- avg_by_day[which.max(avg_by_day$avg_steps), ]$interval

ggplot(data=avg_by_day, aes(x=interval, y=avg_steps)) + geom_line() +  xlab("5-minute interval") + ylab("Average number of steps") + ggtitle("Average number of steps by interval") + annotate("text", x= label_pos_x + 20, y= label_pos_y - 25, hjust = 0,  label = label_max)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

## Imputing missing values
##### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows withvNAs)

```r
sum(is.na(data_activity$steps))
```

```
## [1] 2304
```

##### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data_missing <- data_activity
data_missing$steps[is.na(data_missing$steps)] <- mean(data_missing$steps, na.rm=TRUE)
```

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fill_activity <- transform(data_activity, steps = ifelse(is.na(data_activity$steps), yes = data_missing$steps, no = data_activity$steps))
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
fill_steps_by_date <- with(fill_activity, aggregate(steps, by = list(date), sum, na.rm = FALSE))
names(fill_steps_by_date) <- c("date", "steps")
hist(fill_steps_by_date$steps, main = "Number of steps taken by day", xlab = "Steps per day", ylab="Frequency", ylim = c(0,20), breaks = seq(0,25000, by=1000))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
filled_in_missing <- fill_activity
filled_in_missing$daytype <- weekdays(as.Date(filled_in_missing$date))
filled_in_missing$daytype <- toupper(filled_in_missing$daytype)
filled_in_missing$daytype[filled_in_missing$daytype  %in% c("SATURDAY","SUNDAY","SÁBADO","DOMINGO") ] <- "weekend"
filled_in_missing$daytype[filled_in_missing$daytype != "weekend"] <- "weekday"
filled_in_missing$daytype <- as.factor(filled_in_missing$daytype)
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
average_by_day <- aggregate(steps~interval + daytype, filled_in_missing, mean, na.rm = TRUE)

qplot(interval, 
      steps, 
      data = average_by_day, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ daytype, ncol = 1)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


