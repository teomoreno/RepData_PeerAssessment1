---
title: "Reproducible Research Project 1"
author: "_Teo Moreno_"
date: "_19 Dec 2015_"
output: html_document
---


### Loading and preprocessing the data

Loading the dataset.


```r
activity <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

Saving the dataset as data frame.


```r
activity <- data.frame(activity)
```

```
## Error in data.frame(activity): object 'activity' not found
```

```r
str(activity)
```

```
## Error in str(activity): object 'activity' not found
```

### What is the total number of steps taken per day?

Calculating the number of steps taken per day.


```r
activity_date <- aggregate(steps ~ date, activity, sum)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
str(activity_date)
```

```
## Error in str(activity_date): object 'activity_date' not found
```

Histogram of the total number of steps taken each day.


```r
hist(activity_date$steps, xlab = "Steps per Day",
     main = "Total Number of Steps", col = "blue")
```

```
## Error in hist(activity_date$steps, xlab = "Steps per Day", main = "Total Number of Steps", : object 'activity_date' not found
```

Calculating the mean and median of the total number of steps taken per day.


```r
mean(activity_date$steps, na.rm = TRUE)
```

```
## Error in mean(activity_date$steps, na.rm = TRUE): object 'activity_date' not found
```

```r
median(activity_date$steps, na.rm = TRUE)
```

```
## Error in median(activity_date$steps, na.rm = TRUE): object 'activity_date' not found
```

### What is the average daily activity pattern?

Calculating the average daily activity by 5-minute interval.


```r
activity_interval <- aggregate(steps ~ interval, activity, mean)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
str(activity_interval)
```

```
## Error in str(activity_interval): object 'activity_interval' not found
```

Plotting steps versus the 5-minute interval.


```r
plot(activity_interval$interval, activity_interval$steps,
     xlab= 'Interval', ylab= 'Steps', type = "l",
     main = "Activity Pattern", col = "blue")
```

```
## Error in plot(activity_interval$interval, activity_interval$steps, xlab = "Interval", : object 'activity_interval' not found
```

Calculating the 5-minute interval that contains the maximum number of steps.


```r
activity_interval$interval[which.max(activity_interval$steps)]
```

```
## Error in eval(expr, envir, enclos): object 'activity_interval' not found
```

### Imputing missing values

Calculating and reporting the total number of missing values in the dataset.


```r
sum(!complete.cases(activity))
```

```
## Error in complete.cases(activity): object 'activity' not found
```

Filling all the missing values in the dataset with the mean across intervals.


```r
activity_na <- activity[which(!complete.cases(activity)), ]
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
activity_na$steps <- mean(activity_interval$steps, na.rm = TRUE)
```

```
## Error in mean(activity_interval$steps, na.rm = TRUE): object 'activity_interval' not found
```

```r
str(activity_na)
```

```
## Error in str(activity_na): object 'activity_na' not found
```

Creating a new dataset combining complete data an filled missing data.


```r
activity_comp <- activity[which(complete.cases(activity)), ]
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
str(activity_comp)
```

```
## Error in str(activity_comp): object 'activity_comp' not found
```

```r
activity_fill <- rbind(activity_comp, activity_na)
```

```
## Error in rbind(activity_comp, activity_na): object 'activity_comp' not found
```

```r
index <- order(activity_fill$date)
```

```
## Error in order(activity_fill$date): object 'activity_fill' not found
```

```r
activity_fill <- activity_fill[index, ]
```

```
## Error in eval(expr, envir, enclos): object 'activity_fill' not found
```

```r
str(activity_fill)
```

```
## Error in str(activity_fill): object 'activity_fill' not found
```

New calculatiion of the number of steps taken per day.


```r
activity_fill_date <-aggregate(steps ~ date, activity_fill, sum)
```

```
## Error in eval(expr, envir, enclos): object 'activity_fill' not found
```

```r
str(activity_fill_date)
```

```
## Error in str(activity_fill_date): object 'activity_fill_date' not found
```

New Histogram of the total number of steps taken each day.


```r
hist(activity_fill_date$steps, xlab = "Steps per Day",
     main = "New Total Number of Steps", col = "blue")
```

```
## Error in hist(activity_fill_date$steps, xlab = "Steps per Day", main = "New Total Number of Steps", : object 'activity_fill_date' not found
```

New mean and median of the total number of steps taken per day.


```r
mean(activity_fill_date$steps)
```

```
## Error in mean(activity_fill_date$steps): object 'activity_fill_date' not found
```

```r
median(activity_fill_date$steps)
```

```
## Error in median(activity_fill_date$steps): object 'activity_fill_date' not found
```

### Are there differences in activity patterns between weekdays and weekends?

Adding a weekday column to the activity set.


```r
activity_fill$weekday <- weekdays(as.Date(activity_fill$date))
```

```
## Error in as.Date(activity_fill$date): object 'activity_fill' not found
```

Adding a daytype column to the activity set.


```r
activity_fill$daytype <- ifelse(activity_fill$weekday == "Saturday" | 
                                       activity_fill$weekday == "Sunday", "Weekend", "Weekday") 
```

```
## Error in ifelse(activity_fill$weekday == "Saturday" | activity_fill$weekday == : object 'activity_fill' not found
```

Calculating the average activity by day type (weekday or weekend).


```r
activity_fill_daytype <- aggregate(steps~interval+daytype, activity_fill, mean)
```

```
## Error in eval(expr, envir, enclos): object 'activity_fill' not found
```

```r
str(activity_fill_daytype)
```

```
## Error in str(activity_fill_daytype): object 'activity_fill_daytype' not found
```

Making a time series plot.


```r
library(lattice)
xyplot(steps~interval|daytype, activity_fill_daytype, layout = c(1,2),
       type = "l", main = "Activity Pattern")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'activity_fill_daytype' not found
```
