---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
#Reading "Activity" Data
data <- read.csv(unzip("activity.zip"))

#Converting Date into "Date" class from "Factor" class
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.6.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
data$date <- ymd(data$date)

#Adding new factor column "weekdays" to the dataset
data$weekdays <- weekdays(data$date, abbreviate = TRUE)

data$weekdays.type <- ifelse(data$weekdays == "Sat" | data$weekdays == 
                                       "Sun", "Weekend", "Weekday")

data$weekdays.type <- factor(data$weekdays.type)
```

## What is mean total number of steps taken per day?


```r
cleanData <- data[!is.na(data$steps), ]
# load plyr library
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.6.3
```

```r
# group values by date
groupedByDate <- ddply(cleanData, ~date, summarise, sum = sum(steps))

# construct the histogram
hist(groupedByDate$sum, xlab = "Total Steps", main = "Histogram total steps per day", 
     col = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate mean value
mean(groupedByDate$sum)
```

```
## [1] 10766.19
```

```r
# Calculate median value
median(groupedByDate$sum)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
groupedByInterval <- ddply(cleanData, ~interval, summarise, mean = mean(steps))
with(groupedByInterval, plot(interval, mean, type = "l", 
                             ylab = "Average number of steps", xlab = "Interval", 
                             main = "Average daily activity", col = "darkred"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# first find max value
maxVal <- max(groupedByInterval$mean)
print(maxVal)
```

```
## [1] 206.1698
```

```r
# locate the line containing this value
maxLine <- groupedByInterval[groupedByInterval$mean == maxVal, ]
# find the interval
maxInterval <- maxLine$interval
print(maxInterval)
```

```
## [1] 835
```


## Imputing missing values

```r
# calculate sum of missing values
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
# create a new dataset
newdata <- data
# locate tha NAs
missingsteps <- is.na(newdata$steps)
# convert interval(s) to factor(s)
newdata$interval <- factor(newdata$interval)
groupedByInterval$interval <- factor(groupedByInterval$interval)

newdata[missingsteps, "steps"] <- groupedByInterval[newdata[missingsteps, "interval"], 
                                                    "mean"]

# group values by date
groupedByDate2 <- ddply(newdata, ~date, summarise, sum = sum(steps))

# construct the histogram
hist(groupedByDate2$sum, xlab = "Total Steps", main = "Histogram total steps per day", 
     col = "darkmagenta")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Calculate mean value
mean(groupedByDate2$sum)
```

```
## [1] 10766.19
```

```r
# Calculate median value
median(groupedByDate2$sum)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
# make a new dataset grouping data by interval and weekday.type
groupedBy.Interval.WeekDay <- ddply(newdata, ~interval + weekdays.type, summarise, 
                                    mean = mean(steps))

# For a nice plot 'unfactor' interval. Be careful to convert to characters
# first, or else you get the level values 1,2,3...
groupedBy.Interval.WeekDay$interval <- as.numeric(as.character(
        groupedBy.Interval.WeekDay$interval))
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.6.3
```

```r
xyplot(mean ~ interval | weekdays.type, groupedBy.Interval.WeekDay, type = "l", 
       layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
