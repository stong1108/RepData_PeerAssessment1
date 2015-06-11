# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
clean <- data[!is.na(data$steps),]
```

## What is mean total number of steps taken per day?

```r
hist(clean$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(clean$steps)
```

```
## [1] 37.3826
```

```r
median(clean$steps)
```

```
## [1] 0
```


## What is the average daily activity pattern?

```r
library(ggplot2)
acrossDates <- tapply(clean$steps, clean$interval, mean)
intervals <- unique(clean$interval)
acrossDates <- cbind(intervals, acrossDates)
df <- data.frame(acrossDates)
qplot(intervals, acrossDates, data = df, geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
df$intervals[df$acrossDates == max(df$acrossDates)]
```

```
## [1] 835
```

## Imputing missing values

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

```r
data$steps[is.na(data$steps)] <- mean(clean$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
library(chron)
```

```
## 
## Attaching package: 'chron'
## 
## The following objects are masked from 'package:lubridate':
## 
##     days, hours, minutes, seconds, years
```

```r
library(lattice)
data <- transform(data, weekend = is.weekend(ymd(data$date)))
data$weekend <- factor(data$weekend, levels = c("FALSE", "TRUE"), labels = c("weekday", "weekend"))

# Split steps and intervals into weekday/weekend groups
wksteps <- split(data$steps, data$weekend)
wkints <- split(data$interval, data$weekend)

# Combine steps and intervals into weekday/weekend groups
df_day <- data.frame(cbind(wkints$weekday, wksteps$weekday))
names(df_day) <- c("interval", "steps")

df_end <- data.frame(cbind(wkints$weekend, wksteps$weekend))
names(df_end) <- c("interval", "steps")
```
