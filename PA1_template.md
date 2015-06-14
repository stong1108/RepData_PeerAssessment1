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

# Average the steps over each interval
acrossDates <- tapply(clean$steps, clean$interval, mean)
df <- data.frame(intervals = unique(clean$interval), avgSteps = acrossDates)
qplot(intervals, avgSteps, data = df, geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Find interval corresponding to max average steps
df$intervals[df$avgSteps == max(df$avgSteps)]
```

```
## [1] 835
```

## Imputing missing values

```r
# Sum number of missing values
sum(!complete.cases(data))
```

```
## [1] 2304
```

```r
# Replace missing values with average of non-missing values
data$steps[is.na(data$steps)] <- mean(clean$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
library(chron)
library(lattice)

# Determine if date is weekday/weekend
data <- transform(data, weekend = is.weekend(ymd(data$date)))
data$weekend <- factor(data$weekend, levels = c("FALSE", "TRUE"), labels = c("weekday", "weekend"))

# Split steps and intervals into weekday/weekend 
wksteps <- split(data$steps, data$weekend)
wkints <- split(data$interval, data$weekend)

# Combine steps and intervals into weekday/weekend 
df_day <- data.frame(interval = wkints$weekday, steps = wksteps$weekday)
avgDay <- tapply(df_day$steps, df_day$interval, mean)
tmpDay <- data.frame(interval = unique(data$interval), steps = avgDay, day = rep("weekday", length(avgDay)))

df_end <- data.frame(interval = wkints$weekend, steps = wksteps$weekend)
avgEnd <- tapply(df_end$steps, df_end$interval, mean)
tmpEnd <- data.frame(interval = unique(data$interval), steps = avgEnd, day = rep("weekend", length(avgEnd)))

# Combine weekday/weekend data frames into master data frame
avgs <- rbind(tmpDay, tmpEnd)

# Panel plot
xyplot(steps ~ interval|day, data = avgs, type = "l", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
