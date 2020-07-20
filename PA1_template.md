---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
df <- read.csv("activity.csv", na.strings = "NA")
#print(head(df))

df$date <- as.Date(df$date)
df_ign <- subset(df, !is.na(steps))
#print(head(df_ign))
```


## What is mean total number of steps taken per day?

```r
dailysum <- tapply(df_ign$steps, df_ign$date, sum, na.rm = TRUE)
dailysum <- dailysum[!is.na(dailysum)]
#print(dailysum)

hist(dailysum, xlab = "Daily Total Steps", ylab = "Frequency", main = "The distribution of daily total (missing data ignored)")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
mean(dailysum)
```

```
## [1] 10766.19
```

```r
median(dailysum)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
int_avg <- tapply(df_ign$steps, df_ign$interval, mean ,na.rm = TRUE)
df_ia <- data.frame(interval = as.integer(names(int_avg)), avg = int_avg)

with(df_ia, plot(interval, avg, type = 'l', xlab = "5-minute intervals", ylab = "average steps in the interval across all days"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
max_st <- max(df_ia$avg)
df_ia[df_ia$avg == max_st,]
```

```
##     interval      avg
## 835      835 206.1698
```

## Imputing missing values

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```


```r
df_impute <- df
ndx <- is.na(df_impute$steps)
int_avg <- tapply(df_ign$steps, df_ign$interval, mean, na.rm = TRUE)
df_impute$steps[ndx] <- int_avg[as.character(df_impute$interval[ndx])]
```


```r
new_dailysum <- tapply(df_impute$steps, df_impute$date, sum, na.rm = TRUE)

hist(x=new_dailysum,
     xlab="daily steps",
     ylab="frequency",
     main="The distribution of daily total (with missing data imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
mean(new_dailysum)
```

```
## [1] 10766.19
```


```r
median(new_dailysum)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
is_weekday <- function(d) {
    wd <- weekdays(d)
    ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(df_impute$date, is_weekday)
df_impute$wk <- as.factor(wx)
head(df_impute)
```

```
##       steps       date interval      wk
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```


```r
wk_df <- aggregate(steps ~ wk+interval, data=df_impute, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=wk_df)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
