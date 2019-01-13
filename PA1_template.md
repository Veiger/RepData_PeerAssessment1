---
output:
  pdf_document: default
  html_document: default
---
Course 5 Project 1
====================

##set working directory and load packages

```r
setwd("/Users/weigeguo/Desktop/coursera")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

##Load and preprocessing data

```r
activitydata <- read.csv("activity.csv")
head(activitydata)
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
str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##What is mean total number of steps taken per day?

```r
#calculate total numbers of steps per day
q1data <- activitydata %>%
  group_by(date)%>%
  summarise(totalsteps = sum(steps), na.rm = T)
head(q1data)
```

```
## # A tibble: 6 x 3
##         date totalsteps na.rm
##       <fctr>      <int> <lgl>
## 1 2012-10-01         NA  TRUE
## 2 2012-10-02        126  TRUE
## 3 2012-10-03      11352  TRUE
## 4 2012-10-04      12116  TRUE
## 5 2012-10-05      13294  TRUE
## 6 2012-10-06      15420  TRUE
```

```r
#make the plot
ggplot(q1data, aes(x = totalsteps)) +
  geom_histogram() +
  ggtitle("Total Number of Steps Taken Per Day") +
  xlab("Total Number of Steps")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
#calculate the mean and the median
mean(q1data$totalsteps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(q1data$totalsteps, na.rm = T)
```

```
## [1] 10765
```

As calculated above, among all the days in the two months, the mean of total steps taken each day is 10766, and the median is 10765. 

##What is the average daily activity pattern?

```r
#calculate the mean of each interval across all days
activitydata$interval <- as.factor(activitydata$interval)
q2data <- activitydata %>%
  group_by(interval) %>%
  summarise(average = mean(steps, na.rm = T))

#make the plot
ggplot(q2data, aes(interval, average))+
  geom_line()+
  xlab("Intervals")+
  ylab("Average Steps Taken") +
  ggtitle("Average Daily Patern")
```

```
## geom_path: Each group consists of only one observation. Do you need to
## adjust the group aesthetic?
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
#interval with the maximum number of steps
which.max(q2data$average)
```

```
## [1] 104
```

```r
q2data[104,]
```

```
## # A tibble: 1 x 2
##   interval  average
##     <fctr>    <dbl>
## 1      835 206.1698
```

The interval with the maximum average number of steps taken is 835.

##Imputing missing values

```r
#calculate missing values
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```

```r
#filling in missing values
#First I created a new dataframe with an extra column that shows the mean of that interval
nomissing <- activitydata %>%
  group_by(interval) %>%
  mutate(ave = mean(steps, na.rm = T))

#Then I filled NAs with the mean of that interval
nomissing$steps[is.na(nomissing$steps)] <- nomissing$ave[is.na(nomissing$steps)]

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
nomissing$ave <- NULL
#make the plot
nomissingPlot <- nomissing %>%
  group_by(date) %>%
  summarise(totalsteps2 = sum(steps))

ggplot(nomissingPlot, aes(x = totalsteps2)) +
  geom_histogram() +
  ggtitle("Total Number of Steps Taken Per Day") +
  xlab("Total Number of Steps")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
#calculate the mean and the medium
mean(nomissingPlot$totalsteps2)
```

```
## [1] 10766.19
```

```r
median(nomissingPlot$totalsteps2)
```

```
## [1] 10766.19
```

There are 2304 missing values.

Since I filled all NAs with the mean of that interval across all days, the mean hasn't changed at all and the median has slightly increased. The overll patern has stayed the same except for the bar after the first pike, which has had a much higher frequency after filling the NAs.

##Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
nomissing$date <- ymd(nomissing$date)
nomissing$type <- ifelse(weekdays(nomissing$date) == "Saturday" | weekdays(nomissing$date) == "Sunday", "weekend", "weekday")

#make the plot
plot1 <- nomissing %>%
  filter(type == "weekend")%>%
  group_by(interval)%>%
  summarise(mean = mean(steps))
plot1
```

```
## # A tibble: 288 x 2
##    interval        mean
##      <fctr>       <dbl>
##  1        0 0.214622642
##  2        5 0.042452830
##  3       10 0.016509434
##  4       15 0.018867925
##  5       20 0.009433962
##  6       25 3.511792453
##  7       30 0.066037736
##  8       35 0.108490566
##  9       40 0.000000000
## 10       45 0.558962264
## # ... with 278 more rows
```

```r
plot2 <- nomissing %>%
  filter(type == "weekday")%>%
  group_by(interval)%>%
  summarise(mean = mean(steps))

par(mfrow=c(2,1))
plot(plot1$interval, plot1$mean, type = "l", xlab = "Intervals", ylab = "Average steps taken", main  = "Weekend")
plot(plot2$interval, plot2$mean, type = "l", xlab = "Intervals", ylab = "Average steps taken", main = "Weekday")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)





