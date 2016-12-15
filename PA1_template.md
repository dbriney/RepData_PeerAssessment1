# Reproducible Research: Peer Assessment 1

The assignment instructions say to always use echo = TRUE. This is the default, so no need to set it or alter it. 

## Loading and preprocessing the data


```r
#Source data .zip file (repdata_data_activity.zip) downloaded from link 
#on Coursera website: https://d396qusza40orc.cloudfront.net
#Assumption: .zip has been unzipped and the .csv it contains is located in 
#the same directory as the .Rmd file.
df <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
First, make sure the dplyr package is loaded. 

```r
require(dplyr)
```

```
## Loading required package: dplyr
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
Now create a new dataframe with total number of steps per day.


```r
df2 <- df %>% group_by(date) %>% summarize(Total_Steps = sum(steps))
```

Create a histogram. Since I realized I do not understand the difference 
between a histogram and a barplot (doh!), I googled around and read this:
http://www.forbes.com/sites/naomirobbins/2012/01/04/a-histogram-is-not-a-bar-chart/#f6aefc328aff, which propelled me to create my histogram as follows.


```r
hist(df2$Total_Steps,xlab="Total Steps Per Day",ylab="Days", ylim = range(0,40),main="Total Steps Histogram, Subject X, October-November 2012",col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Next, find and report the mean and median of the total number of steps per day. 
Note that we need to ignore NA records, of which there are 8 in the dataset. 
Mean:


```r
mean(df2$Total_Steps,na.rm = T)
```

```
## [1] 10766.19
```
And median:

```r
median(df2$Total_Steps,na.rm = T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Plot the relationship of 5-minute intervals to average number of steps. Need to filter out the NAs. 


```r
df3 <- subset(df,!is.na(steps)) %>% group_by(interval) %>% summarize(Average_Steps = mean(steps))
with(df3,plot(interval,Average_Steps,type="l",xlab="5-minute interval", ylab="Average Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Next, we want to identify which interval has the maximum Average_Steps value. 


```r
interval_with_max_steps <- filter(df3, Average_Steps == max(Average_Steps))
interval_with_max_steps$interval
```

```
## [1] 835
```

## Imputing missing values

Let's find the number of rows that contain missing values (NAs).


```r
df_with_na <- df[!complete.cases(df),]
str(df_with_na)
```

```
## 'data.frame':	2304 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

OK, now update the original data.frame where the steps column is "NA" with the average for the corresponding interval value. This seems a bit dubious, but I'm doing what I'm told to do. 

Reminder that "df" is the untouched R data.frame representation of activity.csv, and
"df3" is the average number of steps by interval (with NAs ignored).


```r
#Merge the two DFs on interval. This will give us a fourth Average_Steps column.
df4 <- merge(df,df3,by="interval")
#Update the steps column when NA with the average for that interval.
df4$steps <- ifelse(is.na(df4$steps),df4$Average_Steps,df4$steps)
```
Present the same histogram and mean/median values as above, only this time after having updated all the NAs in the step column. 


```r
df5 <- df4 %>% group_by(date) %>% summarize(Total_Steps = sum(steps))
```
The revised histogram:


```r
hist(df5$Total_Steps,xlab="Total Steps Per Day",ylab="Days", ylim = range(0,40),main="Total Steps Histogram (updated NAs), Subject X, October-November 2012",col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

And the revised mean and median:


```r
mean(df5$Total_Steps)
```

```
## [1] 10766.19
```

```r
median(df5$Total_Steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
