# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The data are provided in this catalog, but may also be obtained from
 [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

Data are read from this file, and date strings are formatted into type Date for convenience.


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

Some statistics describing the datasets

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
head(activity)
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
summary(activity)
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
The missing values are ignored for these computations.

The total number of steps are shown as a histogram below.

```r
totalStep <- aggregate(steps ~ date, data = activity, sum)
minSteps <- min(totalStep$steps)
maxSteps <- max(totalStep$steps)
noOfBins <- ceiling((maxSteps - minSteps)/1000)

library("ggplot2")
ggplot(totalStep, aes(steps)) +
  geom_histogram(bins = noOfBins) +
  coord_cartesian(xlim = c(minSteps, maxSteps)) +
  scale_y_continuous(breaks = seq(0, maxSteps, 2)) +
  xlab ("steps per day")
```

![](PA1_template_files/figure-html/steps_per_day_hist-1.png)<!-- -->

The average steps per day lays in the area of 10,000. The distribution appears 
to be quite uniform, with little/no skewness. 


```r
mn <- format(round(mean(totalStep$steps), 0), big.mark = ",")
med <- format(median(totalStep$steps), big.mark = ",")
```

This is confirmed by means and median, which are nearly identical. 
Mean = 10,766,  Median = 10,765.


## What is the average daily activity pattern?
The plot below shows the daily activity pattern. We can see that night is quiet, and 
mornings are busy.

```r
summaryStep <- aggregate(steps ~ interval, data = activity, mean)
plot(summaryStep$interval, summaryStep$steps, type= "l", xlab= "Interval (resolution = 5 min)", 
       ylab= "No of steps",  col="green" , lwd=2)
```

![](PA1_template_files/figure-html/daily_activity_pattern-1.png)<!-- -->

We expect to find the peek interval in the morning hours.

```r
peekInterval <- summaryStep[summaryStep$steps == max(summaryStep$steps), "interval"]
format(strptime(sprintf("%04d", peekInterval), format="%H%M"), format = "%H:%M")
```

```
## [1] "08:35"
```

## Imputing missing values

### 1 Overview
Let´s get an overview over the missing values


```r
summary(is.na(activity))
```

```
##    steps            date          interval      
##  Mode :logical   Mode :logical   Mode :logical  
##  FALSE:15264     FALSE:17568     FALSE:17568    
##  TRUE :2304      NA's :0         NA's :0        
##  NA's :0
```

We observe that there are only the column "steps" that have missing values.
Let´s look at the distribution of this NAs.


```r
datesWithNas <- unique(activity[is.na(activity$steps) == TRUE, "date"])
datesWithValues <- unique(activity[is.na(activity$steps) == FALSE, "date"])
both <- as.Date(intersect(datesWithNas, datesWithValues), origin = "1970-01-01")
if(length(both) > 0) {
  both
} else {
  print("No dates contains both NAs and values")
}
```

```
## [1] "No dates contains both NAs and values"
```

Surprisingly, we see that every date either has only NAs or only values.

### 2 Strategy for filling missing values
Normally I would just eliminate the dates that only have NAs.

But since the assignment asks to fill in values, the mean value is chosen to be inserted.

### 3 Imputing the missing values
We will make a clean copy of the original dataframe and insert the missing values here.

```r
mn <- aggregate(steps ~ interval, data = activity, mean)
imputed <- activity
impute <- function(steps, interval) {
    if (!is.na(steps))
        return(c(steps))
    else
        return((mn[mn$interval==interval, "steps"]))
}
imputed$steps <- mapply(impute, imputed$steps, imputed$interval)
```

### 4 Histogram and summary by means cleaned dataset
First we plot the histogram to see if it is affected by the imputation.

```r
totalStepImputed <- aggregate(steps ~ date, data = imputed, sum)
minSteps <- min(totalStepImputed$steps)
maxSteps <- max(totalStepImputed$steps)
noOfBins <- ceiling((maxSteps - minSteps)/1000)

ggplot(totalStepImputed, aes(steps)) +
  geom_histogram(bins = noOfBins) +
  coord_cartesian(xlim = c(minSteps, maxSteps)) +
  scale_y_continuous(breaks = seq(0, maxSteps, 2)) +
  xlab ("steps per day")
```

![](PA1_template_files/figure-html/imputed_histogram-1.png)<!-- -->

The imputation does indeed affect the distribution. Since the imputation add mean values,
it will show higher rates around the mean value than without imputation. To impute for
entire dates which only contains NA values has probably distorted the data set.

How has this affected the mean and median?

```r
mnUncleaned <- format(round(mean(totalStep$steps), 0), big.mark = ",")
mnCleaned <- format(round(mean(totalStepImputed$steps), 0), big.mark = ",")
medUncleaned <- format(round(median(totalStep$steps),0), big.mark = ",")
medCleaned <- format(round(median(totalStepImputed$steps),0), big.mark = ",")
```

Unleaned mean = 10,766,  Cleaned mean = 10,766. 
Since the mean was filling value, it will not affect the total mean.

Unleaned median = 10,765,  Cleaned median = 10,766. 
There is a slight redistribution, but there is little skewness, hence median is almost identical.


## Are there differences in activity patterns between weekdays and weekends?

To find out this, we will factor out weekday/weekend as a factor variable

```r
imputed$day <- weekdays(imputed$date)
imputed$weekday <- ifelse(imputed$day %in% c("Saturday","Sunday"), "weekend", "weekday")
imputed$weekday <- as.factor(imputed$weekday)
```

Then we compare the activity (steps) during weekdays and weekends.

```r
weekdaySummary <- aggregate(steps ~ interval + weekday, data = imputed, mean)

ggplot(weekdaySummary, aes(interval, steps)) +
  geom_line(color = "green") +
  facet_grid( . ~ weekday) +
  xlab ("Interval (resolution = 5 min)") +
  ylab ("No of steps")
```

![](PA1_template_files/figure-html/cmp_weekdays_weekend-1.png)<!-- -->

There are distinct differences in patterns. Weekdates has a higher peek activity during the morning, 
while weekends has higher activity during afternoons and evenings.
