# Reproducible Research: Peer Assessment 1
COURSERA REVIEWER: please see figures folder if the plots do not display in your browser. Thanks!
Connect with me on Linkedin: https://www.linkedin.com/in/hadrien-dykiel-29a78432

#Set up R environment
Set working directory and load requried packages, etc.

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
library(plyr)
library(mice) #used to impute missing values
```

```
## Warning: package 'mice' was built under R version 3.2.4
```

```
## Loading required package: Rcpp
```

```
## mice 2.25 2015-11-09
```

```r
library(chron)
library(cowplot)#needed to graph multiple plots
```

```
## Warning: package 'cowplot' was built under R version 3.2.4
```

```
## 
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     ggsave
```


## Loading and preprocessing the data
Save the CSV file to a data frame. Remove NAs. Summarize data set for later use.

```r
df <- read.csv('Data/activity.csv') #Load Data
df2 <- df[!is.na(df$steps), ] #remove NAs
df2_summarized <- ddply(df2, c("date"), summarize, sumSteps = sum(steps)) #summarize
```



## What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day:

```r
hist <- qplot(df2_summarized$sumSteps,
      xlab='Total Steps Each Day',
      ylab='Frequency')

#save to figures folder
png('figures/totalStepsPerDay.png', width=1000)
hist
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
dev.off()
```

```
## png 
##   2
```

```r
#print plot
hist
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Plot total number of steps per date (out of curiousity, not part of assignment)

```r
#plot results
ggplot(data=df2_summarized, mapping = aes(x=date, y=sumSteps)) +
  geom_bar(stat="identity", fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90)) + #rotate x axis labels
  ggtitle('Steps Per Day') +
  theme(plot.title = element_text(face='bold', size=16))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Calculate mean & median
mean <- mean(df2_summarized$sumSteps, na.rm = TRUE)
median <- median(df2_summarized$sumSteps, na.rm = TRUE)
```
The mean number of steps is 1.0766189\times 10^{4} and the median is 10765.


## What is the average daily activity pattern?

```r
timeSeries <- ggplot(data=df2, aes(interval, steps)) +
  stat_summary(fun.y = 'mean', geom = 'line') +
  ggtitle('Avg Steps Per 5 Minute Interval') +
  theme(plot.title = element_text(face='bold', size=16))

timeSeries #print plot
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#save to figures folder
png('figures/activityPattern.png', width=1000)
timeSeries
dev.off()
```

```
## png 
##   2
```

```r
avg_steps_by_interval <- ddply(df2, c("interval"), summarize, avgSteps = sum(steps)) #summarize

#Print busiest time
busiestInterval <- (avg_steps_by_interval$interval[avg_steps_by_interval$avgSteps == max(avg_steps_by_interval$avgSteps, na.rm = T)])
```

The busiest interval is 835.

## Impute missing values and re-plot results
We'll use the mice package that imputes missing values with plausible values "drawn from a distribution specifically designed for each missing datapoint"(http://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/).

```r
sum(is.na(df$steps)) #calculate sum of NAs
```

```
## [1] 2304
```

```r
tempData <- mice(df, seed = 500) #impute missing values
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   1   4  steps
##   1   5  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   2   4  steps
##   2   5  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   3   4  steps
##   3   5  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   4   4  steps
##   4   5  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   5   4  steps
##   5   5  steps
```

```r
df3 <- complete(tempData, 1) 
df3_summarized <- ddply(df3, c("date"), summarize, sumSteps = sum(steps)) #summarize

hist3 <- qplot(df3_summarized$sumSteps,
      xlab='Total Steps Each Day (Imputed)',
      ylab='Frequency')

#save to figures folder
png('figures/totalStepsPerDayImputed.png', width=1000)
hist3
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
dev.off()
```

```
## png 
##   2
```

```r
#print plot
hist3
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#Plot steps per day
ggplot(data=df3_summarized, mapping = aes(x=date, y=sumSteps)) +
  geom_bar(stat="identity", fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90)) + #rotate x axis labels
  ggtitle('Steps Per Day (Imputed)') +
  theme(plot.title = element_text(face='bold', size=16))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
ggplot(data=df3, aes(interval, steps)) +
  stat_summary(fun.y = 'mean', geom = 'line') +
  ggtitle('Avg Steps Per 5 Minute Interval (Imputed)') +
  theme(plot.title = element_text(face='bold', size=16)) 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
#Calculate mean & median
mean_imputed <- mean(df3_summarized$sumSteps, na.rm = TRUE)
median_imputed <- median(df3_summarized$sumSteps, na.rm = TRUE)
```
After imputation, the new mean and median are ` r mean_imputed` and 11352, respectively.


## Are there differences in activity patterns between weekdays and weekends?

```r
df3$weekend <- is.weekend(df3$date)
for (i in 1:nrow(df3)) {

  if (df3$weekend[i] == TRUE) {
    df3$dayType[i] <- 'weekend'
  } else {
    df3$dayType[i] <- 'weekday'
  }
}

plot_weekend <- ggplot(data=subset(df3, dayType == 'weekend'), aes(interval, steps)) +
  stat_summary(fun.y = 'mean', geom = 'line') +
  ggtitle('Weekend Avg Steps Per 5 Minute Interval (Imputed)') +
  theme(plot.title = element_text(face='bold', size=16)) 

plot_weekday <- ggplot(data=subset(df3, dayType == 'weekday'), aes(interval, steps)) +
  stat_summary(fun.y = 'mean', geom = 'line') +
  ggtitle('Weekday Avg Steps Per 5 Minute Interval (Imputed)') +
  theme(plot.title = element_text(face='bold', size=16)) 

#Save plot to figures folder
png('figures/final_figure.png')
plot_grid(plot_weekday, plot_weekend, ncol = 1, nrow=2)
dev.off()
```

```
## png 
##   2
```

```r
#Print plot
plot_grid(plot_weekday, plot_weekend, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


