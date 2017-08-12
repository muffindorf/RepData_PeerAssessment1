# Project 1 - Daily Activities
Terry Wang  
8/11/2017  

In this report we look at a dataset that is a record of the number of steps in each 5-minute interval. First we load the data.


```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename <- "repdata_data_activity.zip"
if(!file.exists(filename)) {
        download.file(fileurl, filename, method="curl")
}
if (!file.exists("activity")) {
        unzip(filename) 
}
data <- read.csv("activity.csv")
```

We see that this dataset contains `nrow(data)` rows, with `sum(is.na(data$steps))` missing entries for the "steps" column.  Later I will show how I chose to deal with these.

I made the following transformations of the data in order to proceed:


```r
data$steps <- as.numeric(data$steps)
data$date <- as.Date(data$date)
data$interval <- as.numeric(data$interval)
data$Interval <- as.factor(data$interval)
data$wday <- as.factor(weekdays(data$date))
data$is.NA <- is.na(data$steps)
```

I added a "wday" column, which is a factor variable indicating the day of the week, and the "is.NA" column, which indicates whether the row's steps value is NA.

In the following histogram we observe the frequency distribution of total number of steps taken on each day.


```r
step_day <- summarize(group_by(data[data$is.NA==FALSE,], date), steps = sum(steps))
gstep_day <- ggplot(step_day, aes(steps))
gstep_day + geom_histogram() + ggtitle("Frequency Distribution of Total No. of Steps per Day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The plot (omitting 8 days whose step count is NA) shows a roughly normal distribution with mean at 


```r
mean(step_day$steps, na.rm = T)
```

```
## [1] 10766.19
```

steps and median at 


```r
median(step_day$steps, na.rm = T)
```

```
## [1] 10765
```

steps.

Below is a time series plot showing the average number of steps taken per 5-minute interval, showing the daily activity pattern of the subject:


```r
activity_ptn <- summarize(group_by(data, interval), avg_steps = mean(steps, na.rm = T))
gact_ptn <- ggplot(activity_ptn, aes(interval, avg_steps))
gact_ptn+geom_line()+ggtitle("Average No. of Steps by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

We can observe a spike at around the interval corresponding to roughly 8-9am.  Using the following code we find the interval corresponding to the highest activity level:


```r
activity_ptn$interval[activity_ptn$avg_steps==max(activity_ptn$avg_steps)]
```

```
## [1] 835
```

Or the interval corresponding to about 8:35am.

We see that there are many missing values in this dataset. The below table shows how the NAs are distributed by date and by day of the week:


```r
table(data$date[data$is.NA==TRUE])
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```

```r
table(data$wday[data$is.NA==TRUE])
```

```
## 
##    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
##       576       576       288       288       288         0       288
```

We don't see any particular pattern that looks concerning.  I decided to fill in these missing values with predicted values for steps using a linear pattern regressing steps on date and day of the week. The predicted model yields some negative values, which are converted to 0s.


```r
fit_steps <- lm(steps~Interval+wday, data[data$is.NA==F,])
data$steps[data$is.NA==T] <- predict(fit_steps, data[data$is.NA==T,])
data$steps[data$steps<0] <- 0
```

After the missing values are imputed, we produce the following histogram of total number of steps per day.


```r
step_day_na <- summarize(group_by(data, date), steps = sum(steps))
gstep_day_na <- ggplot(step_day_na, aes(steps))
gstep_day_na <- gstep_day_na + geom_histogram()
gstep_day_na <- gstep_day_na + ggtitle("Frequency Distribution of Total No. of Steps per Day, NAs Imputed")
gstep_day_na
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

This looks not much different from the histogram without taking into account of the missing values.

Are the activity patterns different during weekdays and during weekends? We plot the average steps per interval during weekdays and during weekends on separate plots:


```r
data$wknd <- data$wday=="Saturday"|data$wday=="Sunday"
steps_dw <- summarize(group_by(data, interval, wknd), avg_steps = mean(steps))
gsteps_dw <- ggplot(steps_dw, aes(interval, avg_steps))+geom_line()+facet_grid(.~wknd)+ggtitle("Activity Patterns, Weekdays (FALSE) and Weekends (TRUE)")
gsteps_dw
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

We see that the subject has a spike of activities at around 8:30am during weekdays, perhaps indicating some commute/exercise. During weekends, the spike around 8-9am is less pronounced, while the activity level during other intervals tends to be more intense than during weekdays.
