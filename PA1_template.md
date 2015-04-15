---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
***




Processing date/time is Wed Apr 15 11:58:09 AM 2015.

**Required Libraries**


```r
library(knitr)          ## Required to set opts_chunk within this document
library(ggplot2)        ## Graphing tools
library(scales)         ## Used for time axis ticks in ggplot2
library(reshape2)       ## Used to melt dataset for ggplot2 facets
```

**Global Options**


```r
opts_chunk$set(echo=TRUE, results="asis")
```

## Loading and preprocessing the data


```r
srcfile <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile <- "activity.zip"
csvfile <- "activity.csv"
if(!file.exists(csvfile) | !file.exists(zipfile)) {
        download.file(srcfile, zipfile)
        unzip(zipfile)
}
print(downloadtime <- file.info(zipfile)$mtime)
```

[1] "2015-04-08 02:19:34 EDT"

- The source file was downloaded from **https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip** on **2015-04-08 02:19:34**.


```r
dat <- read.csv(csvfile)
```


## What is mean total number of steps taken per day?


```r
# Per assignment instructions, we will ignore NAs for this step
dat2 <- dat[!is.na(dat$steps),] 
dsum <- aggregate(dat2$steps, list(dat2$date), sum)
colnames(dsum) <- c("Date", "TotalSteps")
```


```r
ggplot(dsum, aes(x=TotalSteps)) + 
        geom_histogram(binwidth=1000, col="lightblue", fill="red") +
        ggtitle("Total Steps Per Day") + 
        xlab("Total Steps") + ylab("Frequency")
```

![plot of chunk PlotTotalSteps](figure/PlotTotalSteps-1.png) 


```r
print(mean(dsum$TotalSteps))
```

[1] 10766.19

```r
print(median(dsum$TotalSteps))
```

[1] 10765



## What is the average daily activity pattern?


```r
# Again per instructions, we will ignore NA values for this step.
dat2 <- dat[!is.na(dat$steps),]
davg <- aggregate(dat2$steps, list(dat2$interval), mean)
colnames(davg) <- c("interval", "AverageSteps")

# Format intervals to show as time slots
davg$Times <- sprintf("%02d:%02d",floor(davg$interval/100), davg$interval%%100)
```


```r
ggplot(davg, aes(x=Times, y=AverageSteps, group=1)) + geom_line() +
        scale_x_discrete(breaks=c(davg[grep(":00", davg$Times),]$Times)) +
        theme(axis.text.x=element_text(angle=60, hjust=1.2 )) +
        ggtitle("Average Steps Taken per 5-Minute Interval,\nAveraged Across All Days") + 
        xlab("Times") + 
        ylab("Average Steps")
```

![plot of chunk PlotIntervals](figure/PlotIntervals-1.png) 


```r
print(maxsteps <- max(davg$AverageSteps))
```

[1] 206.1698

```r
print(avgmax <- davg[davg$AverageSteps==max(davg$AverageSteps),]$Times)
```

[1] "08:35"

- The maximum number of steps, **206.1698113**, occurred during the 5-minute interval of **08:35**.

## Imputing missing values


```r
sprintf("%d NAs were found in the original dataset", sum(is.na(dat$steps)))
```

[1] "2304 NAs were found in the original dataset"


```r
# Create new dataseet to include imputed values
idat <- dat

# Impute using average for that 5-minute interval
avg_by_interval <- lapply(split(dat2$steps, dat2$interval), mean) 
idat$impbyint <- unlist(ifelse(!is.na(idat$steps), idat$steps, 
                               avg_by_interval[as.character(idat$interval)]))

# Impute using average for that day (or use 0 if no entries for that day)
avg_by_day      <- lapply(split(dat2$steps, dat2$date), mean) 
avg_by_day[is.na(avg_by_day)] <- 0   ## Use 0 if no entries for that day
idat$impbyday <- unlist(ifelse(!is.na(idat$steps), idat$steps, 
                                    avg_by_day[as.character(idat$date)]))

# Impute using Zeroes to replace all NAs
idat$impbyzer <- unlist(ifelse(!is.na(idat$steps), idat$steps, 0))

# Calculate total for each column
isum <- aggregate(list(idat[1],idat[4:6]), list(idat$date), sum)
colnames(isum) <- c("Date", "OriginalTotalSteps","ImputedWithIntervalAverages",
                    "ImputedWithDailyAverages","ImputedWithZeroes")

# Melt dataset into long form to prepare to plot
isum2 <- melt(isum,"Date")
colnames(isum2) <- c("Date", "ImputedType", "Steps")
```

**Compare effects of imputing values**


```r
ggplot(isum2, aes(x=Steps)) + 
        geom_histogram(binwidth=1000, col="lightblue", fill="red") +
        facet_wrap( ~ ImputedType) +
        ggtitle("Total Steps Per Day with Imputed Values") + 
        xlab("Total Steps") + ylab("Frequency")
```

![plot of chunk PlotComparisons](figure/PlotComparisons-1.png) 

**Impact and Differences by imputing values**


```r
mean_tot    <- summary(isum)[4,2]
median_tot  <- summary(isum)[3,2]
mean_iint   <- summary(isum)[4,"ImputedWithIntervalAverages"]
median_iint <- summary(isum)[3,"ImputedWithIntervalAverages"]
mean_iday   <- summary(isum)[4,"ImputedWithDailyAverages"]
median_iday <- summary(isum)[3,"ImputedWithDailyAverages"]
mean_izer   <- summary(isum)[4,"ImputedWithZeroes"]
median_izer <- summary(isum)[3,"ImputedWithZeroes"]
```

When we impute values using the average of the 5-minute intervals, the outcomes differ slightly from our original estimates.  Comparing mean and median, we see:

- With no imputed values, where NAs are simply ignored, we have:  **Mean   :10766  ** and **Median :10765  **.

- When we impute values using the average of the 5-minute intervals, we have:   **Mean   :10766  ** and **Median :10766  **.

If we, however, impute values based upon daily averages, or by replacing NAs with zeroes, then the values are different from the original estimate, but are identical to each other.  Notice the mean and median as follows:

- When we impute values using the average of the daily total steps, we have:    **Mean   : 9354  ** and **Median :10395  **.

- When we impute values using zeroes to fill in where there are NAs, we have:   **Mean   : 9354  ** and **Median :10395  **.



## Are there differences in activity patterns between weekdays and weekends?


```r
# Create new column for day type (weekend or weekday)
idat$daytype <- weekdays(as.POSIXlt(idat$date), abbreviate=TRUE)
idat$daytype <- as.factor(ifelse(idat$daytype %in% c("Sat","Sun"), "Weekends", "Weekdays"))

# Calculate new totals by daytype and interval
iavg2 <- aggregate(list(idat[1],idat[4:6]), list(idat$daytype,idat$interval), mean)
colnames(iavg2) <- c("DayType", "interval",
                     "OriginalTotalSteps","ImputedWithIntervalAverages",
                     "ImputedWithDailyAverages","ImputedWithZeroes")
iavg2$Times <- sprintf("%02d:%02d",floor(iavg2$interval/100), iavg2$interval%%100)
```


```r
ggplot(iavg2, aes(x=Times, y=ImputedWithIntervalAverages, group=1)) + geom_line() +
        scale_x_discrete(breaks=c(iavg2[grep(":00", iavg2$Times),]$Times)) +
        theme(axis.text.x=element_text(angle=60, hjust=1.2 )) +
        ggtitle("Average Steps Taken By 5-Minute Intervals,\nWeekday Activity Pattern vs Weekend Activity Pattern") + 
        facet_grid(DayType ~ ., scales="free") + 
        xlab("5-Minute Time Intervals") + 
        ylab("Average Steps including Imputed Values")
```

![plot of chunk PlotIntervalsByDayType](figure/PlotIntervalsByDayType-1.png) 

The above plot shows the differences in activity patterns between weekdays and weekends.


