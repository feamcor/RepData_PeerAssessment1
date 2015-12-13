# Reproducible Research: Peer Assessment 1
Fabio Correa (@feamcor, coursera, data science specialization, repdata-035)  
December 2015  



## Loading the Dataset

```r
filename.zip <- "activity.zip"
if(!file.exists(filename.zip)) {
    dataset.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url = dataset.url, destfile = filename.zip, method = "auto")
    message(paste(Sys.time(), "Dataset zip file downloaded!", filename.zip))
} else {
    message(paste(Sys.time(), "Dataset zip file already exists!", filename.zip))
}
```

```
## 2015-12-13 11:22:56 Dataset zip file already exists! activity.zip
```

```r
filename.csv <- "activity.csv"
if(!file.exists(filename.csv)) {
    unzip(filename.zip, overwrite = TRUE)
    message(paste(Sys.time(), "Dataset file expanded!", filename.csv))
} else {
    message(paste(Sys.time(), "Dataset file already exists!", filename.csv))
}
```

```
## 2015-12-13 11:22:56 Dataset file already exists! activity.csv
```

```r
dataset <- read.csv(filename.csv,
                    header=TRUE,
                    colClasses=c("integer", "Date", "integer"))
```
The dataset contains __17568__ observations!  
The variables included in the dataset are:

*  __steps__: Number of steps taken in a 5-minute interval (missing values are coded as NA)
*  __date__: The date on which the measurement was taken (YYYY-MM-DD)
*  __interval__: Identifier for the 5-minute interval in which measurement was taken

## Pre-processing the Dataset

```r
weekend <- c("Sat", "Sun")
date.type <- factor(weekdays(dataset$date,
                             abbreviate = TRUE) %in% weekend,
                    levels = c(TRUE, FALSE),
                    labels = c('weekend', 'weekday'))
dataset <- cbind(dataset, date.type)
```
New column __date.type__ added to dataset indicating whether date is __weekday__ or __weekend__.

## What is mean total number of steps taken per day?

```r
stepsday <- aggregate(steps ~ date, data = dataset, FUN = sum)
hist(stepsday$steps,
     freq=TRUE,
     plot=TRUE,
     main="Total Number of Steps Taken per Day",
     xlab="Number of Steps",
     ylab="Number of Days",
     col=palette("default"))
```

![](figure/stepsday-1.png) 

```r
stepsday.mean <- mean(stepsday$steps)
stepsday.median <- median(stepsday$steps)
```
Individual took an average of __10766__ steps per day.  
The median was a day where individual took __10765__ steps.

## What is the average daily activity pattern?
## Imputing missing values
## Are there differences in activity patterns between weekdays and weekends?
