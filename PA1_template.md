# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data



```r
   data<-read.csv("activity.csv",stringsAsFactors=FALSE)
   factoreddata<-transform(data,date=as.Date(date,format="%Y-%m-%d"))
   nonMissingData<-subset(data,data[,"steps"]!="NA")
   dataperday<-aggregate(nonMissingData$steps,by=list(nonMissingData$date),sum) 
   names(dataperday)<-c("date","stepsperday")   
```


## What is mean total number of steps taken per day?

1. Historgram of Number of Steps taken per day


```r
   hist(dataperday$stepsperday,main='Histogram of Number of Steps Taken Per Day',xlab='Steps Per Day',col='red')   
```

![](./PA1_template_files/figure-html/histogram-1.png) 

2. Mean and Median total number of steps taken per day 


```r
   meanperday<-mean(dataperday$stepsperday)
   medianperday<-median(dataperday$stepsperday)
```

### The Mean steps taken per day are 1.0766189\times 10^{4} and Median steps taken per day are 10765


## What is the average daily activity pattern?

1. Time Series Plot of 5 minute interval and the average number of steps taken


```r
   steps<-nonMissingData$steps
   plot.ts(x=ts(steps,start=c(0,5),frequency=288),type="l",
           ylab="Average Number of Steps Taken",
           xlab="Time on the basis of 5 Minutes Interval",yaxt="n")
   avg<-signif(mean(steps),digits=4)
   axis(2,at=mean(steps),labels=paste("Average Value",avg,sep="\n"))
```

![](./PA1_template_files/figure-html/activitypattern-1.png) 

2. The Five Minute Interval contains Maximum number of steps on average across all the days in the dataset


```r
   maxStepRow<-subset(nonMissingData,
                      nonMissingData[,"steps"]==max(nonMissingData$steps))
```

### The 5 Minute interval Interval 610 to 615 on 2012-11-27 has maximum value of number of steps taken which is 806 

## Imputing missing values

1. Total number of Missing Values


```r
   wholerowmissing<-subset(data,is.na(data[,"steps"])|
                              is.na(data[,"date"])|
                              is.na(data[,"interval"]))
   missingCount<-length(wholerowmissing$steps)
```

### Total Number of missing rows in this data set is 2304




## Are there differences in activity patterns between weekdays and weekends?
