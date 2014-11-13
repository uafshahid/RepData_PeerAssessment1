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



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
