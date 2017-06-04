
# Activity Monitoring Study
#### Author: "Aditya"
#### Date: "June 4, 2017"


#### Summary: *This Report captures the Analysis done to analyse and interpret the data collected from personal activity monitoring device of an Individual for 2 months.*

## The below R code chunk will load the R Packages


```r
        library("markdown")
        library("rmarkdown")
        library("knitr")
        library("dplyr")
        library("ggplot2")
```

 
## Setting R Markdown Global options

```r
        opts_chunk$set(echo=TRUE, results = "asis", message=FALSE, error=FALSE, warning=FALSE, cache = TRUE)
```

  
## Downloading the Source Data and Reading into R


```r
        file_url <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        setwd <- "G:/Data Science Project/Reproducible Research/Week 2/Assignment"
        download.file(url=file_url,destfile = "Activity.zip",method = "libcurl")
        unzip("Activity.zip")
        activity<- read.csv(file ="activity.csv",header = TRUE, stringsAsFactors = FALSE)
```

## The below R code helps in understanding the structure & summary Statistics of activity table. 


```r
# Dimensions of the Activity Table
        dim(activity)
```

[1] 17568     3

```r
# Structure of the Activity Table
        str(activity)
```

'data.frame':	17568 obs. of  3 variables:
 $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
 $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

```r
# Summary statistics of the Activity Table
        summary(activity)
```

     steps            date              interval     
 Min.   :  0.00   Length:17568       Min.   :   0.0  
 1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
 Median :  0.00   Mode  :character   Median :1177.5  
 Mean   : 37.38                      Mean   :1177.5  
 3rd Qu.: 12.00                      3rd Qu.:1766.2  
 Max.   :806.00                      Max.   :2355.0  
 NA's   :2304                                        

## Identifying Missing Values in the Entire Table                

```r
        sum(is.na(activity))
```

[1] 2304

```r
        mean(is.na(activity))
```

[1] 0.04371585

## Identifying Missing Values Column/ Variable wise                

```r
        colSums(is.na(activity))
```

   steps     date interval 
    2304        0        0 

```r
        colMeans(is.na(activity))
```

    steps      date  interval 
0.1311475 0.0000000 0.0000000 

## Transforming format of Date variable 

```r
        #activity$date=as.POSIXlt(activity$date)
    
        activity$date=as.Date(activity$date)
```

## Histogram of the total number of steps taken each day


```r
        activity_no_na <- activity[!rowSums(is.na(activity)),]
    
        total_steps <- aggregate(steps~date,data=activity_no_na, FUN = sum,na.rm = FALSE)

ggplot(total_steps, aes(steps))+ geom_histogram(binwidth = 5000) + labs(x="Steps per Day",y="Frequency", title="Histogram of Total no of Steps per Day") + theme(plot.title = element_text(hjust=0.5))
```

![plot of chunk Total Steps per Day1](figure/Total Steps per Day1-1.png)

## Mean and median number of steps taken each day


```r
        round(mean(total_steps$steps))
```

[1] 10766

```r
        median(total_steps$steps)
```

[1] 10765

## Average No of Steps by Interval

```r
    avg_steps <- aggregate(steps~interval, activity_no_na, FUN=mean)

ggplot(avg_steps, aes(x=interval,y=steps))+geom_line(col="red")+labs(x="5- minute Inteval", y="Average Step Count", title="Average No of Steps by Interval") + theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

## The 5-minute interval on average contains the maximum number of steps

```r
    avg_steps<- arrange(avg_steps, desc(steps))
    paste("The 5- minute interval -- ", avg_steps[1,"interval"], ", has maximum no of steps i.e. " ,round(avg_steps[1,"steps"])   )
```

[1] "The 5- minute interval --  835 , has maximum no of steps i.e.  206"

## Imputing missing data -- Using the Average steps of each 5- minute interval


```r
    activity_new <- merge.data.frame(activity, avg_steps, by.x = "interval", by.y="interval")
    for (i in 1: nrow(activity_new))
    {   
        activity_new$steps[i]<- if (is.na(activity_new$steps.x[i])) {activity_new$steps.y[i]}                                   else {activity_new$steps.x[i]}
    }
    activity_new1 <- select(activity_new,c(interval,date,steps))
```


## Histogram of the total number of steps taken each day after missing values are imputed

```r
        total_steps1 <- aggregate(steps~date,data=activity_new1, FUN = sum)

ggplot(total_steps1, aes(steps))+ geom_histogram(binwidth = 5000) + labs(x="Steps per Day",y="Frequency", title="Histogram of Total no of Steps per Day") + theme(plot.title = element_text(hjust=0.5))
```

![plot of chunk Total Steps per Day Imputed values](figure/Total Steps per Day Imputed values-1.png)

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
    activity_new1$date<- as.POSIXlt(activity_new1$date)
    activity_new2<- activity_new1
    x <- activity_new2$date$wday
    for(i in 1:length(x))
        {activity_new2$day_flag[i] <- if(x[i] %in% c(6,7)) {"weekend"} else {"weekday"}}

    avg_steps2 <- aggregate(steps~interval+day_flag, activity_new2, FUN = sum)
    
    ggplot(avg_steps2, aes(x=interval, y=steps)) + geom_line(col="red") + facet_wrap(~day_flag) +labs(x="5- minute Inteval", y="Average Step Count", title="Average No of Steps by Interval across Weekday and Weekend") + theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk panel plot](figure/panel plot-1.png)


