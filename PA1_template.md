# Reproducible Research: Peer Assessment 1



## Data loading and preprocessing
In the forked repository the data is zipped, therefor it needs to be unzip into a variable called activity for further processing.

```r
unzip("activity.zip")
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```
## Question 1: What is mean total number of steps taken per day?
To answer this question we filter out of the activity variable all NA values into activity_woNA
The clean set is aggregated (SUM) to activity_woNA.grouped as the steps taken per day
With activity_woNA.grouped we create a historgam to show the frequency distribution

```r
activity_woNA <- activity[!is.na(activity$steps),]
activity_woNA.grouped <- aggregate(activity_woNA$steps,list(activity_woNA$date),sum)
hist(activity_woNA.grouped$x, main="Histogram of total steps per day", xlab="Nuber of steps per day (total)",breaks=15)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

In the histogram we can see, that the person who is wearing the device takes more than 10000 steps per day most of the time.

Than we calculate the mean and median for the steps taken the person is taking per day:

```r
activity_woNA.mean <- mean(activity_woNA.grouped$x)
activity_woNA.median <- median(activity_woNA.grouped$x)
```
The mean is:    1.0766189\times 10^{4} 
The median is:  10765

## Question 2: What is the average daily activity pattern?
To answer the question of avarage daily activity we need to change the aggregation from date to interval.
We also need to change the aggregation function to mean


```r
activity_woNA.day <- aggregate(activity_woNA$steps,list(activity_woNA$interval),mean)
plot(activity_woNA.day$Group.1,activity_woNA.day$x,type="l", xlab="Interval", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
activity_woNA.maxInterval <- activity_woNA.day[activity_woNA.day$x==max(activity_woNA.day$x),]
```
The interval with the most steps taken on average is 835 with 206.1698113 steps on average.

## Question 3: Imputing missing values

```r
activity.missing <- nrow( activity[is.na(activity$steps),])
```
There are 2304 rows with NA values in the set.

Replace all NA values with the calculated mean of the interval

```r
colnames(activity_woNA.day)<- c("interval", "mean")
activity.merged <- merge(activity, activity_woNA.day, by.x= "interval")
activity.merged["filled"]<- 0 
activity.merged[!is.na(activity.merged$steps),]$filled <- activity.merged[!is.na(activity.merged$steps),]$steps 
activity.merged[is.na(activity.merged$steps),]$filled <- activity.merged[is.na(activity.merged$steps),]$mean
```

Now we aggregate and plot the data

```r
activity_clean <- aggregate(activity.merged$filled,list(activity.merged$date),sum)
hist(activity_clean$x, main="Total number of steps taken each day ", xlab="Nuber of steps per day (total)",breaks=15)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The new histogram is pretty close to the first chart where we eliminated the NA.


```r
activity_clean.mean <- mean(activity_clean$x)
activity_clean.median <- median(activity_clean$x)
```

The new mean: 1.0766189\times 10^{4} is close to the woNA mean: 1.0766189\times 10^{4}

The new median: 1.0766189\times 10^{4} is close to the woNA median: 10765

## Question 4: Are there differences in activity patterns between weekdays and weekends?

First the date will ve converted from Text to date so that the weekday function works
Based on the weekday function it is distinguished if this is a weekday or weekend
According to the split new data framew for the plot will be calculated for weekdays and weekends


```r
activity.merged$date<-as.Date(activity.merged$date)
activity.merged["weekday"]<-weekdays(activity.merged$date)
activity.merged["typeOfDay"]<-NA
activity.merged$typeOfDay <- ifelse(activity.merged$weekday %in% c("Saturday","Sunday"), "weekend", "weekday")
activity.merged$typeOfDay <- as.factor(activity.merged$typeOfDay)

activity.merged_day <- aggregate(activity.merged$filled,list(activity.merged$interval,activity.merged$typeOfDay),mean)
activity.merged_weekdays <- subset(activity.merged_day,as.character(activity.merged_day$Group.2)=="weekday")
activity.merged_weekends <- subset(activity.merged_day,as.character(activity.merged_day$Group.2)=="weekend")

par(mfcol=c(1,2))
plot(activity.merged_weekdays$Group.1,activity.merged_weekdays$x,type="l", xlab="Interval", ylab="Average number of steps", main="Weekdays")
plot(activity.merged_weekends$Group.1,activity.merged_weekends$x,type="l", xlab="Interval", ylab="Average number of steps", main="Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
