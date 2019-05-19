---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Step 1
## Setting the **Working Directory** and preparing the file.

###   *Activating the packages you need and changing the format of the date.*

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.5.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```r
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
activity <- read.csv("activity.csv")
activity$date<-ymd(activity$date)
```

# Step 2
## Getting the Total, Mean and Mediam Nbr of Steps per Day

###   *Getting the total nbr of steps per day.*

```r
st2 <- data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
st2$date <- rownames(st2)
names(st2)[[1]] <- "Total Steps"
rownames(st2) <- NULL

range <- c(0, 5000, 10000, 20000, 25000)
col <- findInterval(st2$`Total Steps`, range, all.inside = TRUE)
col[which(col==1)] <- "cornflowerblue"
col[which(col==2)] <- "coral"
col[which(col==3)] <- "cyan"
col[which(col==4)] <- "cadetblue3"
hist(st2$'Total Steps', main = "Histogram for the Total Nbr of Steps", 
     ylab = "Frequency in Days", xlab = "Nbr of Steps", col = col)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

###   *Getting the **mean** and **median** of the **total number of steps taken per day**.*

```r
st3 <- data.frame(round(tapply(activity$steps,activity$date,mean,na.rm=TRUE),2))
st3$Date <- rownames(st3)
rownames(st3) <- NULL
names(st3)[[1]] <- "MeanSteps"
st3temp <- activity %>% 
  select(date,steps) %>% 
  group_by(date) %>% 
  summarise(median(steps))
names(st3temp)[[2]]<-"MedianSteps"
st3$MedianSteps <- st3temp$MedianSteps
st3$TotSteps <- st2$`Total Steps`
st3 <- st3 %>% 
  select("Date","TotSteps","MeanSteps","MedianSteps")
```

###   *Removing the NAs and creating a Time Series Plot for the*
###   *Average Nbr of Steps per day.*

```r
st4 <- st3
st4$Date <- as.Date(st4$Date, format = "%Y-%m-%d")
st4 <- st4[complete.cases(st4[ , 2:3]),]

ggplot(st4,aes(x = st4$Date, y = st4$MeanSteps)) + 
  geom_line(stat = "identity") + scale_x_date() + 
  ylab("Mean Steps Every day") + xlab("Date") + 
  ggtitle("Mean Steps per Day") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

# Step 3
## The maximum mean nbr of steps per interval per day

```r
st5 <- aggregate(data = activity, steps~interval, FUN = "mean")
st5_1 <- aggregate(data = activity, steps~interval, FUN = "median")
ggplot(st5,aes(x = st5$interval, y = st5$steps)) + 
  geom_line(stat = "identity") + 
  ylab("Steps") + xlab("5 min Interval") + 
  ggtitle("Average Steps per Interval per Day") + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
Max_st5 <- st5[which.max(st5$steps),1]
```

# Step 4
## Imputation of the missing values to the masterfile

###   *Count of all missing values for the whole dataset and per columns.*

```r
TotMissVal <- sum(is.na(activity))
ColMissVal <- colSums(is.na(activity))
```

###   *Imputing the mean steps per interval to the corresponding missing values.*

```r
for(i in 1:nrow(activity))
{
  if(is.na(activity$steps[i])){
    activity$steps[i] <- st5$steps[which(st5$interval == activity$interval[i])]
  }}
```

###   *Getting the total steps per day and creating a histogram for*
###   *the new imputed master file.*

```r
imp_act <- data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
imp_act$date <- rownames(imp_act)
names(imp_act)[[1]] <- "TotSteps"
rownames(imp_act) <- NULL

range <- c(0, 5000, 10000, 20000, 25000)
col <- findInterval(st2$`Total Steps`, range, all.inside = TRUE)
col[which(col==1)] <- "cornflowerblue"
col[which(col==2)] <- "coral"
col[which(col==3)] <- "cyan"
col[which(col==4)] <- "cadetblue3"
hist(imp_act$TotSteps, main = "Histogram for the Total Nbr of Steps", 
     ylab = "Frequency in Days", xlab = "Nbr of Steps", col = col)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

###   *Getting the mean and median of the total steps.*

```r
imp_act$MeanSteps <- tapply(activity$steps,activity$date,mean,na.rm=TRUE)
imp_act$MedSteps <- tapply(activity$steps,activity$date,median,na.rm=TRUE)
imp_act <- imp_act[,c(2,1,3,4)]
```

# Step 5
## Creating an indicator for weekday and weekend

```r
activity$DateInd <- ifelse(wday(activity$date) == 1 | wday(activity$date) == 7,0,1)
```

## Creating a plot for average steps per weekend and per weekdays

###   *Getting the average steps per weekday.*

```r
activityWE <- activity[activity$DateInd == 0,]
activityWE <- aggregate(data = activityWE, steps~interval, FUN = "mean")
activityWD <- activity[activity$DateInd == 1,]
activityWD <- aggregate(data = activityWD, steps~interval, FUN = "mean")
```

###   *Creating the Time Series Plot for total steps per interval*
###   *per Weekend and per Weekday.*

```r
actWEPlot <- ggplot(activityWE,aes(y = activityWE$steps, x = activityWE$interval)) + 
  geom_line(stat = "identity") + ylab("Average Steps") + 
  xlab("5 Minute Interval for Weekends") + 
  ggtitle("Average Steps per Weekend")+
  theme(plot.title = element_text(hjust = 0.5))

actWDPlot <- ggplot(activityWD,aes(y = activityWD$steps, x = activityWD$interval)) + 
  geom_line(stat = "identity") + ylab("Average Steps") + 
  xlab("5 Minute Interval for Weekdays") + 
  ggtitle("Average Steps per Weekday")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(actWEPlot, actWDPlot, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

