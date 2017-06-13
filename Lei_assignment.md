Introduction
------------

Project assignment of week 2, Coursera course "Replicative research".

### Load and preprocess data

    activity <- data.frame(read.csv("activity.csv", header = TRUE))
    dim(activity)

    ## [1] 17568     3

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### Question 1:

What is the mean total number of steps taken per day?

Process data for plotting:

    aggreSteps <- aggregate(activity$steps, list(activity$date), FUN = sum)
    #change names for the aggregated table:
    names(aggreSteps)[[1]] <- "Date"
    names(aggreSteps)[[2]] <- "Steps"
    # delete rows with NaN in steps:
    aggreSteps <- aggreSteps[complete.cases(aggreSteps),]

Make histogram of the total number of steps taken each day:

![](Lei_assignment_files/figure-markdown_strict/hist-1.png)

### Question 2:

Calculate and report the mean and median of total number of steps taken
per day:

    steps_mean <- mean(aggreSteps$Steps)

    steps_median <- median(aggreSteps$Steps)

The mean value of the total number of steps taken per day is
1.076618910^{4}. The median of the the total number of steps taken per
day is 10765.

### What is the average daily activity pattern?

Make a time series plot (line chart) of the 5-minute interval and the
average number of steps taken, averaged across all days:

-   first get average steps for each interval --&gt; aggregate
-   then plot steps (as y) against interval
-   use line chart (type="l")

<!-- -->

    step_by_inteval <- aggregate(steps ~ interval, activity, mean)
    plot(step_by_inteval$interval, step_by_inteval$steps, type = "l",
         main="Mean of steps per day over interval",
         xlab = "Interval", ylab="Steps", col="red")

![](Lei_assignment_files/figure-markdown_strict/aggregate-1.png)

    max_interval <- step_by_inteval$interval[which.max(step_by_inteval$steps)]

The 5-minute interval 835 corresponds with the maximum number of steps.

### Imputing missing values

Substitute the missing values (NAs) in the dataset with pertinent
strategy.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs).

<!-- -->

    NA_rows <- nrow(activity) - sum(complete.cases(activity))

There are 2304 incomplete rows in the dataset.

1.  Filling in the missing values in the dataset with median for that
    day: (some how I have not made it with apply...)

<!-- -->

    mean_per_day <- aggregate(steps~date, activity, FUN = mean, na.rm=TRUE)
    names(mean_per_day)[[2]] <- "Mean_steps"

    # try lookup:
    filled_activity <- activity #copy activity to a new df
    # lookup and replace NAs:
    #filled_activity[] <- mean_per_day$Mean_steps[match(unlist(activity), mean_per_day$date)]

    # for loop wroks at least:
    for(i in 1:nrow(activity) ){
        if (is.na(activity[i,"steps"])){
            activity[i,"steps"] <- mean_per_day[activity[i,"date"],"Mean_steps"]
        }
    }

    head(filled_activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    summary(filled_activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

Again, make histogram of the total number of steps taken each day, this
time with filled dataset:

    aggreSteps_filled <- aggregate(filled_activity$steps, list(filled_activity$date), FUN = sum)
    #change names for the aggregated table:
    names(aggreSteps_filled)[[1]] <- "Date"
    names(aggreSteps_filled)[[2]] <- "Steps"

![](Lei_assignment_files/figure-markdown_strict/hist_filled-1.png)

Again, calculate and report the mean and median of total number of steps
taken per day, here for filled datasets (without NAs):

    steps_mean_filled <- mean(aggreSteps_filled$Steps, na.rm = TRUE)

    steps_median_filled <- median(aggreSteps_filled$Steps, na.rm = TRUE)

The mean value of the total number of steps taken per day is
1.076618910^{4}. The median of the the total number of steps taken per
day is 10765.

It seems that the NA substitution did not change the mean and median of
the dataset, which is good.

### Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.3.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    filled_activity_w <- mutate(filled_activity, weekend = weekdays(as.Date(filled_activity$date)) %in% c('Samstag','Sonntag')) ## Note: German version of RStudio!

1.  Make lattice plot for weekends (TRUE in the weekend column) and for
    weekdays:

<!-- -->

    library(lattice)

    ## Warning: package 'lattice' was built under R version 3.3.3

    # create factors with value labels
    filled_activity_w$weekend.f<-factor(filled_activity_w$weekend,levels=c("FALSE","TRUE"),
       labels=c("Weekdays","Weekends")) 

    # second try with helping hand:
    steps_by_interval_filled <- aggregate(steps ~ interval + weekend.f, filled_activity_w, mean)

    xyplot(steps ~ interval| weekend.f, 
           data = steps_by_interval_filled,
           main="Compare weekday and weekdays", 
           ylab="Number of steps",layout=c(1,2), type="l")

![](Lei_assignment_files/figure-markdown_strict/lattice%20plot-1.png)
