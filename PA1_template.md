#### Loading and preprocessing the data

The data was loaded using by first using the download.file function and
then after unzipping using read.csv.

1.  Load the data (i.e. **<span style="color:red">read.csv()</span>**)

<!-- -->

            
            library(data.table)
            library(dplyr)
            library(ggplot2)
            ## Setup the working directory where the data is located
            setwd("C:/Users/paddy/Documents/Coursera/Assignments/Reproducable Research/Week 2/Reproducible Research Course Project 1")
            #setwd("C:/Users/paddy/Documents/Coursera/Data Science/Course Material/5. Reproducable Research/Assignments/Week 2/Reproducible Research Course Project 1")
            ## Download the raw archive file from the web: 
            fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
            download.file(fileUrl, destfile = "./repdata%2Fdata%2Factivity.zip")
            dateDownloaded <- date()
            dateDownloaded
            unzip(zipfile = "./repdata%2Fdata%2Factivity.zip")

It is always good to take a brief look at the structure of the data.
This was done via str and head functions.

            ## Load the test data
            data <- read.csv("./activity.csv")
            ## Take a quick look at the data
            str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

            head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

1.  Some small post processing of the data was carried out to facilitate
    later analysis.

<!-- -->

            ## Load data into dplyr
            data1 <- tbl_df(data)
            rm(data)
            data1$date<-as.Date(data1$date)
            data1$interval<-as.factor(data1$interval)

#### What is mean total number of steps taken per day?

For the initial part of the analysis the dataset missing values were
ignored.

1.  The data was grouped by date in order to calculate the total number
    of steps taken per day

<!-- -->

            ## Load data into dplyr
            daily_steps<- 
                    data1%>%
                    filter(!is.na(steps)) %>%
                    group_by(date) %>%
                    summarise(total_steps=sum(steps))

1.  This can be seen in the histogram below showing the total number of
    steps taken each day

<!-- -->

            ## Histogram of the total daily steps during the sample period
            hist(daily_steps$total_steps,breaks = 5,xlab = "Total Daily Steps", main = "Total Steps per Day")
            abline(v=median(daily_steps$total_steps),lty=3,lwd=2)
            abline(v=mean(daily_steps$total_steps))

![](PA1_template_files/figure-markdown_strict/hist_steps-1.png)

1.  This the mean and median of the total number of steps taken per day
    during the sample perioed

<!-- -->

            mean(daily_steps$total_steps)

    ## [1] 10766.19

            median(daily_steps$total_steps)

    ## [1] 10765

#### What is the average daily activity pattern?

1.  The time series plot (i.e. **<span style="color:red">type =
    "l"</span>**) below shows the 5-minute interval (x-axis) and the
    average number of steps taken, averaged across all days (y-axis)

<!-- -->

            ## Summarise data into average steps per 5-min interval
            step_intervals<- 
                    data1%>%
                    filter(!is.na(steps)) %>%
                    group_by(interval) %>%
                    summarise(avg_steps=mean(steps))
            ## max(step_intervals$interval)
         summary (step_intervals)

    ##     interval     avg_steps      
    ##  0      :  1   Min.   :  0.000  
    ##  5      :  1   1st Qu.:  2.486  
    ##  10     :  1   Median : 34.113  
    ##  15     :  1   Mean   : 37.383  
    ##  20     :  1   3rd Qu.: 52.835  
    ##  25     :  1   Max.   :206.170  
    ##  (Other):282

    plot(step_intervals$interval, step_intervals$avg_steps, type="n", xlab="Interval",
         ylab="Average Steps per 5-min across full monitoring period" )
    lines(step_intervals$interval, step_intervals$avg_steps, type="l",col="blue" )
    title("Average Steps(per 5-min across full monitoring period)")

![](PA1_template_files/figure-markdown_strict/AvgNoStepInt-1.png)

1.  We ca see that interval 835 contains the maximum number of
    steps (206) on average across all the days in the dataset.

<!-- -->

            head(arrange(step_intervals, desc(avg_steps)),1)

    ## # A tibble: 1 x 2
    ##   interval avg_steps
    ##   <fct>        <dbl>
    ## 1 835            206

#### Inputing missing values

1.  The total number of rows with missing values (coded as NA) is shown
    below.

<!-- -->

            sum(!complete.cases(data1)) 

    ## [1] 2304

            #To view which rows have missing data
            # data1[!complete.cases(data1), ]

1.  The missing values in the dataset are filled by simply using the
    mean value for that 5-minute interval. A new dataframe data2 is then
    created which has these filled values.

<!-- -->

            ##clean_data2 <- subset(data1, !is.na(data1$steps))
            data2<-data1
            na_steps<-is.na(data1$steps)
            clean_data<-data1[complete.cases(data1),]
            avg_inter <- tapply(clean_data$steps, clean_data$interval, mean, na.rm=TRUE, simplify=T)
            data2$steps[na_steps] <- avg_inter[as.character(data2$interval[na_steps])]

1.  The first few rows of this new dataset (data2) can be seen here.
    This can be compared with the original dataset (data1) were is
    contained missing data in these same rows. The summary function for
    data2 doesn't show any NAs.

<!-- -->

            head(data1)
    ## # A tibble: 6 x 3
    ##   steps date       interval
    ##   <int> <date>     <fct>   
    ## 1    NA 2012-10-01 0       
    ## 2    NA 2012-10-01 5       
    ## 3    NA 2012-10-01 10      
    ## 4    NA 2012-10-01 15      
    ## 5    NA 2012-10-01 20      
    ## 6    NA 2012-10-01 25
            head(data2)
    ## # A tibble: 6 x 3
    ##    steps date       interval
    ##    <dbl> <date>     <fct>   
    ## 1 1.72   2012-10-01 0       
    ## 2 0.340  2012-10-01 5       
    ## 3 0.132  2012-10-01 10      
    ## 4 0.151  2012-10-01 15      
    ## 5 0.0755 2012-10-01 20      
    ## 6 2.09   2012-10-01 25
            summary(data2)
    ##      steps             date               interval    
    ##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
    ##  Median :  0.00   Median :2012-10-31   10     :   61  
    ##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
    ##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   20     :   61  
    ##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
    ##                                        (Other):17202

1.  The new dataset is again grouped by date to show the total daily
    steps.

<!-- -->

           library(dplyr)
             ## Load data into dplyr
            NewDailySteps<- 
                    data2%>%
                    group_by(date) %>%
                    summarise(total_steps=sum(steps))
            head(NewDailySteps)

    ## # A tibble: 6 x 2
    ##   date       total_steps
    ##   <date>           <dbl>
    ## 1 2012-10-01       10766
    ## 2 2012-10-02         126
    ## 3 2012-10-03       11352
    ## 4 2012-10-04       12116
    ## 5 2012-10-05       13294
    ## 6 2012-10-06       15420

Below is an updated histogram from the filled in dataset of the total
number of steps taken each day.
![](PA1_template_files/figure-markdown_strict/hist_DailyStep_filled-1.png)

It can be seen that there is no change to the mean and only minor change
to the median total number of steps when using the imputed dataframe.
Using the mean interval values for imputing the missing data resulted in
an increase of samples around the mean value.

            mean(NewDailySteps$total_steps)

    ## [1] 10766.19

            median(NewDailySteps$total_steps)

    ## [1] 10766.19

#### Are there differences in activity patterns between weekdays and weekends?

A comparison was made of the weekday vs. weekend step activity.

The weekdays() function was used to flag each row into its relevant
group.

1.  A new factor variable weekeday was added to a copy of the filled
    dataset with two levels – “weekday” and “weekend” becoming data3.
    This indicated whether a given date is a weekday or weekend day.

<!-- -->

            class(data2$date)

    ## [1] "Date"

            data2$date<-as.Date(data2$date)
            #class(data2$date)
            #weekdays(data2$date)
            
            data3<-
                    data2%>%
                    mutate(day=weekdays(data2$date))%>%
                    mutate(weekday=ifelse(day=="Saturday" | day=="Sunday", "Weekend", "Weekday"))%>%
                    group_by(interval,weekday)%>%
                    summarise(W_avg_steps=mean(steps))
            data3$weekday<-as.factor(data3$weekday)
            head(data3)

    ## # A tibble: 6 x 3
    ## # Groups: interval [3]
    ##   interval weekday W_avg_steps
    ##   <fct>    <fct>         <dbl>
    ## 1 0        Weekday      2.25  
    ## 2 0        Weekend      0.215 
    ## 3 5        Weekday      0.445 
    ## 4 5        Weekend      0.0425
    ## 5 10       Weekday      0.173 
    ## 6 10       Weekend      0.0165

1.  The panel plot below compares the weekday days vs. the weekend days.

<!-- -->

      ##Plot of the summarised data 
            library(lattice)
            xyplot(W_avg_steps ~ interval | factor(weekday),
                   layout = c(1, 2),
                   xlab="Interval",
                   ylab="Average number of steps per 5-min interval",
                   type="l",
                   lty=1,
                   scales=list(x=list(at=seq(0,300,50), limits=c(0,300))),
                   data=data3
                   )

![](PA1_template_files/figure-markdown_strict/panel_plot-1.png)

From this data there was more activity on the weekend days.
