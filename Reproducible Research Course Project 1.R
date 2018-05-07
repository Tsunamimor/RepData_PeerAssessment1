## 1. Code for reading in the dataset and/or processing the data
## 2. Histogram of the total number of steps taken each day
## 3. Mean and median number of steps taken each day
## 4. Time series plot of the average number of steps taken
## 5. The 5-minute interval that, on average, contains the maximum number of steps
## 6. Code to describe and show a strategy for imputing missing data
## 7. Histogram of the total number of steps taken each day after missing values are imputed
## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report



## Load packages
        library(data.table)
        library(dplyr)
        getwd()
        setwd("C:/Users/paddy/Documents/Coursera/Data Science/Course Material/5. Reproducable Research/Assignments/Week 2/Reproducible Research Course Project 1")
        dir()
        list.files("./")
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileUrl, destfile = "./repdata%2Fdata%2Factivity.zip")
        dateDownloaded <- date()
        dateDownloaded
        unzip(zipfile = "./repdata%2Fdata%2Factivity.zip")
        list.files("./")
        
        
## Load the test data
        data <- read.csv("./activity.csv")
        ## Load test set
        str(data)
        head(data,10)
        head((complete.cases(data)),10)
        data1 <- tbl_df(data)
             
        daily_steps<- 
                data1%>%
                filter(!is.na(steps)) %>%
                group_by(date) %>%
                summarise(total_steps=sum(steps))

                head(daily_steps)
                daily_steps$date<-as.Date(daily_steps$date)
                
        library(ggplot2)
                library(grDevices)
                library(RColorBrewer)

                
        
        hist(daily_steps$total_steps,breaks = 5,xlab = "Total Daily Steps", main = "Total Steps per Day")
                ## Histogram of the total daily steps during the sample period
                abline(v=median(daily_steps$total_steps),lty=3,lwd=2)
                abline(v=mean(daily_steps$total_steps),lty=1)
                
                ## sample(colors(),10)
                ## cols<- brewer.pal(4,"Set1")        
        ggplot(daily_steps, 
               aes(x=date, y=total_steps)) +
                geom_col(colour="blue",fill="orchid4",width = 0.75) +
                xlab("Date") +
                ylab(expression("total number of steps taken each day")) +
                ggtitle("Steps Measured During Daily Activity Monitoring") +
                scale_x_date(name = waiver(), 
                             breaks = waiver() 
                             #,date_breaks = waiver()
                             #,labels = waiver() 
                             #,date_labels = waiver()
                             #,minor_breaks = waiver()
                             #,date_minor_breaks = waiver()
                             #,limits = NULL
                             #,expand = waiver()
                             #,position = "bottom"
                             ) +
                theme_minimal()
               ## Plot of the total daily steps taken per day
        

        ## Summarise data into average steps per 5-min interval
        step_intervals<- 
                data1%>%
                filter(!is.na(steps)) %>%
                group_by(interval) %>%
                summarise(avg_steps=mean(steps))
        ## max(step_intervals$interval)
        ##Plot of the summarised data 
        ggplot(step_intervals, 
               aes(x=interval, y=avg_steps)) +
                geom_line(colour="orchid4") +
                xlab("Interval") +
                ylim(0,max(step_intervals$avg_steps))+
                ylab("Average number of steps per 5-min interval") +
                ggtitle("Average Steps per 5-min across full monitoring period") +
                theme_minimal()

        ggplot(step_intervals, 
               aes(x=interval, y=avg_steps)) +
                geom_line(colour="blue") +
                labs(title = "Average Steps per 5-min interval", x = "Interval", y = "Avg. Steps per 5-min interval") 
        #xlab("Interval") +
        #ylab(expression("Average number of steps per 5-min interval")) +
        #ggtitle("Average Steps per 5-min across full monitoring period") +
        #+theme_minimal()        
        
        
        
        summary_steps<- 
                data1%>%
                filter(!is.na(steps)) %>%
                group_by(date) %>%
                summarise(mean_steps=mean(steps),total_steps=sum(steps),median_steps=median(steps))
        
        summary(data1$steps)