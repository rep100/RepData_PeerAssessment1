#Include required libraries.
library(dplyr)
library(ggplot2)
library(mice)
library(timeDate)

#Read file
activity_raw <- read.csv("activity.csv")

#Get dataset without missing data.
activity <- na.omit(activity_raw)

#Calculating total steps per day, mean and median
activity_summary <- activity %>% group_by(date) %>% summarize(total_steps=sum(steps), mean_steps=mean(steps), median_steps=median(steps))

#Transform data
activity_summary <- transform(activity_summary, date=strptime(date, format = "%Y-%m-%d"))

#Plot total steps. Since we already have calculated the total we can use a bar plot.
ggplot(data=activity_summary, aes(activity_summary$date, activity_summary$total_steps)) + geom_bar(stat="identity") + ggtitle("Total steps per day") + ylab("Steps") + xlab("Date")

#Create time series
interval_average <- activity %>% group_by(interval) %>% summarize(average_steps=mean(steps))
ggplot(interval_average, aes(interval_average$interval, interval_average$average_steps)) + geom_line() + xlab("Interval") + ylab("Average Steps") + ggtitle("Average steps by interval")

#Get the interval with the higher steps average
max_average_row <- which.max(interval_average$average_steps)
max_average_interval <- interval_average[[max_average_row,1]]

#Calculate the number of missing values.
md.pattern(activity_raw)
imputedValues <- mice(activity_raw, method = "pmm", seed = 500)
activity_imputed <- complete(imputedValues, 1)

#Calculating total steps per day, mean and median for activity with imputed data
activity_imputed_summary <- activity_imputed %>% group_by(date) %>% summarize(total_steps=sum(steps), mean_steps=mean(steps), median_steps=median(steps))

#Transform data
activity_imputed_summary <- transform(activity_imputed_summary, date=strptime(date, format = "%Y-%m-%d"))

#Create graphic
ggplot(data=activity_imputed_summary, aes(activity_imputed_summary$date, activity_imputed_summary$total_steps)) + geom_bar(stat="identity") + ggtitle("Total steps per day (Imputed data)") + ylab("Steps") + xlab("Date")

#Transform data to create time series
activity_imputed_date <- transform(activity_imputed, date=as.POSIXct(strptime(date, format = "%Y-%m-%d")))
activity_imputed_clasif <- mutate(activity_imputed_date, type=ifelse(isWeekday(activity_imputed_date$date, wday=1:5), "WEEK", "WEEKEND"))
interval_average_clas <- activity_imputed_clasif %>% group_by(interval, type) %>% summarize(average_steps=mean(steps))
ggplot(interval_average_clas, aes(interval_average_clas$interval, interval_average_clas$average_steps)) + geom_line() + xlab("Interval") + ylab("Average Steps") + ggtitle("Average steps by interval") + facet_grid(type ~ .)

