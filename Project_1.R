# Course Project 1 - Reproducible Research
setwd("C:/Users/Marcelo/Desktop/Coursera/Data-Science-Specialization/RepData_PeerAssessment1/")
# Downloading and  Unziping File
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,"activity.zip", method = "auto")
unzip("activity.zip")
# Loading Data
data <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)

str(data)
summary(data)

library(dplyr)

#For this part of the assignment, you can ignore the missing values in the 
# dataset.

data$date <- as.Date(data$date)

steps_per_day <- data %>%
        group_by(date)%>%
        summarise(total_steps = sum(steps, na.rm = TRUE))
steps_per_day

library(ggplot2)
# Make a histogram of the total number of steps taken each day
ggplot(steps_per_day, aes(x = date, y = total_steps))+
        geom_bar(stat = "identity", col = "black", fill = "gray")+
        xlab("Days")+
        ylab("Total Steps")+
        ggtitle("Total Number of Steps Taken Each Day")

# Calculate and report the mean and median total number of steps taken per day

mean_median_steps <- data %>%
        group_by(date) %>%
        summarise(mean = mean(steps, na.rm = FALSE),
                  median = median(steps, na.rm = FALSE))
mean_median_steps 

# Make a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across
# all days (y-axis)

min_5_int <- data %>%
        group_by(interval)%>%
        summarise(avg = mean(steps, na.rm = TRUE))

ggplot(min_5_int, aes(x = interval, y = avg))+
        geom_line()+
        xlab("5-minute interval")+
        ylab("Avg Steps")

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?        

min_5_int$interval[which.max(min_5_int$avg)]

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
sum(is.na(data))
     
# Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could 
# use the mean/median for that day, or the mean for that 5-minute interval, 
# etc.

data_copy <- data #keeping original dataset
NAs <- is.na(data_copy$steps)
avg_int <- tapply(data_copy$steps, data_copy$interval, mean, 
                  na.rm = TRUE, simplify = TRUE)
data_copy$steps[NAs] <- avg_int[as.character(data_copy$interval[NAs])]

# Make a histogram of the total number of steps taken each day and 
# Calculate and report the mean and median total number of steps taken 
# per day. Do these values differ from the estimates from the first part
# of the assignment? What is the impact of imputing missing data on the 
# estimates of the total daily number of steps?
steps_per_day_2 <- data_copy %>%
        group_by(date)%>%
        summarise(total_steps = sum(steps, na.rm = TRUE))
steps_per_day_2

# Make a histogram of the total number of steps taken each day
ggplot(steps_per_day_2, aes(x = date, y = total_steps))+
        geom_bar(stat = "identity", col = "black", fill = "gray")+
        xlab("Days")+
        ylab("Total Steps")+
        ggtitle("Total Number of Steps Taken Each Day")

# Calculate and report the mean and median total number of steps taken per day

mean_median_steps_2 <- data_copy %>%
        group_by(date) %>%
        summarise(mean = mean(steps, na.rm = FALSE),
                  median = median(steps, na.rm = FALSE))
mean_median_steps_2 

summary(steps_per_day)
summary(steps_per_day_2)
# Using the approach of subs NAs by its interval mean, the Mean increased by 15%
# and and the 1st Quantile by 50%.

# Creating new factor variable using ifelse and weekdays() 
data_copy <- data_copy%>%
        mutate(wdays = ifelse(weekdays(data_copy$date) == "sábado" |
                                      weekdays(data_copy$date) == "domingo",
                              "Weekends", "Weekday"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). The plot should look
# something like the following, which was created using simulated data:

steps_per_weekdays <- data_copy %>%
        group_by(wdays, interval)%>%
        summarise(avg_steps = mean(steps, na.rm = TRUE))
steps_per_weekdays

ggplot(steps_per_weekdays, aes(x = interval, y = avg_steps))+
        geom_line()+
        xlab("5-minute interval")+
        ylab("Average # Steps")+
        facet_grid(wdays~.)
