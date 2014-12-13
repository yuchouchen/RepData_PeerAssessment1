require(dplyr)
require(plyr)
require(ggplot2)

setwd("C:\\Users\\YUCHOUCHEN\\Documents\\R programming\\RepData_PeerAssessment1")

# 1. Loading and preprocessing the data
# 1.1 Load the data
inFile <- "./activity.csv"
data <- read.csv(inFile, sep = ",", header = TRUE, stringsAsFactors = FALSE)

# 1.2 Process/transform the data (if necessary) into a format suitable for your 
# analysis
data$date <- as.POSIXct(data$date)

# 2. What is mean total number of steps taken per day?
# 2.2 Calculate and report the mean and median total number of steps taken per 
# day

# Calculate the mean and median total number of steps taken per day
sum<-ddply(data, .(date), summarize, total = sum(steps), 
           mean = mean(steps, na.rm = TRUE), 
           median = median(steps, na.rm = TRUE))
# Report the mean and median total number of steps taken per day
select(sum, date, mean, median)

# 2.1 Make a histogram of the total number of steps taken each day
hist(sum$total, 
     main = "Histogram of total number of steps \ntaken each day", 
     xlab = "Total number of steps taken each day")

# 3. What is the average daily activity pattern?
# 3.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
newIData<-ddply(data, .(interval), summarize, aver = mean(steps, na.rm = TRUE))
ggplot(newIData, aes(interval, aver)) + geom_line() +
      ylab("Average number of steps taken \naveraged across all days")

# 3.2 Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
newIData$interval[newIData$aver == max(newIData$aver)]

# 4. Imputing missing values
# 4.1 Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
sum(is.na(data$steps))

# 4.2 Devise a strategy for filling in all of the missing values in the dataset.
# Calculate the mean of all steps data
allMean <- mean(data$steps, na.rm = TRUE)

# 4.3 Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.

# Declare a new dataset: newData
newData <- data
# Assign the mean to the missing data
newData$steps[is.na(data$steps)] <- allMean

# 4.4 Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? What 
# is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

# Calculate the mean and median total number of steps taken per day
newSum<-ddply(newData, .(date), summarize, total = sum(steps), 
           mean = mean(steps, na.rm = TRUE), 
           median = median(steps, na.rm = TRUE))
# Report the mean and median total number of steps taken per day
select(newSum, mean, median)

hist(newSum$total,
     main = "Histogram of total number of steps \ntaken each day", 
     xlab = "Total number of steps taken each day")

# 5. Are there differences in activity patterns between weekdays and weekends?
# 5.1 Create a new factor variable in the dataset with two levels - "weekday" 
# and "weekend" indicating whether a given date is a weekday or weekend day.

# Create a new column WD for marking "weekday" or "weekend"
newData <- mutate(newData, WD = " ")

# Assign "weekday" or "weekend" to WD column based on date column
newData$WD[weekdays(newData$date) == "Saturday" | 
                 weekdays(newData$date) == "Sunday"] <- "weekend"
newData$WD[newData$WD != "weekend"] <- "weekday"

# Transform class of WD from character to factor
newData$WD <- factor(newData$WD, levels=c("weekday", "weekend"))

# 5.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). 
newTData<-ddply(newData, .(interval, WD), summarize, 
                mean = mean(steps, na.rm = TRUE))

ggplot(newTData, aes(interval, mean)) + geom_line() + facet_grid(WD ~ .) +
      ylab("Number of steps")
