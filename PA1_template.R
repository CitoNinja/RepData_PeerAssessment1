
## Loading data
data <- read.csv("activity.csv")


## Creating plot - chunk-1
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1050, xlab="Total Number of Steps Per Day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


## Creating plot - chunk-2
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
            FUN=mean, na.rm=TRUE)

ggplot(data=averages, aes(x=interval, y=steps)) +
   geom_line(colour="red") +
   xlab("5-Minute Interval") +
   ylab("Step Average")


averages[which.max(averages$steps),]


## missing data
missing <- is.na(data$steps)
table(missing)


## Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
   filled <- NA
   
      if (!is.na(steps))
         filled <- c(steps)
      
      else
         filled <- (averages[averages$interval==interval, "steps"])
          return(filled)
}

filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)


## Finding the total number of steps per day
## creating plot - chunk-3
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1050, xlab="Total Number of Steps Per Day")
mean(total.steps)
median(total.steps)


## Setting up days of the week
weekday.or.weekend <- function(date) {
   day <- weekdays(date)
   
      if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
            return("Weekday")
   
      else if (day %in% c("Saturday", "Sunday"))
            return("Weekend")
   
      else
            stop("Invalid Date")
}

filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)


## aggregating the number of steps by intervals
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
## Creating plot - chunk-4
ggplot(averages, aes(interval, steps)) + geom_line(colour="blue") + facet_grid(day ~ .) +
   xlab("5-Minute Interval") + ylab("Number of Steps")
