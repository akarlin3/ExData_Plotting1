library(dplyr)
library(tidyverse)

narrow <- function() {
  data <- read.csv("~/Desktop/hpc.txt", sep = ";")
  date1 <- "1/2/2007"
  date2 <- "2/2/2007"
  data <- with(data, subset(data, Date == date1 | Date == date2))
  dt <- rep(as.Date(0), length(data$Date))
  dateTime <- data.frame(dt)
  dateTime$dt <- as.POSIXlt(0)
  for (i in 1:length(data$Date)) {
    combineI <- paste(
      data$Date[[i]], data$Time[[i]], sep = " ")
    dateTimeI <- strptime(combineI, format = "%d/%m/%Y %H:%M:%S")
    dateTime$dt[[i]] <- dateTimeI
    
  }
  data <- data[, -1]
  data <- data[, -1]
  cbind(dateTime, data)
}

plot1 <- function() {
  data <- narrow()
  png("~/Desktop/working_files/plot1.png")
  gap <- as.numeric(data$Global_active_power)
  par(xaxt = "n")
  hist(gap, col = "red", 
       xlab = "Global Active Power (kilowatts)", 
       ylab = "Frequency", 
       main = "Global Active Power", xlim = c(0, 6),
       ylim = c(0, 1200), breaks = 24)
  par(xaxt = "s")
  axis(1, at = c(0, 2, 4, 6))
  dev.off()
}