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

plot2 <- function() {
  data <- narrow()
  png("~/Desktop/working_files/plot2.png")
  date1 <- "1/2/2007 00:00:00"
  date2 <- "2/2/2007 00:00:00"
  date3 <- "3/2/2007 00:00:00"
  date1 <- strptime(date1, format = "%d/%m/%Y %H:%M:%S")
  date2 <- strptime(date2, format = "%d/%m/%Y %H:%M:%S")
  date3 <- strptime(date3, format = "%d/%m/%Y %H:%M:%S")
  par(xaxt = "n")
  plot(data$dt, data$Global_active_power, type = "l",
       xlab = "", ylab = "Global Active Power (kilowatts)")
  par(xaxt = "s")
  dayLevels <- c("Thu", "Fri", "Sat")
  dayLevels <- factor(wday(data$dt, label = TRUE), dayLevels)
  axis(1, at = c(as.POSIXct(date1), as.POSIXct(date2), as.POSIXct(date3)), labels = levels(dayLevels))
  dev.off()
}
