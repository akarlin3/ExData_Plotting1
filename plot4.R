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

plot4 <- function() {
  data <- narrow()
  png("~/Desktop/working_files/plot4.png")
  par(mfcol = c(2, 2))
  
  par(xaxt = "n")
  plot(data$dt, data$Global_active_power, type = "l",
       xlab = "", ylab = "Global Active Power (kilowatts)")
  par(xaxt = "s")
  dayLevels <- c("Thu", "Fri", "Sat")
  dayLevels <- factor(wday(data$dt, label = TRUE), dayLevels)
  axis(1, at = c(as.POSIXct(date1), as.POSIXct(date2), as.POSIXct(date3)), labels = levels(dayLevels))
  
  par(xaxt = "n")
  plot(data$dt, subm1, type = "l", xlab = "",
       ylab = "Energy sub metering", col = "black")
  lines(data$dt, subm2, col = "red")
  lines(data$dt, subm3, col = "blue")
  legend(x = "topright", 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty = c(1, 1, 1), col = c("black", "red", "blue"))
  par(xaxt = "s")
  axis(1, at = c(as.POSIXct(date1), as.POSIXct(date2), as.POSIXct(date3)), labels = levels(dayLevels))
  
  par(xaxt = "n", yaxt = "n")
  plot(data$dt, data$Voltage, type = "l",
       xlab = "datetime", ylab = "Voltage")
  par(xaxt = "s", yaxt = "s")
  dayLevels <- c("Thu", "Fri", "Sat")
  dayLevels <- factor(wday(data$dt, label = TRUE), dayLevels)
  axis(1, at = c(as.POSIXct(date1), as.POSIXct(date2), as.POSIXct(date3)), labels = levels(dayLevels))
  axis(2, at = c(234, 236, 238, 240, 242, 244, 246), labels = c(234, "", 238, "", 242, "", 246))
  
  par(xaxt = "n")
  plot(data$dt, data$Global_reactive_power, type = "l",
       xlab = "datetime", ylab = "Global_reactive_power")
  par(xaxt = "s")
  axis(1, at = c(as.POSIXct(date1), as.POSIXct(date2), as.POSIXct(date3)), labels = levels(dayLevels))
  
  dev.off()
}
