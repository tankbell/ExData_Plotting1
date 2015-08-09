plot4 <- function() {
  ## Read the household power consumption data
  ## Skip the first 66637 rows of data as the
  ## data for Feb 01 2007 and Feb 02 2007 starts
  ## from row 66638. This detail was found by
  ## opening the file in vim and searching for
  ## 1/2/2007. The number of rows that are read
  ## is set to 2880 that includes the data for
  ## 1/2/2007 and 2/2/2007. In the dataset, the
  ## missing values are coded as ? and so na.strings
  ## has been set to ?.
  x <- read.table("./household_power_consumption.txt",
                  header = FALSE,
                  sep = ";",
                  skip = 66637, 
                  nrows = 2880,
                  na.strings = "?",
                  stringsAsFactors = FALSE)
  
  ## Assign column headers
  colnames(x) <- c("Date", 
                   "Time", 
                   "Global_active_power", 
                   "Global_reactive_power", 
                   "Voltage", 
                   "Global_intensity", 
                   "Sub_metering_1", 
                   "Sub_metering_2", 
                   "Sub_metering_3")
  ## Add a new column called "Formatted_Date"
  ## that merges the Date and Time Columns.
  x$Formatted_Date <- strptime(paste(as.Date(x$Date, format="%d/%m/%Y"), x$Time), "%Y-%m-%d %H:%M:%S")
  
  ## Open a PNG plotting device so that the
  ## plot output can be sent to a file
  ## named plot4.png
  png(filename = "plot4.png", width = 480, height = 480, units = "px")
  ## Plot 4 graphs , 2 in each row
  par(mfrow=c(2,2))
  ## Graph.1 Date Time(X-Axis) vs Global Active Power(Y-Axis)
  y <- na.omit(x[,"Global_active_power"])
  plot(x$Formatted_Date, y , type = "l", xlab = "", ylab = "Global Active Power")
  
  ## Graph.2 Date Time(X-Axis) vs Voltage(Y-Axis)
  x$Voltage <- na.omit(x$Voltage)
  plot(x$Formatted_Date, x$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
  
  ## Graph.3 Date Time(X-Axis) vs Energy sub metering(Y-Axis)
  x$Sub_metering_1 <- na.omit(x$Sub_metering_1)
  x$Sub_metering_2 <- na.omit(x$Sub_metering_2)
  x$Sub_metering_3 <- na.omit(x$Sub_metering_3)
  plot(x$Formatted_Date, x$Sub_metering_1, type = 'n',
       xlab = "", ylab = "Energy sub metering")
  points(x$Formatted_Date, x$Sub_metering_1, type = 'l')
  points(x$Formatted_Date, x$Sub_metering_2, type = 'l', col = "red")
  points(x$Formatted_Date, x$Sub_metering_3, type = 'l', col = "blue")
  ## Remove the box around the legend using bty = n 
  legend("topright",
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty = c(1,1,1),
         col = c("black","red","blue"), bty = "n")
  
  ## Graph.4 Date Time(X-Axis) vs Global Reactive Power(Y-Axis)
  y <- na.omit(x$Global_reactive_power)
  plot(x$Formatted_Date, y, type = "l", xlab = "datetime", ylab = "Global Reactive Power")
  dev.off()
}