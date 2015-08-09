plot2 <- function() {
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
  ## named plot2.png
  png(filename = "plot2.png", width = 480, height = 480, units = "px")
  ## Plot the graph ( Date Time(X-Axis) vs Global Active Power in kilowatts(Y-Axis))
  ## after removing any possible NAs
  y <- na.omit(x[,"Global_active_power"])
  plot(x$Formatted_Date, y , type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
  
  dev.off()
}