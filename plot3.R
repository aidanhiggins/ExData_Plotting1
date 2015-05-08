load_data <- function(inputFileLocation) {
  # Load the dplyr library
  library(dplyr)
  
  # Loads the full input file
  input.file <- read.table(inputFileLocation, header=TRUE, sep=';')
  
  # Updates the date column based on the expected date format
  input.file$Date <- as.Date(input.file$Date, '%d/%m/%Y')
  
  # Subset the data
  input.data.2007 <- subset(input.file, grepl('2007-02-0(1|2)', Date))
  
  # Merge the Date & time columns and use strptime to create the correct date/time object
  data.withdatetime <- mutate(input.data.2007, datetime = paste(Date, Time))
  
  # Format the new column as a date object using strptime
  data.withdatetime$datetime <- format(data.withdatetime$datetime, format="yyyy-mm-dd hh:mm:ss")
  
  # Update the column data types (converting numerics to characters first due to an issue with
  # direct conversion between factor and numeric)
  data.withdatetime$Global_active_power <- as.character(data.withdatetime$Global_active_power)
  data.withdatetime$Global_active_power <- as.numeric(data.withdatetime$Global_active_power)
  data.withdatetime$datetime <- as.POSIXct(data.withdatetime$datetime)
  
  data.withdatetime$Sub_metering_1 <- as.character(data.withdatetime$Sub_metering_1)
  data.withdatetime$Sub_metering_1 <- as.numeric(data.withdatetime$Sub_metering_1)
  data.withdatetime$Sub_metering_2 <- as.character(data.withdatetime$Sub_metering_2)
  data.withdatetime$Sub_metering_2 <- as.numeric(data.withdatetime$Sub_metering_2)
  data.withdatetime$Sub_metering_3 <- as.character(data.withdatetime$Sub_metering_3)
  data.withdatetime$Sub_metering_3 <- as.numeric(data.withdatetime$Sub_metering_3)
  
  data.withdatetime$Voltage <- as.character(data.withdatetime$Voltage)
  data.withdatetime$Voltage <- as.numeric(data.withdatetime$Voltage)
  data.withdatetime$Global_reactive_power <- as.character(data.withdatetime$Global_reactive_power)
  data.withdatetime$Global_reactive_power <- as.numeric(data.withdatetime$Global_reactive_power)
  
  # Return the subsetted, formatted data
  data.withdatetime
}

draw_plot3 <- function(input.data) {
  png("plot3.png")
  with(input.data,
       plot(datetime, Sub_metering_1,
            col="black",
            xlab="",
            ylab="Energy sub metering",
            type="l"))
  with(input.data,
       lines(datetime, Sub_metering_2,
             col="red"))
  with(input.data,
       lines(datetime, Sub_metering_3,
             col="blue"))
  legend("topright",
         col = c('black', 'red', 'blue'),
         lwd="2",
         legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
  dev.off()
}