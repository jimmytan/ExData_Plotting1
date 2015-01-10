getData <- function(path, startDate, endDate){
    file <- read.table(path, header = TRUE, sep=";", na.strings="?")
    file$Time <- paste(file[,1], file[,2], sep =" ")
    file$Date <- as.Date(file$Date, format="%d/%m/%Y")
    
    subData <- subset(file, file$Date == startDate | file$Date == endDate)
    subData$Time <- strptime(subData$Time, format="%d/%m/%Y %H:%M:%S")
    
    subData
}

start <- function(testMode){
    if(testMode == TRUE){
        path <- "test.txt"
        startDate <- as.Date("2007-07-01")
        endDate <- as.Date("2007-07-02") 
        getData(path, startDate, endDate)
    }else{
        path <- "household_power_consumption.txt"
        startDate <- as.Date("2007-02-01")
        endDate <- as.Date("2007-02-02") 
        getData(path, startDate, endDate)
    }
}
# indicator is for the testing
data <- start(FALSE)
par(pch="|")
par(mfrow = c(2,2))
with(data, {
    plot(Time, Global_active_power, type="n", xlab="", ylab="Global Active Power (kilowatts)")
    lines(Time, Global_active_power)
    plot(Time, Voltage, type="n", xlab="datetime", ylab="Voltage")
    lines(Time, Voltage)
    plot(Time, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
    lines(Time, Sub_metering_1)
    lines(Time, Sub_metering_2, col="red")
    lines(Time, Sub_metering_3, col="blue")
    legend("topright", lty = 1, bty="n", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    plot(Time, Global_reactive_power, type="n", xlab="datatime")
    lines(Time, Global_reactive_power)
})

dev.copy(png, file="plot4.png")
dev.off()
