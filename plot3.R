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
data <- start(FALSE)
par(pch="|")
with(data, plot(Time, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering"))
with(data, lines(Time, Sub_metering_1))
with(data, lines(Time, Sub_metering_2, col="red"))
with(data, lines(Time, Sub_metering_3, col="blue"))

legend("topright", lty = 1, col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png, file="plot3.png")
dev.off()