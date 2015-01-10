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
with(data, plot(Time, Global_active_power, type="n", xlab="", ylab="Global Active Power (kilowatts)"))
with(data, lines(Time, Global_active_power))

dev.copy(png, file="plot2.png")
dev.off()