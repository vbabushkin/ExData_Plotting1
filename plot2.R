startDate<-as.Date("2007-02-01")
endDate<-as.Date("2007-02-02")

con <- file("household_power_consumption.txt", "rt")

header<-strsplit(readLines(con, n=1),";",fixed=TRUE)
 
#define an empty data frame
df <-data.frame(DateTime=numeric(),
				Global_active_power=numeric(),
				Global_reactive_power=numeric(),
				Voltage=numeric(),
				Global_intensity=numeric(),
				Sub_metering_1=numeric(),
				Sub_metering_2=numeric(),
				Sub_metering_3=numeric()
				)

while (length(input <- readLines(con, n=1)) > 0){  
	    output = strsplit(input,";", fixed = TRUE)
		date=output[[1]][1]
		time=output[[1]][2]
		x <- paste(date, time)
		dateTime=strptime(x, '%d/%m/%Y %H:%M:%S')
		currentDate=as.Date(dateTime)
		if(currentDate>=startDate & currentDate<=endDate){
			newRow <- data.frame(
				DateTime=dateTime,
				Global_active_power=as.numeric(output[[1]][3]),
				Global_reactive_power=as.numeric(output[[1]][4]),
				Voltage=as.numeric(output[[1]][5]),
				Global_intensity=as.numeric(output[[1]][6]),
				Sub_metering_1=as.numeric(output[[1]][7]),
				Sub_metering_2=as.numeric(output[[1]][8]),
				Sub_metering_3=as.numeric(output[[1]][9]))
				
			df<-rbind(df, newRow)
		}   
} 

#second plot
y<-df$Global_active_power
x<-df$DateTime
plot(x,y,type="l", xlab="", ylab="Global active power (kilowatts)")
dev.copy(png, file="plot2.png",width=480,height=480)
dev.off()