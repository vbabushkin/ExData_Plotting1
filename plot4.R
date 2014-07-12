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

#fourth plot
v<-df$Voltage
x<-df$DateTime
y<-df$Global_active_power
z<-df$Global_reactive_power
y1<-df$Sub_metering_1
y2<-df$Sub_metering_2
y3<-df$Sub_metering_3
par(mfrow=c(2,2))
#par(mar=c(b,l,t,r)) --adjusts bottom, left, top, and right margins to specified size (measured in lines, default is (5,4,4,2))
par(mar = c(5,4,2,2))
plot(x,y,type="l", xlab="", ylab="Global Active Power")
plot(x,v,type="l", xlab="datetime", ylab="Voltage")
plot(x,y1, xlab="", ylab="Energy sub metering", type="l")
points(x,y2,type="l",col="red")
points(x,y3,type="l",col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),col=c("black","blue","red"),bty = "n",cex=0.7)
plot(x,z,type="l", xlab="datetime", ylab="Global_reactive_power")
dev.copy(png, file="plot4.png",width=480,height=480)
dev.off()