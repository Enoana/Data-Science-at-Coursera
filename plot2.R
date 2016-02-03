setwd("C:/Adatok/coursera_edX/4_Exploratory Data analysis/Quizes_Assignments/Assignment2")

unzip("exdata-data-NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)

NEI2<-NEI[NEI$fips %in% "24510",]
d2<-ddply(NEI2,.(year),summarise, sum = sum(Emissions))

#Plot2
png("plot2.png", width= 480, height= 480)
par(bg="thistle1", pch=19, mar= c(5.1, 4.1, 4.1, 2.1))
plot(x= d2$year, y= d2$sum, col= "blue", xlab="Year", ylab= "Total emission of PM2.5 [tonnes]", 
     main= "Total emission of PM2.5 in Baltimore City for years 1999-2008")

dev.off()
