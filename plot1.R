setwd("C:/Adatok/coursera_edX/4_Exploratory Data analysis/Quizes_Assignments/Assignment2")

unzip("exdata-data-NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)

d<-ddply(NEI,.(year),summarise, sum = sum(Emissions)) #returns a dataframe with the year and the sum(Emissions)

#Plot1
png("plot1.png", width= 480, height= 480)
par(bg="thistle1", pch=19, mar= c(5.1, 4.1, 4.1, 2.1))
plot(x= d$year, y= d$sum, col= "blue", xlab="Year", ylab= "Total emission of PM2.5 [tonnes]", 
     main= "Total emission of PM2.5 from all sources in the USA")

dev.off()

