setwd("C:/Adatok/coursera_edX/4_Exploratory Data analysis/Quizes_Assignments/Assignment2")

unzip("exdata-data-NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)

NEI2<-NEI[NEI$fips %in% "24510",] #subset for Baltimore
d3<-ddply(NEI2,.(year, type),summarise, Emission = sum(Emissions)) #total emission by sources and by years

#Plot3
library(ggplot2)
png("plot3.png", width= 960, height= 480)

g<-ggplot(d3, aes(year, Emission))
  g + 
  facet_grid(.~type) + 
  geom_point(aes(color= type), size=4) + 
  geom_smooth(method= "lm") +
  labs(title= "PM2.5 emissions from 1999-2008 for Baltimore City") + 
  labs(x= expression("Year"), y=expression("Total emission of PM2.5 [tonnes]")) +
  theme_gray(base_family= "Times")

dev.off()