setwd("C:/Adatok/coursera_edX/4_Exploratory Data analysis/Quizes_Assignments/Assignment2")

unzip("exdata-data-NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)

match1<-grepl("[Cc][o][a][l]", SCC[,10]) #identifying coal related sources
match<-SCC[match1, c(1,10)]
NEI3<-merge(match, NEI, by.x= "SCC", by.y= "SCC" ) # subsetting NEI based on the match
d4<-ddply(NEI3,.(year),summarise, Emission = sum(Emissions)) # calculating sum of Emission for each year

#Plot4
library(ggplot2)
png("plot4.png", width= 480, height= 480)

g<-ggplot(d4, aes(year, Emission))
g + 
  geom_point(aes(size=4)) + 
  geom_smooth(method= "lm") +
  labs(title= "PM2.5 emission for Coal sources in the USA for years 1999-2008") + 
  labs(x= expression("Year"), y= expression("PM2.5 emission [tonnes]")) +
  theme_gray(base_family= "Times") +
  theme(legend.position= "none")

dev.off()
