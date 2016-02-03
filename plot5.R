setwd("C:/Adatok/coursera_edX/4_Exploratory Data analysis/Quizes_Assignments/Assignment2")

unzip("exdata-data-NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)

match <- levels(SCC[,4])[45:52]  #identifying motor vehicle sources
match1<-SCC[SCC$EI.Sector %in% match, c(1,4)]
#table(match1$EI.Sector) # to check that the match subsetted the rigth observations
NEI2<-NEI[NEI$fips %in% "24510",] #subset for Baltimore
NEI3<-merge(match1, NEI2, by.x= "SCC", by.y= "SCC" ) # subsetting NEI based on the match
d4<-ddply(NEI3,.(year),summarise, Emission = sum(Emissions)) # calculating sum of Emission for each year

#Plot5

library(ggplot2)
png("plot5.png", width= 480, height= 480)

g<-ggplot(d4, aes(year, Emission))
g + 
  geom_point(aes(size=4)) + 
  geom_smooth(method= "lm") +
  labs(title= "PM2.5 emission from motor vehicle sources in Baltimore for years 1999-2008") + 
  labs(x= expression("Year"), y= expression("PM2.5 emission [tonnes]")) +
  theme_gray(base_family= "Times") +
  theme(legend.position= "none")

dev.off()
