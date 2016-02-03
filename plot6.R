setwd("C:/Adatok/coursera_edX/4_Exploratory Data analysis/Quizes_Assignments/Assignment2")

unzip("exdata-data-NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)
library(dplyr)

match <- levels(SCC[,4])[45:52]  #identifying motor vehicle sources
match1<-SCC[SCC$EI.Sector %in% match, c(1,4)]
#table(match1$EI.Sector) # to check that the match subsetted the rigth observations

BLA<-NEI[NEI$fips %in% c("24510", "06037"),] #subset for Baltimore & LA
B<-merge(match1, BLA, by.x= "SCC", by.y= "SCC" ) # subsetting BLA based on the match
B<-mutate(B, city= factor(fips == "06037", labels= c("Baltimore", "Los Angeles"))) #creating new factor variable-> city
b<-ddply(B,.(year, city),summarise, Emission = sum(Emissions)) # calculating sum of Emission for each year

#Plot6
library(ggplot2)
png("plot6.png", width= 960, height= 480)

g<-ggplot(b, aes(year, Emission))
g + 
  facet_grid(.~city) + 
  geom_point(aes(color= city), size=4) + 
  geom_smooth(method= "lm") +
  labs(title= "Comparing PM2.5 emissions for Baltimore and Los Angeles from motor vehicle sources") + 
  labs(x= expression("Year"), y=expression("Total emission of PM2.5 [tonnes]")) +
  theme_gray(base_family= "Times")

dev.off()