setwd("C:/Adatok/coursera_edX/4_Exploratory Data analysis/Quizes_Assignments/Assignment2")

#unzip("exdata-data-NEI_data.zip")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#for (i in 1:4) {
 #   TotalEmission<- data.frame(0)
 #   year<- table(NEI$year)[i]
 #   #TotalEmission<- sum(NEI$year %in% year)
 #   TotalEmission<-sapply(NEI$year %in% year,sum)
#    
#}

#NEI2<-NEI[NEI$year %in% "1999",]
#boxplot(year~Emissions, NEI2)

#plot(x= NEI$year, y=NEI$Emissions)

#To get the total emission of PM2.5
list<- list(a=NEI[NEI$year %in% "1999", 4], b=NEI[NEI$year %in% "2002", 4], c=NEI[NEI$year %in% "2005", 4],
            d=NEI[NEI$year %in% "2008", 4])
sapply(list, sum)

#or

TotalPM<-tapply(NEI$Emissions,NEI$year,sum) # problem: it returns a list
#try to make a data.frame from the list
year<- vector(mode="numeric", length=0)
total<- vector(mode="numeric", length=0)
for (i in 1:4) {
  TotalPM<-append(total, TotalPM[[i]]) #sg went wrong here...
  Year<-append(year, names(TotalPM)[i])
}
data<-cbind(TotalPM, Year)
#good solution
library(plyr)
d<-ddply(NEI,.(year),summarise, sum = sum(Emissions)) #returns a dataframe with the year and the sum(Emissions), Oh yeee

#Plot1
png("plot1.png", width= 480, height= 480)
par(bg="thistle1", pch=19, mar= c(5.1, 4.1, 4.1, 2.1))
plot(x= d$year, y= d$sum, pch= 19, col= "blue", xlab="Year", ylab= "Total PM2.5 [tonnes]", 
     main= "Total emission of PM2.5 for years 1999-2008")

dev.off()

#Question2

NEI2<-NEI[NEI$fips %in% "24510",] #subset for Baltimore
library(plyr)
d2<-ddply(NEI2,.(year),summarise, sum = sum(Emissions))

#Plot2
png("plot2.png", width= 480, height= 480)
par(bg="thistle1", pch=19, mar= c(5.1, 4.1, 4.1, 2.1))
plot(x= d2$year, y= d2$sum, col= "blue", xlab="Year", ylab= "Total PM2.5 [tonnes]", 
     main= "Total emission of PM2.5 in Baltimore City for years 1999-2008")

dev.off()

#Question3

NEI2<-NEI[NEI$fips %in% "24510",] #subset for Baltimore
d3<-ddply(NEI2,.(year, type),summarise, Emission = sum(Emissions)) #total emission by sources and by years

library(ggplot2)
qplot(year, Emission, data=d3, color= type, shape= type)
qplot(year, Emission, data=d3, geom= c("point", "smooth"), 
      facets= .~ type, color= type, shape= type)

g<-ggplot(d3, aes(year, Emission))

g + facet_grid(.~type) + geom_point(aes(color= type), size=4) + geom_smooth(method= "lm")

#Plot3
library(ggplot2)
png("plot3.png", width= 960, height= 480)

g<-ggplot(d3, aes(year, Emission))
g + facet_grid(.~type) + geom_point(aes(color= type), size=4) + geom_smooth(method= "lm") +
  labs(title= "PM2.5 emissions from 1999-2008 for Baltimore City") + labs(x= expression("Year")) +
  theme_gray(base_family= "Times")

dev.off()

#Question4 regarding coal combustionrelated sources

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

#Question5 

match <- levels(SCC[,4])[45:52]  #identifying motor vehicle sources
match1<-SCC[SCC$EI.Sector %in% match,c(1,4)]
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
  labs(title= "PM2.5 emission for motor vehicle sources in Baltimore for years 1999-2008") + 
  labs(x= expression("Year"), y= expression("PM2.5 emission [tonnes]")) +
  theme_gray(base_family= "Times") +
  theme(legend.position= "none")

dev.off()

#Question 6

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