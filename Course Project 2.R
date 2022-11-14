setwd("c:/Users/Molopyane/Documents/datasciencecoursera/Course 4 Exploratory Data Analysis/Week 4/exdata_data_NEI_data")

source<- readRDS("Source_Classification_Code.rds")
summary<- readRDS("summarySCC_PM25.rds")


Total_Annual_Emissions <- aggregate(Emissions ~ year, summary, sum)
Total_Annual_Emissions

png('plot1.png')
barplot(height=Total_Annual_Emissions$Emissions, names.arg=Total_Annual_Emissions$year, 
        xlab="years", ylab=expression('total PM'[2.5]*' emission'), col = 2,
        main=expression('Total PM'[2.5]*' Emissions at various years'))
dev.off()
?barplot

# Question 2

#Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 

baltimore  <- summary[summary$fips=="24510", ]

Annual_Emissions_Baltimore <- aggregate(Emissions ~ year, baltimore, sum)

png('plot2.png') 
barplot(height=Annual_Emissions_Baltimore$Emissions, names.arg=Annual_Emissions_Baltimore$year, xlab="years", 
        ylab=expression('total PM'[2.5]*' emission'), col = 5,
        main=expression('Total PM'[2.5]*' in the Baltimore City, MD emissions at various years'))
dev.off()


# Questions 3

library(ggplot2)


Total_Emissions_By_Year_And_Type <- aggregate(Emissions ~ year + type, baltimore, sum)
Total_Emissions_By_Year_And_Type


png("plot3.png")
g <- ggplot(Total_Emissions_By_Year_And_Type, aes(year, Emissions, color = type))
g <- g + geom_line() +
  xlab("year") +
  ylab(expression('Total PM'[2.5]*" Emissions")) +
  ggtitle('Total Emissions in Baltimore City')
print(g)
dev.off()


#Question 4
# Extract all EI.Sectors which are associated with coal
coalind<- c(grep("Coal", source$EI.Sector))
coalind
# All rows that have cities with coal production or utilisation services
coalcodes<- source[coalind, 1]
coalcodes

# Units/ facilities that makes use of coal in all cities
coaldata<- summary[which(summary[,"SCC"] %in% coalcodes),]
dim(coaldata)
coaldata

c1999 <- coaldata[which(coaldata[,"year"] == 1999), ]
c2002 <- coaldata[which(coaldata[,"year"] == 2002), ]
c2005 <- coaldata[which(coaldata[,"year"] == 2005), ]
c2008 <- coaldata[which(coaldata[,"year"] == 2008), ]

coalStations <- c(nrow(c1999),  nrow(c2002),  nrow(c2005), 
                   nrow(c2008))


Annual_Coal_Emissions <- aggregate(Emissions ~ year, coaldata, sum)
Annual_Coal_Emissions

png("plot4.png")
barplot(height=Annual_Coal_Emissions$Emissions, names.arg=Annual_Coal_Emissions$year, xlab="years", 
        ylab=expression('total PM'[2.5]*' emission'), col = 12,
 
dev.off() 
        
library(lattice)
xyplot(Emissions~coalStations, table)
       main=expression('Total PM'[2.5]*' in the United States'))
       

table<- cbind(Annual_Coal_Emissions ,as.data.frame(coalStations))
table
# Questions 5

Mobile_Ind<- grep("Mobile", source$EI.Sector)
Mobilecodes<- source[Mobile_Ind, 1]


mobiledata_ba<- baltimore[which(baltimore[,"SCC"] %in% c(Mobilecodes)),]
dim(mobiledata_ba)

aggregated_Annual_Emissions_mobile_ba <- aggregate(Emissions ~ year, mobiledata_ba, sum)
aggregated_Annual_Emissions_mobile_ba



png("plot5.png")
barplot(height=aggregated_Annual_Emissions_mobile_ba$Emissions, names.arg=aggregated_Annual_Emissions_mobile_ba$year, xlab="years", 
        ylab=expression('total PM'[2.5]*' emission'), col = 5,
        main=expression('Total PM'[2.5]*' in the Baltimore City from motor vehicle'))
dev.off()
# Questions 6

# Select all mobile devices
LosAngeles <- summary[summary$fips=="06037", ]
Mobile_Ind<- grep("Mobile", source$EI.Sector)
Mobilecodes<- source[Mobile_Ind, 1]



mobiledata_La<- LosAngeles[which(LosAngeles[,"SCC"] %in% c(Mobilecodes)),]
dim(mobiledata_La)

aggregated_Annual_Emissions_mobile_La <- aggregate(Emissions ~ year, mobiledata_La, sum)
aggregated_Annual_Emissions_mobile_La



png("plot6.png")
barplot(height=aggregated_Annual_Emissions_mobile_La$Emissions, names.arg=aggregated_Annual_Emissions_mobile_La$year, xlab="years", 
        ylab=expression('total PM'[2.5]*' emission'), col = 6,
        main=expression('Total PM'[2.5]*' in the Los Angeles City from Mobile sources'))
dev.off()


# Comparison

png("plot7.png")
par(mfrow = c(1,2))

barplot(height=aggregated_Annual_Emissions_mobile_ba$Emissions, names.arg=aggregated_Annual_Emissions_mobile_ba$year, xlab="Baltimore", 
        ylab=expression('total PM'[2.5]*' emission'), col = 5)

barplot(height=aggregated_Annual_Emissions_mobile_La$Emissions, names.arg=aggregated_Annual_Emissions_mobile_La$year, xlab="Los Angeles", 
        ylab=expression('total PM'[2.5]*' emission'), col = 6)


dev.off()

# Number of Cars
library(dplyr)
bacars<- mobiledata_ba %>%
  count(year)

lacars<- mobiledata_La %>%
  count(year)
lacars

cars<- cbind("year"= bacars[1],"Baltimore"= bacars[2],"Los Angeles"= lacars[2])
cars        
