download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip','data1.zip',method = "auto", quiet=FALSE)
unzip("data1.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
Temis <- aggregate(Emissions ~ year,NEI, sum)
png("barplot.png")
barplot(height=(Temis$Emissions)/10^5, names=Temis$year, border="#69b3a2", col="red", xlab="YEAR", ylab="EMISSION (10^5)", main="Total PM2.5 from 1999 to 2008", ylim=c(0,80))
dev.off()
 
baltimore <- NEI[NEI$fips=="24510",]
TBalt <- aggregate(Emissions ~ year, baltimore,sum)
png("barplot1.png")
barplot(height=(TBalt$Emissions)/10^2, names=TBalt$year, border="#69b3a2", col="red", xlab="YEAR", ylab="EMISSION (10^2)", main="Total PM2.5 for Baltimore", ylim = c(0,35))
dev.off()
library(ggplot2)

ggplot(baltimore,aes(factor(year),Emissions,fill=type)) + geom_bar(stat="identity") +
    facet_grid(.~type,scales = "free",space="free") + 
    labs(x="YEAR", y=expression("EMISSION")) + 
    labs(title=expression("PM2.5 Emissions, Baltimore City 1999-2008") +
    theme_dark() + theme(legend.position = "NONE"))

combustion <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
combustion <- (combustion & coal)
comb_SCC <- SCC[coal,]$SCC
comb_NEI <- NEI[NEI$SCC %in% comb_SCC,]

png("Coalcombustion.png")
ggplot(comb_NEI,aes(factor(year),Emissions/10^5)) +
  geom_bar(stat="identity",fill = "white") + theme_dark() + theme(legend.position = "NONE") +
  labs(x="year", y=expression("EMISSION (10^5 Tons)")) + 
  labs(title=expression("Coal Combustion Emissions 1999-2008"))
dev.off()

veh <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
veh_SCC <- SCC[veh,]$SCC
veh_NEI <- NEI[NEI$SCC %in% veh_SCC,]
Bveh_NEI <- veh_NEI[veh_NEI$fips==24510,]
png("vehicle.png")
ggplot(Bveh_NEI,aes(factor(year),Emissions)) +
  geom_bar(stat="identity",fill="white") +
  theme_dark() +  theme(legend.position = "NONE") +
  labs(x="year", y=expression("Emission (10^5)")) + 
  labs(title=expression("Vehicle Emissions in Baltimore"))
dev.off()

Bveh_NEI$city <- "Baltimore"
LAveh_NEI <- veh_NEI[veh_NEI$fips=="06037",]
LAveh_NEI$city <- "LA"
Baltimore_LA_NEI <- rbind(Bveh_NEI,LAveh_NEI)
png("LA_Balt.png")
ggplot(Baltimore_LA_NEI, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(stat="identity",fill = Baltimore_LA_NEI$year) + 
  facet_grid(scales="free", space="free", .~city) + 
  scale_fill_gradient2(low='white', high='red') +
  theme_dark() + theme(legend.position = "NONE") + 
  labs(x="year", y="Emission (10^3)") + 
  labs(title=expression("Vehicle Emissions in Baltimore & LA"))
dev.off()