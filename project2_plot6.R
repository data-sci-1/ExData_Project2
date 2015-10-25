# Q6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
# sources in Los Angeles County, California. Which city has seen greater changes over
# time in motor vehicle emissions?

## load dplyr lib
library(dplyr)

# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Create summary tables of total emissions for Baltimore and Los Angeles by year
baltimoreNEI <- filter(NEI, fips == "24510")
losAngelesNEI <- filter(NEI, fips == "06037")

# Filter SCC table by "Vehicles"
filteredSCC <- filter(SCC, grepl("Vehicles", SCC$EI.Sector, ignore.case = TRUE))
# Convert SCC factor to list of strings
listOfFips <- as.character(filteredSCC$SCC) 

# Filter NEI tables by SCC being one of "Vehicles" SCCs
filteredBaltimoreNEI <- filter(baltimoreNEI, baltimoreNEI$SCC %in% listOfFips)
filteredLosAngelesNEI <- filter(losAngelesNEI, losAngelesNEI$SCC %in% listOfFips)

# Create summary tables of total emissions by year
by_year_summary_baltimore <- group_by(filteredBaltimoreNEI, year) %>% summarize_each(c("sum"), Emissions)
by_year_summary_los_angeles <- group_by(filteredLosAngelesNEI, year) %>% summarize_each(c("sum"), Emissions)

# set up png device
png(file = "project2_plot6.png", width = 960, height = 480, units="px", type="windows") 
par(cex.axis=.75, cex.lab=.75, cex.main=.9)

# create linear regression lines
by_year_model_baltimore <- lm(Emissions ~ year, by_year_summary_baltimore)
by_year_model_los_angeles <- lm(Emissions ~ year, by_year_summary_los_angeles)

# plot data
par(mfrow = c(1, 2))
plot(Emissions ~ year, by_year_summary_baltimore, xlab = "Year", ylab = "Emissions from Motor Vehicles (tons)", col = "steelblue", pch = 19, main = "Emissions from Motor Vehicles in Baltimore City, MD", xlim=c(1999,2008), ylim=c(0,5000))
abline(by_year_model_baltimore, lwd = 1, col = "steelblue")
plot(Emissions ~ year, by_year_summary_los_angeles, xlab = "Year", ylab = "Emissions from Motor Vehicles (tons)", col = "green", pch = 19, main = "Emissions from Motor Vehicles in Los Angeles, CA", xlim=c(1999,2008), ylim=c(0,5000))
abline(by_year_model_los_angeles, lwd = 1, col = "green")

# close png device
dev.off()
