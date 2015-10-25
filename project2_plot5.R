# Q5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# load dplyr lib
library(dplyr)

# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Create summary table of total emissions for Baltimore by year
baltimoreNEI <- filter(NEI, fips == "24510")

# Filter SCC table by "Vehicles"
filteredSCC <- filter(SCC, grepl("Vehicles", SCC$EI.Sector, ignore.case = TRUE))
# Convert SCC factor to list of strings
listOfFips <- as.character(filteredSCC$SCC) 
# Filter NEI table by SCC being one of "Vehicles" SCCs
filteredNEI <- filter(baltimoreNEI, baltimoreNEI$SCC %in% listOfFips)

# Create summary table of total emissions by year
by_year_summary <- group_by(filteredNEI, year) %>% summarize_each(c("sum"), Emissions)

# set up png device
png(file = "project2_plot5.png", width = 480, height = 480, units="px", type="windows") 
par(cex.axis=.75, cex.lab=.75, cex.main=.9)

# create linear regression line
by_year_model <- lm(Emissions ~ year, by_year_summary)

with(by_year_summary, {
  # plot data
  plot(Emissions ~ year, xlab = "Year", ylab = "Emissions from Motor Vehicles (tons)", col = "steelblue", pch = 19, main = "Emissions from Motor Vehicles in Baltimore City, MD")
  abline(by_year_model, lwd = 1, col = "steelblue")
})

# close png device
dev.off()
