# Q2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

# load dplyr lib
library(dplyr)

# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Create summary table of total emissions for Baltimore by year
filteredNEI <- filter(NEI, fips == "24510")
by_year_summary <- group_by(filteredNEI, year) %>% summarize_each(c("sum"), Emissions)

# set up png device
png(file = "project2_plot2.png", width = 480, height=480, units="px", type="windows") 
par(cex.axis=.75, cex.lab=.75, cex.main=.9)

# create linear regression line
by_year_model <- lm(Emissions ~ year, by_year_summary)

with(by_year_summary, {
  # plot data
  plot(Emissions ~ year, xlab = "Year", ylab = "Emissions (tons)", col = "steelblue", pch = 19, main = "Total Emissions in Baltimore City, MD")
  abline(by_year_model, lwd = 1, col = "steelblue")
})

# close png device
dev.off()
