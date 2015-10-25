# Q1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all
# sources for each of the years 1999, 2002, 2005, and 2008.

# load dplyr lib
library(dplyr)

# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Create summary table of total emissions by year
by_year_summary <- group_by(NEI, year) %>% summarize_each(c("sum"), Emissions)

# set up png device
png(file = "project2_plot1.png", width = 480, height=480, units="px", type="windows") 
par(cex.axis=.75, cex.lab=.75, cex.main=.9)

# create linear regression line
by_year_model <- lm(Emissions ~ year, by_year_summary)

# plot data
with(by_year_summary, {
  # plot data
  plot(Emissions ~ year, xlab = "Year", ylab = "Emissions (tons)", col = "steelblue", pch = 19, main = "Total Emissions in the US")
  abline(by_year_model, lwd = 1, col = "steelblue")
})

# close png device
dev.off()
