# Q4: Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

# load dplyr lib
library(dplyr)

# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Filter SCC table by "coal"
filteredSCC <- filter(SCC, grepl("coal", SCC$EI.Sector, ignore.case = TRUE))
# Convert SCC factor to list of strings
listOfFips <- as.character(filteredSCC$SCC) 
# Filter NEI table by SCC being one of "coal" SCCs
filteredNEI <- filter(NEI, NEI$SCC %in% listOfFips)

# Create summary table of total emissions by year
by_year <- group_by(filteredNEI, year)
by_year_summary <- summarize_each(by_year, c("sum"), Emissions)

# set up png device
png(file = "project2_plot4.png", width = 480, height = 480, units="px", type="windows") 
par(cex.axis=.75, cex.lab=.75, cex.main=.9)

# create linear regression line
by_year_model <- lm(Emissions ~ year, by_year_summary)

with(by_year_summary, {
  # plot data
  plot(Emissions ~ year, xlab = "Year", ylab = "Emissions from Coal (tons)", col = "steelblue", pch = 19, main = "Emissions From Coal in the US")
  abline(by_year_model, lwd = 1, col = "steelblue")
})

# close png device
dev.off()
