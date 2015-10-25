# Q3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these 
# four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions
# from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

# load dplyr and ggplot2 libs
library(dplyr)
library(ggplot2)

# read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Create summary table of total emissions for Baltimore by type and year
filteredNEI <- filter(NEI, fips == "24510")
by_year_and_type <- group_by(filteredNEI, type, year)
by_year_and_type_summary <- summarize_each(by_year_and_type, c("sum"), Emissions)

# set up png device
png(file = "project2_plot3.png", width = 960, height = 480, units="px", type="windows") 
par(cex.axis=.75, cex.lab=.75, cex.main=.9)

# plot data with ggplot2 method qplot (quick plot)
#qplot(year, Emissions, facets = . ~ type, data = by_year_and_type_summary, geom=c("point", "smooth"))

# or use ggplot2 ggplot method
g <- ggplot(by_year_and_type_summary, aes(year, Emissions)) + 
  geom_point(col = "steelblue") + 
  geom_smooth() + facet_grid(. ~ type) + 
  labs(y = "Emissions (tons)", x = "Year", title = "Emissions By Type for Baltimore City, MD")
print(g)

# close png device
dev.off()
