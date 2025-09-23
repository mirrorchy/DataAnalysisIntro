# For slide p12: lines for deaths over time - Shipman example
# Output: shipman_deaths_by_hour.png

# Example adapted from David Spiegelhalter's "The Art of Statistics"
# Data are contained in [0-2-shipman-times-x.csv](0-2-shipman-times-x.csv), and taken from [Harold Shipman's Clinical Practice 1974-1998: A Clinical Audit Commissioned by the Chief Medical Officer, by Richard Baker, page 40, Figure 5.2](https://webarchive.nationalarchives.gov.uk/20090808160000/http://www.the-shipman-inquiry.org.uk/ge_doclist.asp?ID=5), derived from the cremation forms of Shipman's victims.

setwd("D:/GitHub/DataAnalysisIntro(Public)/")

library(ggplot2)
shipmantimes<-read.csv("Input/Lec2/00-2-shipman-times-x.csv", header=TRUE) # reads data into shipmantimes data frame

attach(shipmantimes)


p <- ggplot(shipmantimes, aes(x=Hour, y)) + ylim(0,15) # constructs initial plot object, p
p <- p + geom_line(aes(y = Comparison, col = "Comparison GPs"), size=1.5) # adds a y-series
p <- p + geom_line(aes(y = Shipman, col = "Shipman"), size=1.5) # adds a y-series
p <- p +  labs(title="Deaths by Hour of Day", subtitle="From Shipman dataset", y="% of Deaths", x="Hour of Day") # Adds title, subtitle
p <- p + scale_colour_brewer(palette = "Set1") # sets the colour palette 
p <- p + theme(legend.position="none")#, legend.box = "horizontal") # removes the legend

p <- p + annotate("text", x = 11, y = 12, label = "Shipman", color = "#E41A1C")
p <- p + annotate("text", x = 4, y = 7, label = "Comparison GP's", color = "#377EB8")

# Save the plot to Output/Lec2/ directory
ggsave("Output/Lec2/shipman_deaths_by_hour.png", plot = p, width = 10, height = 6, dpi = 300)

