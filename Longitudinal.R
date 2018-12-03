# have counts in the Bronx decreased over time? 
setwd("~/Desktop/mat327 project")
data_set <- read.csv("CrimeCountsDataSet.csv")
Bronx <- subset(data_set, County == "Bronx")
plot(Bronx$Year, Bronx$Violent.Count)
plot(Bronx$Year, Bronx$Property.Count)
plot(Bronx$Year, Bronx$Firearm.Count)
Bronx$Population <- as.integer(Bronx$Population)

bronx_df <- data.frame(pop = c(Bronx$Population),year = c(Bronx$Year), vc = c(Bronx$Violent.Count), pc = c(Bronx$Property.Count))
## shows the violent counts and property counts have decreased over time
## ask professor how to fix the bronx population from factors to integers
## it prints as in the hundreds??






