setwd("~/Desktop/mat327 project")
data_set <- read.csv("CrimeCountsDataSet.csv")
mean_violentcount <- mean(data_set$Violent.Count)
sd_violentcount <- sd(data_set$Violent.Count)
mean_propertycount <- mean(data_set$Property.Count)
sd_propertycount <- sd(data_set$Property.Count)
mean_firearmcount <- mean(data_set$Firearm.Count)
sd_firearmcount <- sd(data_set$Firearm.Count)

