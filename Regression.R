# do larger populations predict higher counts
setwd("~/Desktop/mat327 project")
data_set <- read.csv("CrimeCountsDataSet.csv")
data_set$Year <- factor(data_set$Year)
data_set$Population <- as.numeric(gsub(",","",data_set$Population))
#has counts increased over years?
# all count catagories are in int form!
# index count = violent count + property rate
# Albany
Albany <- subset(data_set, County == "Albany")
y1990 <- subset(data_set, Year == "1990")
# ^^ seperates all Albany and year 1990 data into smaller frames
VC_1990 <- y1990$Violent.Count
VC_Albany <-Albany$Violent.Count 
#mean violent count of 1990 = 3418
#mean violent count of Albany = 1379
plot(data_set$Population, data_set$Violent.Count, xlab ="Population", ylab = "Violent Counts", main = "Populational Violent Counts")
# ^ x-axis = population,  y-axis = violent counts
plot(data_set$Population, data_set$Property.Count, xlab ="Population", ylab = "Violent Counts", main = "Populational Property Counts")
# ^ x-axis = population,  y-axis =  property counts

# small popualtion v larger population
small_pop <- subset(data_set, Population < 750000)
plot(small_pop$Population, small_pop$Violent.Count)
lm (Violent.Count~Population, data = small_pop)
lm_small_pop_violent <- lm (Violent.Count~Population, data = small_pop)
abline(coef(lm_small_pop_violent))

#large 
large_pop <- subset(data_set, Population > 750000)
plot(large_pop$Population, large_pop$Violent.Count)
lm (Violent.Count~Population, data = large_pop)
lm_large_pop_violent <- lm (Violent.Count~Population, data = large_pop)
abline(coef(lm_large_pop_violent))

#T-TEST:
#idea for t-test. choce a county and year, and 
#see if there count is significant in comparrasion to the rest of the county.
#show number of counts, per county, then over year.
#do larger populations predict higher counts
#are higher counts positviely correlated between county
