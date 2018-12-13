setwd("~/Desktop/mat327 project")
data_set <- read.csv("CrimeCountsDataSet.csv")
data_set$Population <- as.numeric(gsub(",","",data_set$Population))
data_set$yearint <- data_set$Year
data_set$Year <- factor(data_set$Year)
Queens_df <- subset(data_set, County == "Queens")
Qviolent <- Queens_df$Violent.Count
Qpop <- Queens_df$Population
Qyear <- Queens_df$Year
Qyearint <- Queens_df$yearint


#descriptive stats
mean(Qviolent, trim = .1)
sd(Qviolent)
mean <- mean(Qviolent, trim = .1)
sd <- sd(Qviolent)

#plots
boxplot(Queens_df$Violent.Count, main = "Violent Crimes in Queens")
year <- Queens_df$Year
plot(Queens_df$yearint, Qviolent, main = "Queens Crimes from 1990-2017", xlab = "Year", ylab = "Amount of Crimes")

#confidence interval
n <- length(Queens_df$Violent.Count)
error <- qt(0.975,df=n-1)*sd/sqrt(n)
mean-error
mean+error
print(c(mean-error,mean+error))

#hypothesis test
early <- subset(Queens_df, yearint <= 2001)
later <- subset(Queens_df, yearint >= 2002)
t.test(early$Violent.Count, later$Violent.Count)

#regression years/counts regression line
plot(Queens_df$yearint, Qviolent, main = "Queens Crimes from 1990-2017", xlab = "Year", ylab = "Amount of Crimes")
formula_ <- lm(Qviolent~yearint, data = Queens_df)
lm(Qviolent~yearint, data = Queens_df)
abline(coef(formula_))

#t-test for populational information
Qmed <- median(Queens_df$Population)
Qsmall_pop <- subset(Queens_df, Qpop < Qmed)
Qlarge_pop <- subset(Queens_df, Qpop > Qmed)
t.test(Qsmall_pop$Violent.Count, Qlarge_pop$Violent.Count)

#correlational between year, popualtion, counts
cor(Qpop, Qyearint)
cor(Qyearint, Qviolent)
cor(Qpop, Qviolent)

plot(Qyearint, Qpop, main = "Changes In Population", xlab = "Year", ylab = "Population")
plot(Qyearint, Qviolent, main = "Violent Crime Occurrences From 1990-2017", xlab = 'Year', ylab = "Amount of Violent Crimes")
plot(Qpop, Qviolent, main = "Violent Crimes and Population", xlab = "Population", ylab = "Violent Crimes")

