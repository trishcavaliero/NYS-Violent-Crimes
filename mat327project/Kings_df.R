setwd("~/Desktop/mat327 project")
data_set <- read.csv("CrimeCountsDataSet.csv")
data_set$Population <- as.numeric(gsub(",","",data_set$Population))
data_set$yearint <- data_set$Year
data_set$Year <- factor(data_set$Year)
Kings_df <- subset(data_set, County == "Kings")
Kviolent <- Kings_df$Violent.Count
Kpop <- Kings_df$Population
Kyear <- Kings_df$Year
Kyearint <- Kings_df$yearint


#descriptive statistics
mean(Kviolent, trim = .1)
sd(Kviolent)
mean <- mean(Kviolent, trim = .1)
sd <- sd(Kviolent)
       
#plots
boxplot(Kviolent, main = "Violent Crimes in Brooklyn")
Kyearint <- Kings_df$yearint
plot(Kings_df$yearint, Kviolent, main = "Brooklyn Crimes from 1990-2017", xlab = "Year", ylab = "Amount of Crimes")

#confidence interval
n <- length(Kings_df$Violent.Count)
error <- qt(0.975,df=n-1)*sd/sqrt(n)
mean-error
mean+error
print(c(mean-error,mean+error))

#hypothesis test
early <- subset(Kings_df, yearint <= 2001)
later <- subset(Kings_df, yearint >= 2002)
t.test(early$Violent.Count, later$Violent.Count)

#regression years/counts regression line
plot(Kings_df$yearint, Kviolent, main = "Brooklyn Crimes from 1990-2017", xlab = "Year", ylab = "Amount of Crimes")
formula_ <- lm(Kviolent~yearint, data = Kings_df)
lm(Kviolent~yearint, data = Kings_df)
abline(coef(formula_))

#t-test for popualtional infromation
Kmed <- median(Kings_df$Population)
Ksmall_pop <- subset(Kings_df, Kpop < Kmed)
Klarge_pop <- subset(Kings_df, Kpop > Kmed)
t.test(Ksmall_pop$Violent.Count, Klarge_pop$Violent.Count)

#correlational between year, popualtion, counts
cor(Kpop, Kyearint)
cor(Kyearint, Kviolent)
cor(Kpop, Kviolent)

plot(Kyearint, Kpop, main = "Changes In Population", xlab = "Year", ylab = "Population")
plot(Kyearint, Kviolent, main = "Violent Crime Occurrences From 1990-2017", xlab = 'Year', ylab = "Amount of Violent Crimes")
plot(Kpop, Kviolent, main = "Violent Crimes and Population", xlab = "Population", ylab = "Violent Crimes")

