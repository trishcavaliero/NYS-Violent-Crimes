setwd("~/Desktop/mat327 project")
data_set <- read.csv("CrimeCountsDataSet.csv")
data_set$Population <- as.numeric(gsub(",","",data_set$Population))
data_set$yearint <- data_set$Year
data_set$Year <- factor(data_set$Year)
Bronx_df <- subset(data_set, County == "Bronx")
Bviolent <- Bronx_df$Violent.Count
Bpop <- Bronx_df$Population
Byear <- Bronx_df$Year
Byearint <- Bronx_df$yearint


#descriptive statistics
mean(Bviolent, trim = .1)
sd(Bviolent)
mean <- mean(Bviolent, trim = .1)
sd <- sd(Bviolent)

#plots
boxplot(Bronx_df$Violent.Count, main = "Violent Crimes in the Bronx")
Byearint <- Bronx_df$yearint
plot(Bronx_df$yearint, Bviolent, main = "Bronx Crimes from 1990-2017", xlab = "Year", ylab = "Amount of Crimes")

#confidence interval
n <- length(Bronx_df$Violent.Count)
error <- qt(0.975,df=n-1)*sd/sqrt(n)
mean-error
mean+error
print(c(mean-error,mean+error))

#hypothesis test
early <- subset(Bronx_df, yearint <= 2001)
later <- subset(Bronx_df, yearint >= 2002)
t.test(early$Violent.Count, later$Violent.Count)

#regression years/counts regression line
plot(Bronx_df$yearint, Bviolent,  main = "Bronx Crimes from 1990-2017", xlab = "Year", ylab = "Amount of Crimes")
formula_ <- lm(Bviolent~yearint, data = Bronx_df)
lm(Bviolent~yearint, data = Bronx_df)
abline(coef(formula_))

#t-test for popualtional infromation
Bmed <- median(Bronx_df$Population)
Bsmall_pop <- subset(Bronx_df, Bpop < Bmed)
Blarge_pop <- subset(Bronx_df, Bpop > Bmed)
t.test(Bsmall_pop$Violent.Count, Blarge_pop$Violent.Count)

#correlational between year, popualtion, counts
cor(Bpop, Byearint)
cor(Byearint, Bviolent)
cor(Bpop, Bviolent)

plot(Byearint, Bpop, main = "Changes In Population", xlab = "Year", ylab = "Population")
plot(Byearint, Bviolent, main = "Violent Crime Occurrences From 1990-2017", xlab = 'Year', ylab = "Amount of Violent Crimes")
plot(Bpop, Bviolent, main = "Violent Crimes and Population", xlab = "Population", ylab = "Violent Crimes")

