#loading libraries
library(ggplot2)

#reading csv
squat_madcow <- read.csv("C:/Users/Shion/Downloads/squat_madcow.csv")

#creating plots
ggplot(squat_madcow, aes(squat_madcow$Date..YYYYMMDD.,squat_madcow$X)) + geom_line() + xlab("Time") + ylab("Weight (in lbs)")

#subsetting data
squatz <- subset(squat_madcow,select=c(Date..YYYYMMDD.,X))
head(squatz)

#fitting naive time series
fit <- lm(X ~ Date..YYYYMMDD.,data=squatz)
summary(fit)