timeOnSite=read.csv('timeonsite.csv')
timeOnSite$Subject = factor(timeOnSite$Subject)
timeOnSite$Site = factor(timeOnSite$Site)
View(timeOnSite)

#How many subjects were in this website A/B test?
summary(timeOnSite)

# How many subjects were exposed to each variation of the website A and B?


# To the nearest tenth (one digit), what was the median time on site for site "B"?
library(plyr)
ddply(timeOnSite, ~ Site, function(data) summary(data$Time))

# To the nearest hundredth (two digits), what was the standard deviation of time on site for site "A"?
sd(timeOnSite[timeOnSite$Site == "A",]$Time)

# Question 5
# Conduct an independent-samples t-test on Time by Site. Assume equal variances. To the nearest hundredth (two digits), what is the t-statistic?
t.test(Time ~ Site, data=timeOnSite, var.equal=TRUE)

t.test(Time ~ Site, data=timeOnSite, var.equal=FALSE)
