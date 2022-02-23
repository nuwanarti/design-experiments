# week 3 
# analyzing a simple A/B test with a T-Test

###
## Scenario:   Comparing pages visited in an A/B test
##
## Statistics: Descriptive statistics, independent-samples t-test
###

## Independent-samples t-test

# read in a data file with page views from an A/B test
pgviews = read.csv("pgviews.csv")
View(pgviews)
pgviews$Subject = factor(pgviews$Subject) # convert to nominal factor
pgviews$Site = factor(pgviews$Site) # Rv4 
summary(pgviews)


# descriptive statistics by Site
library(plyr)
ddply(pgviews, ~ Site, function(data) summary(data$Pages)) # split dataframe, apply function and return results in a dataframe
# split by site, summarize by page
ddply(pgviews, ~ Site, summarise, Pages.mean=mean(Pages), Pages.sd=sd(Pages))

# graph histograms and a boxplot
hist(pgviews[pgviews$Site == "A",]$Pages)
# histogram for A is a normal, gaussian distribution, but for B its not a normal distribution, we'll consider this later in the course

hist(pgviews[pgviews$Site == "B",]$Pages)
plot(Pages ~ Site, data=pgviews) # box plot

# independent-samples t-test
# factors can be between subjects or within subjects. between factor is the type of 
#   factor that the site would be. (each visitor eather get A or B)
#   so this is an independent-samples T-Test
# var.equal=TRUE indicates that the variation is equal
# for T-Test its not mandotory to make the variables equal. 
t.test(Pages ~ Site, data=pgviews, var.equal=TRUE)

# t-test gives the value in the t distribution that we are getting from this data
# t = -7.2083, df = 498, p-value = 2.115e-12
# t=t-value, df=degree of freedom, p-value
# p value tells that significant variance in the dataset.
