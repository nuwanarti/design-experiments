library(readr)
deviceprefs <- read_csv("deviceprefs.csv")
View(deviceprefs)
deviceprefs$Subject = factor(deviceprefs$Subject)
deviceprefs$Pref = factor(deviceprefs$Pref) # Rv4
summary(deviceprefs)
plot(deviceprefs$Pref)

deviceprefs$Disability = factor(deviceprefs$Disability)
summary(deviceprefs)


# Pearson chi-square test
prfs = xtabs( ~ Pref, data=deviceprefs)
prfs # show counts
chisq.test(prfs)

binom.test(prfs)


# For people without disabilities, perform a binomial test to see whether their preference for touchpads differed significantly from chance. To the nearest ten-thousandth (four digits), what is the p-value? Hint: Run a binomial test comparing the sum of rows of people without disabilities who prefer the touchpad, against the number of all rows of people without disabilities. With two possible preferences, touchpad and trackball, the chance probability would be 1/2. Do not correct for multiple comparisons; consider this a single test on a subset of the data.
# manual post hoc binomial tests for non disabled -- do any prefs for touchpads over trackballs sig. differ from chance for non disables?
nt = binom.test(sum(deviceprefs[deviceprefs$Disability == 0,]$Pref == "touchpad"), nrow(deviceprefs[deviceprefs$Disability == 0,]), p=1/2)
nt

# For people with disabilities, perform a binomial test to see whether their preference for touchpads differed significantly from chance. To the nearest ten-thousandth (four digits), what is the p-value? Hint: Run a binomial test comparing the sum of rows of people with disabilities who prefer the touchpad, against the number of all rows of people with disabilities. With two possible preferences, touchpad and trackball, the chance probability would be 1/2. Do not correct for multiple comparisons; consider this a single test on a subset of the data.
dt = binom.test(sum(deviceprefs[deviceprefs$Disability == 1,]$Pref == 'touchpad'), nrow(deviceprefs[deviceprefs$Disability == 1,]), p=1/2)
dt
p.adjust(c(nt$p.value, dt$p.value), method="holm")
# Conduct a two-sample Chi-Square test of proportions on preferences by disability status. To the nearest hundredth (two digits), what is the Chi-Square statistic?
# Pearson chi-square test


prfs = xtabs( ~ Pref + Disability, data=deviceprefs)
View(prfs)
chisq.test(prfs)

fisher.test(prfs)
