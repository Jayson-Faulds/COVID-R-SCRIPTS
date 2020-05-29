# applying functions to columns
myvars <- c('mpg', 'hp', 'wt')
sapply(mtcars[myvars], mean)

# basic descriptive statistics
summary(mtcars[myvars])
#or
library(psych)
describe(mtcars[myvars])

# descriptive statistics by group
aggregate(mtcars[myvars], by = list(am = mtcars$am), mean)
#or
dstats <- function(x){
  sapply(x, mean)
}
by(mtcars[myvars], mtcars$am, dstats)

# basic one-way table
library(vcd)
mytab <- table(Arthritis$Improved)
mytab

# one-way proportion table
prop.table(mytab)

# basic two-way table
table(Arthritis$Treatment, Arthritis$Improved)
#or
xtabs(~ Treatment + Improved, data = Arthritis)

# getting marginal values for two-way tables
mytab <- xtabs(~ Treatment + Improved, data = Arthritis)
margin.table(mytab, 1)
margin.table(mytab, 2)
margin.table(mytab)

# getting proportions for two-way tables
prop.table(mytab, 1)
prop.table(mytab, 2)
prop.table(mytab)

# adding margins to two-way tables
addmargins(mytab)
addmargins(prop.table(mytab))
addmargins(prop.table(mytab, 1), 2)

# two-way table with counts, chi-square contribution, row total, column total
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

# Multi-dimensional tables
mytab <- xtabs(~ Treatment + Sex + Improved, data = Arthritis)
mytab
ftable(mytab)

# marginal values for multi-dimensional tables
margin.table(mytab, 1)
margin.table(mytab, 2)
margin.table(mytab, 3)
margin.table(mytab, c(1, 3))

# proportions for multi-dimensional tables
ftable(prop.table(mytab, c(1, 2)))
ftable(addmargins(prop.table(mytab, c(1, 2)), 3))

# chi-square test of independence
library(vcd)
mytab <- xtabs(~ Treatment + Improved, data = Arthritis)
chisq.test(mytab) # treatment and improved are not independent

# fisher's exact test of independence
fisher.test(mytab)

# cochran-mantel-haenszel test of conditional independence (Here conditional on sex)
mytab <- xtabs(~ Treatment + Improved + Sex, data = Arthritis)
mantelhaen.test(mytab)

# association measures for two-way tables
library(vcd)
mytab <- xtabs(~ Treatment + Improved, data = Arthritis)
assocstats(mytab)

# basic correlations and variance-covariance matrix
states <- state.x77[, 1:6]
cor(states)
cor(states, method = 'spearman')
cov(states)

# partial correlations (Correlations while controlling for other quantitative variables)
library(ggm)
colnames(states)
pcor(c(1, 5, 2, 3, 6), cov(states)) # first argument is the columns, where the first two are to be correlated, the other 3 are controls. the second argument is var-cov matrix

# Significance tests for correlations
cor.test(states[, 3], states[, 5])
library(psych)
corr.test(states, use = 'complete')

# independent t-test
library(MASS)
t.test(Prob ~ So, data = UScrime)

# dependent t-test (the two groups are not independent)
sapply(UScrime[c('U1', 'U2')], function(x) (c(mean = mean(x), sd = sd(x))))
with(UScrime, t.test(U1, U2, paired = TRUE))

# more than 2 groups? Check out ANOVA

# Comparing two groups when assumptions of t-test or ANOVA do not hold (Nonparametric tests)
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data = UScrime) # also called Mann-Whitney U test

# nonparametric dependent test
sapply(UScrime[c('U1', 'U2')], median)
with(UScrime, wilcox.test(U1, U2, paired = TRUE))

# comparing more than two groups-nonparametric
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data = states) # here, state.region has 4 different factor levels

# what if you want to know which pairwise factor levels were statistically different?
source('http://www.statmethods.net/RiA/wmc.txt')
states <- data.frame(state.region, state.x77)
wmc(Illiteracy ~ state.region, data = states, method = 'holm') # now we can see nonparametric multiple comparisons









