# checking for missing values
data(sleep, package = 'VIM')
sum(is.na(sleep$Dream)) # there are 12 rows missing dream values
mean(is.na(sleep$Dream)) # About 19% of rows are missing dream values
mean(!complete.cases(sleep)) # about 32% of rows contain a missing value

# Different ways to visualize missing data
library(mice)
data(sleep, package = 'VIM')
md.pattern(sleep) # in the console, each column is a variable. Values are dummies for whether the variable was missing or not
# there are 42 complete cases; 9 cases are missing Dream and NonD, etc. It also makes the visual which is a bit easier to read
marginplot(sleep[c('Gest', 'Dream')],
           pch = c(20),                         # makes a scatterplot; gray boxplots are complete cases, red boxplots are when
           col = c('darkgray', 'red', 'blue'))  # the opposing variable is missing; when gest is missing, dream is higher
matrixplot(sleep) # light colors are low values, dark is higher, red refers to missing values
# this lets you assess the values of other variables when some data is missing to see any trends; personally i don't like it
# but I've been seeing these charts a lot lately

# Exploring missing values with correlations
x <- as.data.frame(abs(is.na(sleep))) # matrix of 1s and 0s
head(x)
y <- x[which(apply(x, 2, sum)> 0)] # returns the columns that have missing values
cor(y) # correlation matrix tells you which variables tend to be missing together
cor(sleep, y, use = 'pairwise.complete.obs') # rows are the sleep variables, columns are the missing value columns
# So, higher body weights are positively correlated with missing values for NonD

# multiple imputation (Filling missing values using repeated simulations)
# this is an improved version of single imputation (imputing using something like mean or median)
# uses Monte Carlo methods to generate multiple datasets where all of the missing values are imputed, where each missing
# value is predicted based on all the other variables and subjected to some random variation
# The regression or intended analysis is carried out on all datasets, and then final results are averaged together
library(mice)
data(sleep, package = 'VIM')
imp <- mice(sleep, seed = 1234)
imp # I don't think this information is THAT important; look at documentation if you need something
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)
























