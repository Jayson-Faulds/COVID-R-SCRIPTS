# we use these methods when: data comes from unknown distributions, sample sizes are small, outliers are a problem
# Permutation test: for a simple t-test, let's say we have one of the problems listed above (for a t-test we assume normal distribution with equal variance between two groups)
# for the permutation test, calculate the t-score as usual as if we were using the parametric approach
# then, take your original data, and randomly jumble the responses, this will be a permutation of the original data
# calculate the new t-score for this. And repeat this process many times until you test all permutations
# now you have a distribution of t-scores. Find out where the original t-score is located in this distribution
# if it is still outside of the middle 95%, then we reject the null that the 2 groups are equal
# if there are too many possible permutations to test, we can do a sample of this using Monte Carlo simulation

# independent two-sample permutation test
library(coin)
score <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65)
treatment <- factor(c(rep('A', 5), rep('B', 5)))
mydata <- data.frame(treatment, score)
mydata
t.test(score ~ treatment, data = mydata, var.equal = TRUE) # significant using parametric
oneway_test(score ~ treatment, data = mydata, distribution = 'exact') # not significant using permutation test (this is more realistic bc only 10 observations)

# independent non-parametric two-sample permutation test 
library(MASS)
UScrime <- transform(UScrime, So = factor(So)) # have to factor categorical variables
wilcox_test(Prob ~ So, data = UScrime, distribution = 'exact')

# independent k-sample permutation test
library(multcomp)
set.seed(1234)
oneway_test(response ~ trt, data = cholesterol, distribution = approximate(nresample = 9999))

# independence in contingency tables permutation test (Chi squared permutation test)
library(coin)
library(vcd)
Arthritis <- transform(Arthritis, Improved = as.factor(as.numeric(Improved))) # ordinal variables need to be converted to nominal
set.seed(1234)
chisq_test(Treatment ~ Improved, data = Arthritis, distribution = approximate(nresample = 9999))

# independence between numeric variables permutation test
states <- as.data.frame(state.x77) # needs to be a dataframe not a matrix
set.seed(1234)
spearman_test(Illiteracy ~ Murder, data = states, distribution = approximate(nresample = 9999)) # variables are not independent

# dependent two-sample permutation test
library(coin)
library(MASS)
wilcoxsign_test(U1 ~ U2, data = UScrime, distribution = 'exact') # the two groups differ

# permutation test for linear regression
library(lmPerm)
set.seed(1234)
fit <- lmp(weight ~ height, data = women, perm = 'Prob') # perm = 'prob' does an approximate test not all of the permutations
summary(fit)

# permutation test for polynomial regression
library(lmPerm)
set.seed(1234)
fit <- lmp(weight ~ height + I(height^2), data = women, perm = 'Prob')
summary(fit)

# permutation test for multiple regression
library(lmPerm)
set.seed(1234)
states <- as.data.frame(state.x77)
fit <- lmp(Murder ~ Population + Illiteracy + Income + Frost, data = states, perm = 'Prob')
summary(fit)

# permutation test for one-way ANOVA
library(lmPerm)
library(multcomp)
set.seed(1234)
fit <- aovp(response ~ trt, data = cholesterol, perm = 'Prob')
anova(fit)

# permutation test for one-way ANCOVA
library(lmPerm)
set.seed(1234)
fit <- aovp(weight ~ gesttime + dose, data = litter, perm = 'Prob')
anova(fit)

# permutation test for two-way ANOVA (balanced factorial design)
library(lmPerm)
set.seed(1234)
fit <- aovp(len ~ supp*dose, data = ToothGrowth, perm = 'Prob')
anova(fit)
# note that lmPerm does not do sequential sum of squares, so if the design were unbalanced set seqs = TRUE

# Bootstrapping: generating an empirical distribution of a test statistic by repeated random sampling with replacement,
# meaning you can generate confidence intervals and test hypotheses without assuming an underlying theoretical distribution
# take a boostrap sample x number of times, each time generating the statistic in question
# arrange those values from lowest to highest, and then select the values representing the 2.5th and 97.5th percentile
# these are the 95% confidence limits

# bootsrapping for one statistic
rsq <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(summary(fit)$r.square)
}
library(boot)
set.seed(1234)
results <- boot(data = mtcars, statistic = rsq, R = 1000, formula = mpg ~ wt + disp)
print(results)
plot(results)
boot.ci(results, type = c('perc', 'bca')) # generates 95% CI using two different methods

# bootstrapping for multiple statistics
bs <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}
library(boot)
set.seed(1234)
results <- boot(data = mtcars, statistic = bs, R = 1000, formula = mpg ~ wt + disp)
print(results)
plot(results)
boot.ci(results, type = 'bca', index = 2) # CI for the second statistic, which here is the weight coefficient from the regression
# index = 1 refers to the intercept in this case









