# quick note on power analysis:
# 4 dimensions: sample size, significance level (alpha, or P(Type 1 error)), Power (1 - P(Type 2 error)), Effect Size
# given any of the 3, you can get the fourth using the pwr package
# but the effect size is calculated differently for different types of statistical tests

# power analysis for t-tests
library(pwr)
pwr.t.test(d = 0.8, sig.level = 0.05, power = 0.9, type = 'two.sample', alternative = 'two.sided')
# d = effect size = (mu1 - mu2)/sigma
# you know from past research that standard deviation = 1.25, and you consider 1 as an important difference
# d = 1/1.25 = 0.8
# you want to be 90% sure to detect a difference if it exists (power) with a 95% confidence of not committing a Type 1 error
# this test will tell you how big your sample size needs to be in each group
pwr.t.test(n = 20, d = 0.5, sig.level = 0.01, type = 'two.sample', alternative = 'two.sided')
# Here, you want to have 20 people per group, and detect a 0.625 difference (0.625/1.25 = 0.5) with a 99% confidence of no type 1 error
# This tells you that you have a 14% chance of detecting an effect difference of 0.625 or less
# in other words, there's an 86% chance you'll miss the effect so you should probably NOT do this experiment

# power analysis for t-tests with unequal group sizes
pwr.t2n.test(n1 = , n2 = , d = , sig.level = , power = , alternative = )

# Power Analysis for one-way ANOVA (balanced)
pwr.anova.test(k = 5, f = 0.25, sig.level = 0.05, power = 0.8)
# here, k is the number of groups, and f is the effect size (f = sqrt((sum(ni(mui - mu)^2/N))/sigma^2))
# the code is easy, you just need to know f. To know f, you need to know or estimate the variance along with the 5 group means

# power analysis for correlation tests
pwr.r.test(r = 0.25, sig.level = 0.05, power = 0.9, alternative = 'greater')
# here the effect size is easy, its just the correlation coefficient
# you are finding the number of people necessary to be 90% confident in detecting the effect if the effect exists

# power analysis for linear models
pwr.f2.test(u = 3, f2 = 0.0769, sig.level = 0.05, power = 0.9)
# I'm not writing everything for this its a lot

# power analysis for proportion testing
pwr.2p.test(h = ES.h(0.65, 0.6), sig.level = 0.05, power = 0.9, alternative = 'greater')
# here, the ES.h() function will calculate the effect size for us. We just pass in the two proportions to be tested

# power analysis for chi-squared tests
prob <- matrix(c(0.42, 0.28, 0.03, 0.07, 0.1, 0.1), byrow = TRUE, nrow = 3)
ES.w2(prob)
pwr.chisq.test(w = ES.w2(prob), df = 2, sig.level = 0.05, power = 0.9)
# here, the ES.w2() function will calculate the effect size for us; we just need to pass in the proportional contingency table
# in chi-squared tests, we are trying to identify relationships between two categorical variables

# What if we have no idea what the effect size is? (Like if there is no prior data to estimate parameters like variance)
method <- c('t-test', 'ANOVA', 'linear models', 'proportion test', 'chi-square')
small <- c(0.2, 0.1, 0.02, 0.2, 0.1)
medium <- c(0.5, 0.25, 0.15, 0.5, 0.3)
large <- c(0.8, 0.4, 0.35, 0.8, 0.5)
benchmarks <- data.frame(method, small, medium, large)
benchmarks
# this table will show you some sample effect sizes that are generally considered small/medium/large for their respective tests
# or
es <- seq(0.1, 0.5, 0.01)
nes <- length(es)
samsize <- NULL
for (i in 1:nes){
  results <- pwr.anova.test(k = 5, f = es[i], sig.level = 0.05, power = 0.9)
  samsize[i] <- ceiling(results$n)
}
plot(samsize, es, type = 'l', col = 'red', ylab = 'Effect Size', xlab = 'Sample size (per cell)',
     main = 'One Way ANOVA with Power= 0.9 and Alpha=.05')
# looking at this chart can help you decide your sample size

# sample-size curves for detecting correlations of various sizes (same as above but more pronounced)
r <- seq(0.1, 0.5, 0.01)
nr <- length(r)
p <- seq(0.4, 0.9, 0.1)
np <- length(p)
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(n = NULL, r = r[j], sig.level = 0.05, power = p[i], alternative = 'two.sided')
    samsize[j, i] <- ceiling(result$n)
  }
}
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type = 'n', xlab = 'Correlation Coefficient (r)', ylab = 'Sample Size (n)')
for (i in 1:np){
  lines(r, samsize[, i], type = 'l', lwd = 2, col = colors[i])
}
abline(v = 0, h = seq(0, yrange[2], 50), lty = 2, col = 'grey89')
abline(h = 0, v = seq(xrange[1], xrange[2], 0.02), lty = 2, col = 'grey89')
title('Sample Size Estimation for Correlation Studies\nSig=0.05 (Two-tailed)')
legend('topright', title = 'power', as.character(p), fill = colors)





