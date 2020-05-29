# common research designs
# one-way ANOVA: y ~ A
# one-way ANCOVA: Y ~ x + A
# Two-way factorial ANOVA: Y ~ A*B
# Two-way factorial ANCOVA: Y ~ x1 + x2 + A*B
# Randomized block: y ~ B + A  (B is a blocking factor)
# one-way within groups ANOVA: y ~ Error(Subject/A)
# Repeated measures ANOVA with 1 within-groups factor (W) and 1 between-groups factor (B): y ~ B*W + Error(Subject/W)

# important ANOVA note for R: a model like y ~ A + B + A:B will assess the following:
# first) The impact of A on y  second) the impact of B on y controlling for A  third) The interaction of A and B controlling for A and B main effects
# when doing design, quantitative covariates should come first, followed by main effects, followed by interactions

# One-way ANOVA
library(multcomp)
attach(cholesterol)
table(trt)
aggregate(response, by = list(trt), FUN = mean)
aggregate(response, by = list(trt), FUN = sd)
fit <- aov(response ~ trt) # F test for group means
summary(fit)
library(gplots)
plotmeans(response ~ trt, xlab = 'Treatment', ylab = 'Response', main = 'Mean Plot\nwith 95% CI') # compare the group means
detach(cholesterol)

# multiple comparisons (See which treatments differ from one another for multiple group/factor levels)
TukeyHSD(fit) # which pairwise combinations are statistically different
par(las = 2)
par(mar = c(5, 8, 4, 2))
plot(TukeyHSD(fit)) # visualize which ones are significant
# or
library(multcomp)
par(mar = c(5, 4, 6, 2))
tuk <- glht(fit, linfct = mcp(trt = 'Tukey'))
plot(cld(tuk, level = 0.05), col = 'lightgrey') # visualize the group distributions, letters on top tell you which are different significantly

# checking assumptions
library(car)
qqPlot(lm(response ~ trt, data = cholesterol), simulate = TRUE, main = 'Q-Q Plot', labels = FALSE) # check normality assumption
bartlett.test(response ~ trt, data = cholesterol) # checking assumption of equal variance in each group
outlierTest(fit) # check for outliers

# one-way ANCOVA
data(litter, package = 'multcomp')
attach(litter)
table(dose)
aggregate(weight, by = list(dose), FUN = mean)
fit <- aov(weight ~ gesttime + dose)
summary(fit) # after controlling for gestation, the dosage levels ARE related to birth weight
detach(litter)

# adjusted group means (After partialing out the effects of the covariates)
library(effects)
effect('dose', fit)

# Significance tests for group means
library(multcomp)
contrast <- rbind('no drug vs. drug' = c(3, -1, -1, -1)) # this is the contrast that we are testing; the first group vs. the average of the other three
summary(glht(fit, linfct = mcp(dose = contrast)))
# we could also just do something similar to one-way AnOVA:
tuk <- glht(fit, linfct = mcp(dose = 'Tukey'))
plot(cld(tuk, level = 0.05), col = 'lightgrey')

# checking assumptions (Need to check normality and variance homogeneity as in one-way ANOVA)
library(multcomp)
fit2 <- aov(weight ~ gesttime*dose, data = litter) # add the interaction term to the model
summary(fit2) # interaction is not significant, assumption of homogeneity of regression slopes holds
library(HH)
ancova(weight ~ gesttime + dose, data = litter) # visualize relationship b/w dependent variable, covariate, and factor
ancova(weight ~ gesttime*dose, data = litter) # here, we add the interaction term and allow the lines to not be parallel
# this is useful to check out the relationships if our assumption of constant regression slopes is violated

# two-way factorial ANOVA
attach(ToothGrowth)
table(supp, dose)
aggregate(len, by = list(supp, dose), FUN = mean)
aggregate(len, by = list(supp, dose), FUN = sd)
dose <- factor(dose) # convert to factor so dose is treated as a grouping variable and not a covariate
fit <- aov(len ~ supp*dose)
summary(fit)
detach(ToothGrowth)

# ways to visualize the ANOVA results
attach(ToothGrowth)
interaction.plot(dose, supp, len, type = 'b', col = c('red', 'blue'), pch = c(16, 18),
                 main = 'Interaction between Dose and Supplement Type')
# OJ is higher for 0.5 and 1 but they are equal for 2; regardless, as dose increases tooth growth increases
# or
library(HH)
interaction2wt(len ~ supp*dose) # this shows you the interaction plot along with the main effects using box plots
detach(ToothGrowth)

# repeated measures ANOVA
CO2$conc <- factor(CO2$conc)
w1bl <- subset(CO2, Treatment == 'chilled')
fit <- aov(uptake ~ conc*Type + Error(Plant/(conc)), w1bl)
summary(fit) # all three are significant
par(las = 2)
par(mar = c(10, 4, 4, 2))
with(w1bl, interaction.plot(conc, Type, uptake, type = 'b', col = c('red', 'blue'), pch = c(16, 18),
                            main = 'Interaction Plot for Plant Type and Concentration'))

# MANOVA
library(MASS)
attach(UScereal)
shelf <- factor(shelf)
y <- cbind(calories, fat, sugars)
aggregate(y, by = list(shelf), FUN = mean)
cov(y)
fit <- manova(y ~ shelf)
summary(fit) # the 3 shelf groups differ on their set of nutritional measures, but we don't know how
summary.aov(fit) # does the univariate tests; looks like the 3 shelf groups differ in all 3 nutritional categories
TukeyHSD(aov(calories ~ shelf)) # we can use Tukey pairwise comparisons for the different y-variables to compare groups
TukeyHSD(aov(fat ~ shelf))
TukeyHSD(aov(sugars ~ shelf))

# checking MANOVA assumptions
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y, center, cov)
coord <- qqplot(qchisq(ppoints(n), df = p),
                d, main = 'Q-Q Plot Assessing Multivariate Normality', ylab = 'Mahalanobis D2')
abline(a = 0, b = 1)
identify(coord$x, coord$y, labels = row.names(UScereal)) # if the points fall on the line, assumption of multivariate normality is upheld
# we might want to delete Wheaties Honey Gold and Wheaties from the data and try the models again
library(mvoutlier)
outliers <- aq.plot(y)
outliers # checks for multivariate outliers

# robust/non-parametric MANOVA, if multivariate normality cannot be upheld
library(rrcov)
Wilks.test(y, shelf, method = 'mcd')

# ANOVA as regression
# we can either use the ANOVA methods as above, or we can do regression with lm() and just use categorical factors
# Instead of treating it as a quantitative value, R will convert factors into a set of dummies, and leave one of them
# out as a default value with which to be compared











