# GLM families and their link functions:
# binomial -- link = 'logit'
# gaussian -- link = 'identity'
# gamma -- link = 'inverse'
# inverse.gauusian -- link = '2/mu^2'
# poisson -- link = 'log'
# quasi -- link = 'identity', variance = 'constant'
# quasibinomial -- link = 'logit'
# quasipoisson -- link = 'log'
# a note on model diagnostics: try to use the same diagnostics as for OLS except for the normally-distributed errors assumption
# when plotting predicted values against residuals, you'll want to adjust the scale of the predicted values:
# plot(predict(model, type = 'response'), residuals(model, type = 'deviance'))

# Basic logistic regression
data(Affairs, package = 'AER')
summary(Affairs)
table(Affairs$affairs)
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, levels = c(0, 1), labels = c('No', 'Yes'))
table(Affairs$ynaffair)
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,
                family = binomial(), data = Affairs)
summary(fit.full)

# comparing full vs. reduced models for logistic regression
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, data = Affairs, family = binomial())
summary(fit.reduced)
anova(fit.reduced, fit.full, test = 'Chisq') # high value means the full model is not significantly better than the simpler model

# interpreting coefficients
coef(fit.reduced)
exp(coef(fit.reduced)) # make sure you exponentiate so that the coefficients are more interpretable
# here, an increase in age by one year leads to a 0.965 multiplicative change in the ODDS of an affair

# Assessing impact of the predictors (Odds are confusing too)
testdata <- data.frame(rating = c(1, 2, 3, 4, 5), age = mean(Affairs$age),
                       yearsmarried = mean(Affairs$yearsmarried), religiousness = mean(Affairs$religiousness))
testdata # we are going to see the effect of changing the rating, holding the other variables constant
testdata$prob <- predict(fit.reduced, newdata = testdata, type = 'response') # response converts the predictions to probabilities
testdata # Now we can see the effect of marriage ratings on class probability estimates

# Dealing with overdispersion for logistic regression
# overdispersion is when the observed variance of the data is bigger than what we would expect under the binomial and poisson distributions
# we know there is overdispersion when Residual Deviance / Residual df is noticeably larger than 1
deviance(fit.reduced)/df.residual(fit.reduced) # this is only 1.03 so there is no overdispersion here
fit <- glm(ynaffiar ~ age + yearsmarried + religiousness + rating, family = quasibinomial(),
           data = Affairs) # use the quasibinomial family if there is overdispersion

# Basic poisson regression
data(breslow.dat, package = 'robust')
fit <- glm(sumY ~ Base + Age + Trt, data = breslow.dat, family = poisson())
summary(fit)

# interpreting parameters
exp(coef(fit)) # an increase in age of one year is associated with a 1.02 multiplicative effect on the response variable

# overdispersion for poisson regression
deviance(fit)/df.residual(fit) # it is the same test for logistic; this time we clearly have overdispersion
fit.od <- glm(sumY ~ Base + Age + Trt, data = breslow.dat, family = quasipoisson())
summary(fit.od) # the parameter estimates are the same, only difference is that the standard error is bigger than before

# Poisson regression for varying time periods (if the time period is not fixed, then we would rather model the RATE than the count)
# the way this looks is like this: log(lambda/time) = beta0 + sum(betaj * xj)
fit <- glm(sumY ~ Base + Age + Trt, data = breslow.dat, family = poisson(), offset = log(time))

# to account for the varying time intervals for each observation, we add an offset parameter based on the time variable in the data














