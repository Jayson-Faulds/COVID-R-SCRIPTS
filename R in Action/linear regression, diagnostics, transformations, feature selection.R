# linear regression operators
lm(y ~ x + z)
lm(y ~ x + z + x:z) # interaction
lm(y ~ x*z*w) # same as lm(y ~ x + z + w + x:z + x:w + z:w + x:z:w)
lm(y ~ (x + z + w)^2) # same as lm(y ~ x + z + w + x:z + x:w + z:w)
lm(y ~ .) # y based on every variable in the data
lm(y ~ x -1) # gets rid of the intercept term
lm(y ~ x + I(z^2)) # allows for a transformation to the predictor

# basic simple linear regression example
fit <- lm(weight ~ height, data = women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height, women$weight, xlab = 'height in inches', ylab = 'weight in pounds')
abline(fit)

# polynomiql regression example
fit2 <- lm(weight ~ height + I(height^2), data = women)
summary(fit2)
plot(women$height, women$weight, xlab = 'height in inches', ylab = 'weight in pounds')
lines(women$height, fitted(fit2))

# scatterplot matrix
states <- as.data.frame(state.x77[, c('Murder', 'Population', 'Illiteracy', 'Income', 'Frost')])
cor(states)
library(car)
scatterplotMatrix(states, spread = FALSE, smoother.args = list(lty = 2), main = 'Scatter Plot Matrix', col = 'black')

# multiple regression example
states <- as.data.frame(state.x77[, c('Murder', 'Population', 'Illiteracy', 'Income', 'Frost')])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
summary(fit)

# Visualizing interaction effect
fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)
library(effects)
plot(effect('hp:wt', fit,, list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)

# basic regression diagnostics
fit <- lm(weight ~ height, data = women)
par(mfrow = c(2, 2))
plot(fit)
# we see evidence of a curved relationship between residuals and fitted, meaning a quadratic term might help
fit2 <- lm(weight ~ height + I(height^2), data = women)
par(mfrow = c(2, 2))
plot(fit2) # these diagnostics look a little better for linear relationship (top left) and normality (top right)

# checking normality assumption 
library(car)
states <- as.data.frame(state.x77[, c('Murder', 'Population', 'Illiteracy', 'Income', 'Frost')])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
qqPlot(fit, labels = row.names(states), id.method = 'identify', simulate = TRUE, main = 'Q-Q Plot')
# also
residplot <- function(fit, nbreaks = 10){
  z <- rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE, xlab = 'Studentized Residuals', main = 'Distribution of Errors')
  rug(jitter(z), col = 'brown')
  curve(dnorm(x, mean = mean(z), sd = sd(z)), add = TRUE, col = 'blue', lwd = 2)
  lines(density(z)$x, density(z)$y, col = 'red', lwd = 2, lty = 2)
  legend('topright', legend = c('Normal Curve', 'Kernel Density CUrve'), lty = 1:2, col = c('blue', 'red'), cex = 0.7)
}
residplot(fit)

# checking independence of errors assumption
library(car)
durbinWatsonTest(fit)

# checking linearity assumption
library(car)
crPlots(fit) # loess line should adhere to the straight line

# checking homoscedasticity assumption
library(car)
ncvTest(fit)
spreadLevelPlot(fit) # recommends a power transformation (if needed). line of best fit should be horizontal

# global OLS test (tests for all of the above at once under the Global Stat row)
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

# checking for multicollinearity
library(car)
vif(fit)

# test that checks for outliers (only returns one outlier at a time)
library(car)
outlierTest(fit)

# identifying high leverage points (weird combinations of the predictors)
hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = 'Index Plot of Hat Values')
  abline(h = c(2, 3)*p/n, col = 'red', lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit) # points above the dotted lines are high leverage, you can click on them to reveal the names

# identifying influential observations (have disproportionate effect on the model parameter coefficients)
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2) # can also use 1 as a cutoff, a little more lenient
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = 'red')

# Let's you see HOW the influential observations are impacting the model parameters
library(car)
avPlots(fit, ask = FALSE, id.method = 'identify')

# nice plot to see outliers, high leverage, and influential all at once
library(car)
influencePlot(fit, id.method = 'identify', main = 'Influence Plot',
              sub = 'Circle size is proportional to Cook\'s distance')
# above and below 2/-2 means the point is an outlier
# to the right of 0.2 and 0.3 are high leverage points
# large circles are highly influential points

# Box-Cox transformation for response variable
library(car)
summary(powerTransform(states$Murder)) # ^0.6 is the recommended transformation, but p-value for ^1 is 0.145, don't really need to transform

# Box-Tidwell transformations for predictors
library(car)
boxTidwell(Murder ~ Population + Illiteracy, data = states) # transformation for Pop is ^0.87 but again not necessary here

# comparing fit of two models
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
anova(fit2, fit1) # we see that the full model (fit2) is not any better than the reduced model
# also
AIC(fit1, fit2) # the full model has a higher AIC score than the reduced model as well

# backward stepwise regression for feature selection
library(MASS)
states <- as.data.frame(state.x77[, c('Murder', 'Population', 'Illiteracy', 'Income', 'Frost')])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
stepAIC(fit, direction = 'backward')

# All subsets regression (does all of the models unlike in stepwise)
library(leaps)
states <- as.data.frame(state.x77[, c('Murder', 'Population', 'Illiteracy', 'Income', 'Frost')])
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = states, nbest = 4)
plot(leaps, scale = 'adjr2')
# the first row has an adjust R^2 for intercept + income, second row is intercept + population, top row is intercept + population + illiteracy
# also
library(car)
subsets(leaps, statistic = 'cp', main = 'Cp Plot for All Subsets Regression')
abline(1, 1, lty = 2, col = 'red') # closest to the line is Population + Illiteracy + Income

# k-fold cross validation
shrinkage <- function(fit, k = 10){
  require(bootstrap)
  theta.fit <- function(x,y) {lsfit(x, y)}
  theta.predict <- function(fit, x) {cbind(1, x) %*% fit$coef}
  x <- fit$model[, 2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup = k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat('Original R-square =', r2, '\n')
  cat(k, 'Fold Cross-Validated R-square =', r2cv, '\n')
  cat('Change =', r2 - r2cv, '\n')
}
shrinkage(fit)

# getting the relative importance of each predictor
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data = zstates)
coef(zfit) # since it is scaled, we can now directly compare the coefficients between parameters
# also
relweights <- function(fit, ...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare)*100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- 'Weights'
  import <- import[order(import), 1, drop = FALSE]
  dotchart(import$Weights, labels = row.names(import),
           xlab = '% of R-Square', pch = 19, main = 'Relative Importance of Predictor Variables',
           sub = paste('Total R-Square=', round(rsquare, digits = 3)),
           ...)
  return(import)
}
relweights(fit, col = 'blue') # we see that 59% of the R-Squared metric is attributed to Illiteracy
















