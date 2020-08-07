# stochastic trends: when it shows inexplicable change in direction, and apparent transient trends associated with high serial correlation
# with random error. These trends are well-simulated by random walks and autoregressive models
# some trends are not stochastic but deterministic: they are not random; they may depend on a relationship with some other variable
# this type of trend is better modeled using regression

# time series regression is different from regular regular regression analysis because the residuals form a time series and tend to be serially correlated
# if the serial correlation is high, this may lead to faulty parameter estimates, sometimes leading to statistical significance when it shouldn't
# time series linear regression: x_t = alpha_0 + alpha_1*x_1_t + alpha_2*x_2_t + ... + z_t   (z_t is the error)

# in time series regression, it is common for the error series z_t to be autocorrelated
set.seed(1)
z <- w <- rnorm(100, sd = 20)
for (t in 2:100) z[t] <- 0.8 * z[t - 1] + w[t] # simulates the autocorrelation of z_t: z_t is the AR(1) process z_t = 0.8*z_t-1 + w_t, where w_t is white noise
Time <- 1:100
x <- 50 + 3*Time + z # generates a time series with a straight line trend of 50 + 3t
plot(x, xlab = 'time', type = 'l')

# fitting models by minimizing the sum of squared errors
x.lm <- lm(x ~ Time)
coef(x.lm)
sqrt(diag(vcov(x.lm)))
summary(x.lm) # remember to be weary of the t-tests due to the correlation of the residual series

# diagnostic plots
acf(resid(x.lm)) # residual time series is auto-correlated
pacf(resid(x.lm)) # only lag 1 is significant, suggesting the residual series follows an AR(1) process

# an example
Global <- scan('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat')
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
temp <- window(Global.ts, start = 1970)
temp.lm <- lm(temp ~ time(temp))
coef(temp.lm)
confint(temp.lm) # the interval does not contain 0, meaning there is a positive trend if the autocorrelation in the residuals is negligible
acf(resid(lm(temp ~ time(temp)))) # we see that the residuals are autocorrelated at shorter lags, biasing our confidence interval to be too narrow

# having seen that autocorrelation of the residual series for time series regression is common, we try to account for that
# we fit with generalised leased squares, which takes that into account
library(nlme)
x.gls <- gls(x ~ Time, cor = corAR1(0.8)) # usually you would fit an lm first, do acf(), then get the autocorrelation from the correlogram
coef(x.gls)
sqrt(diag(vcov(x.gls)))

# continue with example
temp.gls <- gls(temp ~ time(temp), cor = corAR1(0.7))
confint(temp.gls) # after accounting for the autocorrelation, the interval still doesn't contain 0, implying a statistically significant effect
summary(temp.gls)

# adding a predictor for seasonality
# seasonal predictors are added as indicators; if a particular season is active, then that will be incorporated into the calculation
Seas <- cycle(temp)
Time <- time(temp)
temp.lm <- lm(temp ~ 0 + Time + factor(Seas))
coef(temp.lm)

# two-year ahead prediction
new.t <- seq(2006, len = 2 * 12, by = 1/12)
alpha <- coef(temp.lm)[1]
beta <- rep(coef(temp.lm)[2:13], 2)
(alpha * new.t + beta)[1:4]

# or
new.dat <- data.frame(Time = new.t, Seas = rep(1:12, 2))
predict(temp.lm, new.dat)[1:24]

# log transformations: can be appropriate if x_t is positive and has values close to zero
data(AirPassengers)
AP <- AirPassengers
plot(AP)
plot(log(AP)) # notice how taking the log makes the variance approximately constant as t increases

# simulating a nonlinear series (AR(1) residuals) with some negative values
set.seed(1)
w <- rnorm(100, sd = 10)
z <- rep(0, 100)
for (t in 2:100) z[t] <- 0.7 * z[t - 1] + w[t]
Time <- 1:100
f <- function(x) exp(1 + 0.05 * x) # give the time series an increasing exponential trend
x <- f(Time) + z
plot(x, type = 'l') # direct log transformation cannot be used because there are negative values
abline(0, 0)
x.nls <- nls(x ~ exp(alp0 + alp1 * Time), start = list(alp0 = 0.1, alp1 = 0.5)) # these parameter values are the starting points
summary(x.nls)$parameters # notice how the parameters are pretty close to reality

# if you apply a logarithmic transformation to the model, and want to make predictions/forecasts and undo the log, be careful
# it will most likely be biased and require a correction factor
sigma <- summary(log.lm)$sigma # pretend log.lm is a log model
lognorm.correction.factor <- exp((1/2) * sigma^2)
empirical.correction.factor <- mean(exp(resid(log.lm)))
log.lm.pred <- log.lm.pred * empirical.correction.factor

################################################# Exercises ###############################################################

### 1

## a

# set up the time series with AR(1) residuals
set.seed(1)
z <- w <- rnorm(100, sd = 25)
for (t in 2:100) z[t] <- 0.5 * z[t - 1] + w[t] # simulates the autocorrelation of z_t: z_t is the AR(1) process z_t = 0.8*z_t-1 + w_t, where w_t is white noise
Time <- 1:100
x <- 70 + 2*Time - 3*Time^2 + z # generates a time series with a straight line trend of 50 + 3t
plot(x, xlab = 'time', type = 'l')

## b

# fit a quadratic model
lm.quad <- lm(x ~ Time + Time^2)
coef(lm.quad)

## c

# 95% confidence interval for the model parameters
confint(lm.quad)

## d

# correlogram of the residuals
acf(resid(lm.quad))
pacf(resid(lm.quad))

## e

# refit using gls
quad.gls <- gls(x ~ Time, cor = corAR1(0.9))
acf(resid(quad.gls))






















