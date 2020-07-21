# basic recap: decompose() assumes a fixed seasonal pattern about a trend; trend is estimated by local averaging of deseasonalised data
# HoltWinters() allows seasonal variation and trend to change over time; these are estimated with exponentially weighted averages

# residual error series: the set of residuals between model predictions and actual observations
# residual error series are generally stationary with random variation from period to period, but occasionally there is some 
# structure, like consecutive errors being positively correlated
# we assume our model is a good fit if the residual error series is a realization of independent random variables
# The random walk is a type of non-stationary model based on white noise/independent random variation
# it is a good benchmark to use for more complicated models
# we refer to residual error as x_t in this section

# if a model has accounted for all of the serial correlation in the data, there should be no obvious patterns in the
# correlogram of the residuals, as we've seen in previous sections (the residual series is said to be uncorrelated)
# discrete white noise: if the time series variables are IID with mean of 0 and standard deviation sigma^2
# because the variables are independent, Cor(w_i, w_j) = 0; or, there is no autocorrelation at any lag
# if the time series variables are IID normal: w_t ~ N(0, sigma^2), it is called Gaussian white noise
# usually white noise models appear as a residual series after an appropriate time series model is fit

# example of a simulated white noise series:
set.seed(2)
acf(rnorm(100)) # we expect around 5% of autocorrelations to differ significantly from 0
                # the only parameter for a white noise series is sigma^2, which we can estimate using var()

# second-order properties of the white noise definition:
# recall that mu_w = 0 (AKA, the time series variables have mean 0)
# gamma_k = Cov(w_t, w_t+k) = sigma^2 when k != 0 (AKA, constant variance)
# gamma_k = cov(w_t, w_t+k) = 0 when k = 0
# autocorrelation function: rho_k = 1 when k = 0; rho_k = 0 when k != 0 (AKA, independent time series variables)

# random walks: provide a good fit to data with stochastic trends
# a time series is a random walk if: x_t = x_t-1 + w_t (w_t here refers to white noise or error)
# because of the x_t-1, we can continually substitute in the previous term until we get to x_0
# the equation will look like this: x_t = x_0 + sum(w_0, w_1, ..., w_t)
# random walk properties: mu_x = 0 (Constant mean), gamma_k(t) = cov(x_t, x_t+k) = t*sigma^2 (Not constant covariance)
# var(x_t) = t*sigma^2 as well
# because the covariance and variance are functions of time, the process is non-stationary
# because the variance constantly increases with time, random walks are only good for short-term
# autocorrelation function: rho_k(t) = 1/sqrt(1 + k/t), correlogram should have positive autocorrelations that slowly decay as k increases
x <- rnorm(1000)
w <- x
for (t in 2:1000) x[t] <- x[t - 1] + w[t] # builds the random walk
plot(x, type = 'l')
acf(x) # here we see the gradual decay of autocorrelations

# the first-order differences of a random walk are a white noise series
# the correlogram of the differenced series can be used to assess whether the series is modeled well by a random walk
acf(diff(x)) # pretty much 0, so the simulated series in x likely follows a random walk

# another example
Z <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/pounds_nz.dat', header = TRUE)
Z.ts <- ts(Z, st = 1991, fr = 4)
acf(diff(Z.ts)) # significant at lag 1; might indicate a more complicated model is needed; random walk is still good approximation
Z.hw <- HoltWinters(Z.ts, alpha = 1, gamma = 0) # we try a Holt Winters model and allow some trend effect into the model
acf(resid(Z.hw)) # plots the correlogram of the residuals of the fitted values
                 # to be honest, this chart does not match the correlogram from the book and doesn't seem to fit adequately

# random walk with drift example
# we include a drift parameter like this: x_t = x_t-1 + delta + w_t
# what you do is get the correlogram of the differenced series and determine if its well-modeled by white noise
# if so, calculate the mean and sd() of the differences and make a confidence interval
# this is the confidence interval for our delta parameter which represents drifting of the series
HP.dat <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/HP.txt', header = TRUE)
plot(as.ts(HP.dat$Price))
DP <- diff(HP.dat$Price)
plot(as.ts(DP))
acf(DP) # looks good
mean(DP) + c(-2, 2)*sd(DP)/sqrt(length(DP)) # the interval is entirely above 0, suggesting a positive drift

# Autoregressive models (AR)
# a series is AR(p) if x_t = alpha_1*x_t-1 + alpha_2x_t-2 + ... + alpha_p*x_t-p + w_t
# in other words, a linear combination of the previous p observations
# a random walk is AR(1) when alpha = 1 (x_t = alpha*x_t-1 + w_t)
# exponential smoothing is when alpha_i = alpha(1 - alpha)^i
# the model is a regression of x_t on past terms from the same series, which is why its called autoregressive
# the alpha values are determined by minimizing the sum of squared errors

# usually we restrict AR models to stationary data
# for AR(1): -1 < alpha < 1
# for AR(2): -1 < alpha_2 < 1, alpha_1 + alpha_2 < 1, alpha_2 - alpha_1 < 1
# and so on. R will usually handle this itself

# AR(1) correlogram
# rho_k = alpha^k  (for a small alpha, autocorrelation decays to 0 pretty quickly)
rho <- function(k, alpha) alpha^k
layout(1:2)
plot(0:10, rho(0:10, 0.7), type = 'b')
plot(0:10, rho(0:10, -0.7), type = 'b') # notice the behavior when its positive vs negative
layout(1:1)

# partial autocorrelation: the correlation at lag k that results after removing the effect of correlations from shorter lags
# because AR(1) is only based on the previous period, partial autocorrelation will be 0 at all lags beyond 1
# an AR(p) model should have partial autocorrelations of 0 for all lags greater than p
# a plot of the estimated partial autocorrelations is useful to determine the order of a good AR process
set.seed(1)
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- 0.7 * x[t - 1] + w[t]
plot(x, type = 'l')
acf(x)
pacf(x) # partial autocorrelation is only significant at lag 1, which is what we expect from an underlying AR(1) process

# fitting an AR model
x.ar <- ar(x, method = 'mle')
x.ar$order                              # order is determined using AIC, which penalizes added parameters
x.ar$ar                                 # fitted using MLE (notice how it estimated 0.6 when actual was 0.7 above)
x.ar$ar + c(-2, 2) * sqrt(x.ar$asy.var) # confidence interval for the alpha parameter, it contains the true value 0.7

# Exchange rate example
Z.ar <- ar(Z.ts)
mean(Z.ts)
Z.ar$order
Z.ar$ar
Z.ar$order + c(-2, 2) * sqrt(Z.ar$asy.var) # notice how the interval includes 1 and the order is 1, meaning the data is well-approximated by random walk
acf(Z.ar$resid[-1])

# global temperature example
Global <- scan('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat')
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
Global.ar <- ar(aggregate(Global.ts, FUN = mean), method = 'mle') # AR model fitted to the mean annual global temperatures
mean(aggregate(Global.ts, FUN = mean))
Global.ar$order # AR(4) model
Global.ar$ar
acf(Global.ar$resid[-(1:Global.ar$order)], lag = 50) # correlogram of the residual series for the AR(4) model looks nice, meaning AR(4) is a good fit
                                                     # a simple stochastic model can explain the correlation and the trend
                                                     # AR models don't have deterministic trend components, meaning the trend in the data can be
                                                     # explained by serial correlation and random variation; it is therefore possible that the
                                                     # upward temp trend is stochastic (This doesn't mean the trend is happening for no reason)

############################################## Exercises ######################################################

### 1

# exponential white noise
w <- rexp(1000) - 1
hist(w)
acf(w)

### 2

## a

# build the AR(1) simulation, alpha = -0.5
x <- w <- rnorm(100)
for (t in 2:100) x[t] <- -0.5 * x[t - 1] + w[t]

# get alpha estimate and predictions
x.ar <- ar(x)
x.ar$order
x.ar$ar # -0.62 vs. -0.5 in reality
predict(x.ar, n.ahead = 10)

### 4

## a

# simulate the time series
x <- w <- rnorm(1000)
for(t in 3:1000) x[t] <- (5/6 * x[t - 1]) - (1/6 * x[t - 2]) + w[t] # looks like an AR(2) model

## b

# correlogram and partial correlogram
acf(x)
pacf(x) # strongly significant for the first 2 lags, as we expect

## c

# fit the AR model
x.ar <- ar(x)
x.ar$order # decides to fit an AR(3) model which is not quite right
x.ar$ar

## d

# confidence intervals for parameter estimates
x.ar$ar[1] + c(-2, 2) * sqrt(diag(x.ar$asy.var)[1]) # the interval contains 5/6
x.ar$ar[2] + c(-2, 2) * sqrt(diag(x.ar$asy.var)[2]) # the interval contains -1/6
x.ar$ar[3] + c(-2, 2) * sqrt(diag(x.ar$asy.var)[3]) # this parameter does not exist in the actual model

## f

# plot the correlogram of the residuals
acf(x.ar$resid[-(1:3)])











