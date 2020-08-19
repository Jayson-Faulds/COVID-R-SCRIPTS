# many time series are non-stationary because of seasonal effects or trends
# random walks are non-stationary but can be transformed to stationary by first order differencing
# now we extend the random walk model to also include AR and MA terms
# this stochastic process is called ARIMA modeling
# seasonal terms can also be added which is called a SARIMA process

# series can also be non-stationary because of serial correlation of the variance
# usually characterized by high volatility and clear changes in variance
# very common in financial and climate data
# you can model this with an autoregressive model of the variance
# This is called an ARCH model

# ARIMA
# differencing a series can reduce trend, whether it is stochastic or deterministic
CBE <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat', header = T)
Elec.ts <- ts(CBE[, 3], start = 1958, fr = 12)
plot(Elec.ts)
plot(diff(Elec.ts)) # when we plot the differenced series the trend is removed
plot(diff(log(Elec.ts))) # taking the log made the variance constant

# a time series follows an ARIMA(p, d, q) process if the dth differences of the series are an ARMA(p, q) process
# one example: x_t = x_t-1 + w_t + beta*w_t-1   this is ARIMA(0, 1, 1)
# another: x_t = alpha*x_t-1 + x_t-1 - alpha*x_t-2 + w_t    This is ARIMA(1, 1, 0)
# simulating an ARIMA(1, 1, 1): x_t = 0.5x_t-1 + x_t-1 -0.5x_t-2 + w_t + 0.3w_t-1
set.seed(1)
x <- w <- rnorm(1000)
for (i in 3:1000) x[i] <- 0.5 * x[i - 1] + x[i - 1] - 0.5 * x[i - 2] + w[i] + 0.3 * w[i - 1]
arima(x, order = c(1, 1, 1))

# you can also simulate one like this
x <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.5, ma = 0.3), n = 1000)
arima(x, order = c(1, 1, 1))

# ARIMA(0, 1, 1) aka IMA(1, 1) models are often appropriate because they represent linear trends with white noise added
Beer.ts <- ts(CBE[, 2], start = 1958, fr = 12)
Beer.ima <- arima(Beer.ts, order = c(0, 1, 1))
Beer.ima
acf(resid(Beer.ima)) # the cyclic peaks of the correlogram suggest a seasonal term is required

# making predictions/forecasts
Beer.1991 <- predict(Beer.ima, n.ahead = 12)
sum(Beer.1991$pred)

# SARIMA
# for SARIMA, you use differencing and include a lag equal to the number of seasons to remove the additioanl seasonal effects
# a common strategy to get a good-fitting model is to purposely overfit a model, and if the AIC increases then you are
# doing something right
# d = 1 here because earlier we saw that differencing one time successfully got rid of the trend
AIC(arima(log(Elec.ts), order = c(1, 1, 0), seas = list(order = c(1, 0, 0), 12))) # fit model with AR terms
AIC(arima(log(Elec.ts), order = c(0, 1, 1), seas = list(order = c(0, 0, 1), 12))) # fit model with MA terms

# function to get the best model
get.best.arima <- function(x.ts, maxord = c(1, 1, 1, 1, 1, 1)){
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for (d in 0:maxord[2]) for (q in 0:maxord[3]) for (P in 0:maxord[4]) for (D in 0:maxord[5])
    for (Q in 0:maxord[6]) {
      fit <- arima(x.ts, order = c(p, d, q), seas = list(order = c(P, D, Q),
                                                         frequency(x.ts)), method = 'CSS')
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic) {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p, d, q, P, D, Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

best.arima.elec <- get.best.arima(log(Elec.ts), maxord = c(2, 2, 2, 2, 2, 2))
best.fit.elec <- best.arima.elec[[2]]
acf(resid(best.fit.elec))
best.arima.elec[[3]]
ts.plot(cbind(window(Elec.ts, start = 1981), exp(predict(best.fit.elec, 12)$pred)), lty = 1:2)

# ARCH models
library(MASS)
data("SP500")
plot(SP500, type = 'l') # at first it looks stationary but looking closer reveals an nonconstant variance
acf(SP500)
acf((SP500 - mean(SP500))^2) # slightly adjusting the mean and getting the correlogram of the squared values 
                             # shows us evidence of volatility in the variance; here we see evidence of that

# one way to account for conditional changes in the variance is to use an autoregressive model for the variance process
# the following model introduces volatility to the white noise aspect of a time series
# ARCH(1): e_t = w_t*sqrt(alpha_0 + alpha_1*e_t-1^2)
# the variance of an ARCH(1) model behaves just like an AR(1) model
# in model fitting, a decay in the autocorrelations of the squared residuals should indicate whether an ARCH model is good
# of course, this model should only be applied when trend and seasonal effects are removed
# an generalized version is called the GARCH

# simulating non-stationary variance data and fitting GARCH(1, 1) model
# GARCH(1, 1): a_t = w_t*sqrt(h_t), where h_t = alpha_0 + alpha_1*a_t-1 + beta_1*h_t-1
set.seed(1)
alpha0 <- 0.1
alpha1 <- 0.4
beta1 <- 0.2
w <- rnorm(10000)
a <- rep(0, 10000)
h <- rep(0, 10000)
for (i in 2:10000) {
  h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i - 1]
  a[i] <- w[i] * sqrt(h[i])
}
acf(a)
acf(a^2) # uncorrelated values for a, but correlated values for a^2

# fitting the simulated data above to a garch model
library(tseries)
a.garch <- garch(a, grad = 'numerical', trace = FALSE) # default is GARCH(1, 1)
confint(a.garch) # confidence intervals contain the true parameter value

# fitting garch to the sp500 data
sp.garch <- garch(SP500, trace = FALSE)
sp.res <- sp.garch$residuals[-1]
acf(sp.res)
acf(sp.res^2) # the correlograms indicate a satisfactory fit

# looking at volatility in climate series example
stemp <- scan('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/stemp.dat')
stemp.ts <- ts(stemp, start = 1850, freq = 12)
plot(stemp.ts)
stemp.best <- get.best.arima(stemp.ts, maxord = rep(2, 6))
stemp.best[[3]]
stemp.arima <- arima(stemp.ts, order = c(1, 1, 2), seas = list(order = c(2, 0, 1), 12))
t(confint(stemp.arima)) # the second seasonal component is not significantly different from 0, so we refit without
stemp.arima <- arima(stemp.ts, order = c(1, 1, 2), seas = list(order = c(1, 0, 1), 12))
t(confint(stemp.arima))
stemp.res <- resid(stemp.arima)
acf(stemp.res)
acf(stemp.res^2) # clear evidence of volatility in variance
stemp.garch <- garch(stemp.res, trace = FALSE) # notice how we fit GARCH on the residual series
t(confint(stemp.garch)) # coefficients are all statistically significant
stemp.garch.res <- resid(stemp.garch)[-1]
acf(stemp.garch.res)
acf(stemp.garch.res^2) # correlograms look good--satisfactory fit is achieved





















