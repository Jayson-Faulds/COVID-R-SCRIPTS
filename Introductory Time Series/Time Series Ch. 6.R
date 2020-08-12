# stationary models can be suitable for residual series that contain no obvious trends or seasonal cycles
# good regression models can often remove these effects from the residual series
# the fitted stationary models in this chapter can be combined with fitted regression models to improve forecasts

# strictly stationary series: when the distribution is unchanged even if you shift time forward an arbitrary step ahead
# strictly stationary implies that mean and variance are constant over time and that the autocovariance is not affected by time
# reminder that if we fit a stationary series model, we are making an assumption that our data meet these criteria
# so what we should first do is check for any trend or seasonal effect, and then try to remove it to create stationarity
# usually, we treat the residual series produced from a regression model to be stationary

# moving average models: x_t = w_t + beta_1*w_t-1 + ... + beta_q*w_t-q, where w_t is white noise with mean zero and variance sigma_w^2
# simulating an MA model
rho <- function(k, beta){ # this function is for the acf
  q <- length(beta) - 1
  if (k > q) ACF <- 0 else {
    s1 <- 0; s2 <- 0
    for (i in 1:(q-k+1)) s1 <- s1 + beta[i] * beta[i+k] # sum for the autocovariance
    for (i in 1:(q+1)) s2 <- s2 + beta[i]^2 # sum for the variance
    ACF <- s1/s2} # dividing the two gives autocorrelation (acf)
  ACF
}

# this code batch generates the acf for an MA(3) process with the specified beta parameters, and then creates a correlogram from those acf values at the first 10 lags
beta <- c(1, 0.7, 0.5, 0.2)
rho.k <- rep(1, 10)
for (k in 1:10) rho.k[k] <- rho(k, beta)
plot(0:10, c(1, rho.k), pch = 4, ylab = expression(rho[k]))
abline(0, 0)

# same as above but with parameters with alternating signs
beta <- c(1, -0.7, 0.5, -0.2)
rho.k <- rep(1, 10)
for (k in 1:10) rho.k[k] <- rho(k, beta)
plot(0:10, c(1, rho.k), pch = 4, ylab = expression(rho[k]))
abline(0, 0)

# simulating an MA(3)
set.seed(1)
b <- c(0.8, 0.6, 0.4)
x <- w <- rnorm(1000)
for (t in 4:1000){
  for(j in 1:3) x[t] <- x[t] + b[j] * w[t - j]
}
plot(x, type = 'l')
acf(x)

# fitting MA models
# use the arima function and set the order parameter to c(0, 0, q)
# arima minimises the conditional sum of squares to estimate values of the parameters
x.ma <- arima(x, order = c(0, 0, 3))
x.ma # notice how the parameter estimates are pretty close to our simulation

# an example
x <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/pounds_nz.dat', header = T)
x.ts <- ts(x, st = 1991, fr = 4)
x.ma <- arima(x.ts, order = c(0, 0, 1))
x.ma
acf(x.ma$residuals[-1]) # clearly this does not provide a satisfactory fit, AR(1) would be better

# mixed models: the ARMA process
# recall an AR(p) process: x_t = alpha_1*x_t-1 + ... + alpha_p*x_t-p + w_t
# autoregressive moving average process: x_t = alpha_1*x_t-1 + ... + alpha_p*x_t-p + w_t + beta_1*w_t-1 + ... + beta_q*w_t-q
# notice how it combines the AR and MA processes together; it is denoted as ARMA(p, q)

# ARMA models are fit using arima() and setting order = c(p, 0, q)
# simulating ARMA(1, 1)
set.seed(1)
x <- arima.sim(n = 10000, list(ar = -0.6, ma = 0.5))
coef(arima(x, order = c(1, 0, 1))) # notice how the estimates are quite close

# redoing the example earlier with AR, MA, and ARMA
x.ma <- arima(x.ts, order = c(0, 0, 1))
x.ar <- arima(x.ts, order = c(1, 0, 0))
x.arma <- arima(x.ts, order = c(1, 0, 1))
AIC(x.ma)
AIC(x.ar)
AIC(x.arma) # the AIC for the ARMA is the best
x.arma
acf(resid(x.arma)) # small autocorrelations in the correlogram provide evidence of a good fit

# an example following a regression
CBE <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat', header = T)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Time <- 1:length(Elec.ts)
Imth <- cycle(Elec.ts)
Elec.lm <- lm(log(Elec.ts) ~ Time + I(Time^2) + factor(Imth)) # the data has an increasing variance so we took a log of y
acf(resid(Elec.lm)) # notice how the correlogram of the residuals is still cyclical for the 12 months, meaning our monthly indicator variables
                    # were not sufficient to account for the seasonality in the series
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) { 
  fit.aic <- AIC(arima(resid(Elec.lm), order = c(i, 0, j))) # the for loop runs through each combination of ARMA(p, q) where p,q <= 2
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(Elec.lm), order = best.order) # stores the lowest model/order/AIC
    best.aic <- fit.aic
  }
}
best.order # turns out the best model is AR(2)
acf(resid(best.arma))

# What does ARMA do for us? We can use the regression to make a forecast. Then, we can use ARMA to forecast the error associated with that
# prediction. Summing these up will give us a forecast of the log for electricity, which we could then exponentiate and adjust
new.time <- seq(length(Elec.ts), length = 36)
new.data <- data.frame(Time = new.time, Imth = rep(1:12, 3))
predict.lm <- predict(Elec.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 36)
elec.pred <- ts(exp(predict.lm + predict.arma$pred), start = 1991, freq = 12)
ts.plot(cbind(Elec.ts, elec.pred), lty = 1:2) # the projections are not likely to be accurate because of the seasonal autocorrelation in the residuals
                                              # from the fitted model

# another example
wave.dat <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/wave.dat', header = T)
attach(wave.dat)
plot(as.ts(waveht), ylab = 'Wave Height')
acf(waveht)
pacf(waveht) # this suggests that p should be at least 2
wave.arma <- arima(waveht, order = c(4, 0, 4))
acf(wave.arma$residuals[-(1:4)])
pacf(wave.arma$residuals[-(1:4)])
hist(wave.arma$residuals[-(1:4)], xlab = 'height / mm', main = '')










