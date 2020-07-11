# forecasting methods: 1) find a leading variable that is related to our current variable to forecast and a time interval ahead
# 2) without a leading variable, we can use a similar variable from the past to help predict
# 3) make extrapolations based on present trends continuing and implement adaptive estimates of these trends

# leading/associated variable example
Build.dat <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/ApprovActiv.dat', header = TRUE)
App.ts <- ts(Build.dat$Approvals, start = c(1996, 1), freq = 4)
Act.ts <- ts(Build.dat$Activity, start = c(1996, 1), freq = 4)
ts.plot(App.ts, Act.ts, lty = c(1, 3)) # we can see that building activity lags one quarter behind building approvals

# the relationship between two variables, where one is lagged, is quantified using the cross-correlation function (ccf)
# a plot of cross-correlation against the lags is called a cross-correlogram
# notice that the difference between this and acf is that acf is a variable and its lagged self, while ccf is two different variables
# cross-covariance (ccvf): gamma_k(x, y) = E[(x_t+k - mu_x)(y_t - mu_y)] 
# notice how x is k time periods ahead of y in this setup
# cross-correlation (ccf): rho_k(x, y) = gamma_k(x, y)/sd_x*sd_y

# analyzing cross-correlation
acf(ts.union(App.ts, Act.ts)) # returns 2 correlograms and the cross-correlograms
                              # remember that our ts is quarterly, but the lag axis is in terms of years (0.25 is 1 quarter then)
                              # several of the cross-correlations at negative lags pass the dotted lines, indicating that the
                              # approvals are leading the activity; however, we should remove the trend and seasonal effects first

# removing the trend/seasonal effect
app.ran <- decompose(App.ts)$random
app.ran.ts <- window(app.ran, start = c(1996, 3), end = c(2006, 1))
act.ran <- decompose(Act.ts)$random
act.ran.ts <- window(act.ran, start = c(1996, 3), end = c(2006, 1))
acf(ts.union(app.ran.ts, act.ran.ts))
ccf(app.ran.ts, act.ran.ts)     # the ccf plot shows the lagged relationship pretty well
print(acf(ts.union(app.ran.ts, act.ran.ts)))

# exponential smoothing
# assume no systematic or seasonal trend (Or it was removed)
# this means our forecasts using this method will be estimating the future means
# x_t = mu_t + w_t (where mu_t is the average at t and w_t is a random deviation with mean 0 and sd sigma)
# our estimate of mu_t is a_t
# because there is no trend, it is reasonable to say a_t equals a weighted average of the observed value at t and our
# estimation of the mean at t-1:
# exponentially weighted moving average: a_t = alpha*x_t + (1 - alpha)*a_t-1
# alpha is the weight/smoothing parameter (The lower it gets, the more smoothe)
# a typical value to use is 0.2, or R can give us an estimate
# the forecasting equation: x_n+k = a_n
# notice how a_t (or a_n) is a linear combination of the current and past observations, with more weight given to 
# more recent observations: a_t = alpha*x_t + alpha(1 - alpha)x_t-1 + alpha(1 - alpha)^2x_t-2 + ...
# using this method, we set the beginnging a_1 = x_1
# prediction errors are given by: e_t = x_t - a_t-1
# R gives us its estimated alpha by minimizing sum(e_t^2)  <- the sum of squared prediction errors
# note: if alpha is too small, it might take the model longer to pick up on unexpected changes in the time series or market

# Exponential smoothing example
Motor.dat <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/motororg.dat', header = TRUE)
Comp.ts <- ts(Motor.dat$complaints, start = c(1996, 1), freq = 12)
plot(Comp.ts, xlab = 'Time / months', ylab = 'Complaints') # no evidence of systematic trend or seasonal component
Comp.hw1 <- HoltWinters(Comp.ts, beta = 0, gamma = 0) # exponential smoothing is a special case of Holt Winters
Comp.hw1
plot(Comp.hw1)
Comp.hw2 <- HoltWinters(Comp.ts, alpha = 0.2, beta = 0, gamma = 0) # this time try 0.2 for alpha
Comp.hw2
plot(Comp.hw2) # this seems a little bit sensitive so we might want to dampen the alpha value a bit
               # however, at the end it looks like its trending upward, and the model is recognizing that trend

# Holt-Winters method (additive)
# uses exponentially weighted averages to update estimates of the seasonally adjusted mean, slope, and seasonals
# a_t = alpha(x_t - s_t-p) + (1 - alpha)(a_t-1 + b_t-1)   <--- estimated level (mean)
# b_t = beta(a_t - a_t-1) + (1 - beta)(b_t-1)             <--- estimated slope (trend)  
# s_t = gamma(x_t - a_t) + (1 - gamma)(s_t-p)             <--- estimated seasonal effect
# x_t - s_t-p represents the seasonally-adjusted level
# a_t - a_t-1 represents a level change and therefore a slope
# x_t - a_t represents the difference between the observation and our prediction, which we attribute to seasonal adjustment
# b_1 can be initiated as the initial slope, a_1 can be initiated as a_1 = x_1, s_1, ..., s_p  can be initiated at 0,
# reckoned from experience, or obtained from the data using decompose()
# x_n+k = a_n + k*b_n + s_n+k-p   <-- now the forecast is based on the estimated level, estimated trend, and estimated seaonal effect

# Holt-Winters multiplicative:
# a_n = alpha*(x_n/s_n-p) + (1 - alpha)(a_n-1 + b_n-1)
# b_n = beta*(a_n - a_n-1) + (1 - beta)*b_n-1
# s_n = gamma*(x_n/a_n) + (1 - gamma)*s_n-p
# x_n+k = (a_n + k*b_n)*s_n+k-p

# Holt-Winters example:
wine.dat <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/wine.dat', header = TRUE)
sweetw.ts <- ts(wine.dat$sweetw, start = c(1980, 1), freq = 12)
plot(sweetw.ts, xlab = 'Time (months)', ylab = 'Sales (1000 liters)') # seasonal variation looks multiplicative
sweetw.hw <- HoltWinters(sweetw.ts, seasonal = 'mult')
sweetw.hw
sweetw.hw$coefficients
sweetw.hw$SSE
sweetw.hw2 <- HoltWinters(sweetw.ts)
sweetw.hw2$SSE # here we see the SSE for the additive model is much higher than the multiplicative model
plot(sweetw.hw) # looks pretty damn good

# using the model to forecast:
data("AirPassengers")
AP <- AirPassengers
plot(AP) # this model appears to be multiplicative
AP.hw <- HoltWinters(AP, seasonal = 'mult')
plot(AP.hw) # again, looks pretty damn good
AP.predict <- predict(AP.hw, n.ahead = 4*12) # makes a forecast for next 48 months
ts.plot(AP, AP.predict, lty = 1:2)

################################################ Exercises ################################################

### 1

# a
w <- 1:100
k <- 1
x <- w + k * rnorm(100)
y <- w + k * rnorm(100)
ccf(x, y) # at earlier lags, there is correlation between the two variables, because they are pretty similar

# b
Time <- 1:370
x <- sin(2 * pi * Time/37)
y <- sin(2 * pi * (Time + 4)/37)
ccf(x, y) # the degree of autocorrelation between the two variables is cyclic, which makes sense

### 6

# a
default_holt <- HoltWinters(sweetw.ts)
default_holt$SSE
point_two_holt <- HoltWinters(sweetw.ts, alpha = 0.2, beta = 0.2, gamma = 0.2)
point_two_holt$SSE

# b
log_holt <- HoltWinters(log(sweetw.ts))
log_holt$SSE

### 7

## a
temp <- scan('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/global.dat')

# time plot
temp.ts <- ts(temp, freq = 12)
plot(temp.ts)

# aggregated annual mean plot
agg_temp <- aggregate(temp.ts)
plot(agg_temp)

# box series by season
boxplot(temp.ts ~ cycle(temp.ts)) # not much variation in the median across months, but the spread varies

## b

# plot decomposed series
temp.decom <- decompose(temp.ts)
plot(temp.decom)

# trend plot with superimposed seasonal effect 
ts.plot(cbind(temp.decom$trend, temp.decom$trend + temp.decom$seasonal), lty = 1:2)

## c
acf(temp.decom$random[7:1794])

## d
temp.hw <- HoltWinters(temp.ts)
temp.hw
plot(temp.hw)

## e
preds <- predict(temp.hw, n.ahead = 6*12)
ts.plot(temp.ts, preds, lty = 1:2)

### 8

# make the cumulative sum variable
Motor.dat$dif <- Motor.dat$complaints - 18
Motor.dat$csum <- cumsum(Motor.dat$dif)

# plots as a time series
plot(Motor.dat$csum, type = 'l') # looks like it stabilized over time























