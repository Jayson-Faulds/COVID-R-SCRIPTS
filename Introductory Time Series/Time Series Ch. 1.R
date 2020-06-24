# quick look at a time series object
data("AirPassengers")
AP <- AirPassengers
AP
plot(AP)
layout(1:2)
plot(aggregate(AP)) # using aggregate() will aggregate by year and eliminate the seasonal variation
boxplot(AP ~ cycle(AP)) # shows how the time series tends to shift from month to month

# another example
Maine.month <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/Maine.dat', header = TRUE)
time_series <- ts(Maine.month, start = c(1996, 1), freq = 12) # makes time series object
time_series_annual <- aggregate(time_series)/12

# using windows to subset time series
ts_february <- window(time_series, start = c(1996, 2), freq = TRUE) # time series of only february entries
feb.ratio <- mean(ts_february)/mean(time_series) # get the ratio of average february vs general

# comparing multiple time series
commodity <- read.table('http://www.maths.adelaide.edu.au/andrew.metcalfe/Data/cbe.dat', header = TRUE)
Elec.ts <- ts(commodity[, 3], start = 1958, freq = 12)
mult <- ts.intersect(AP, Elec.ts) # gets the intersection of two time series that have an overlap in time
AP <- mult[, 1]
Elec_mult <- mult[, 2]
plot(AP)
plot(Elec_mult)
plot(as.vector(AP), as.vector(Elec_mult)) # makes a scatterplot of the two time series with regression line
abline(lm(Elec_mult ~ AP))
# sometimes, the two time series can have similar seasonal variations, meaning it is usually better to remove trends
# and seasonal effects before comparing the two series

# stochastic trends: when trends change direction at unpredictable/random times (common in financial data)
# there is a risk in using extrapolation for the future when working with stochastic trends due to the unpredictable nature

# simple additive decomposition model: xt = mt + st + zt (xt is the observed series, mt is the trend, st is the seasonal effect, zt is the error term)
# zt is generally a correlated sequence of random variables with mean equal to zero

# multiplicative model: xt = mt * st + zt (If the seasonal effect tends to increase as the trend increases)
# in other words, the variance gets bigger when there is a trend

# log(xt) = mt + st + zt (If the random variation is also modeled by a multiplicative factor and the variable is positive)
# xt = e^(mt + st) * e^(1/2 * sigma^2)  (IFF zt is N(0, sigma^2))

# estimating trend: mt = (x_january + x_february + ... + x_december)/12  (here, mt refers to the trend in the middle of the year, like around June/July)
# technically, its like this: mt = (1/2*x_january + x_february + ... + x_november + 1/2*x_december)/12
# this is called a MOVING AVERAGE
# to estimate the trend, we need to average out the seasonal effects
# for a monthly series, we need to average 12 consecutive months
# in English? let's say our series begins in January and we want the trend estimate; we want to look at the first 12 months
# and try to average them out, so as to get rid of the seasonal effect and be left with the general trend at some time t
# Why the 1/2 thing? Technically, the average of t=1 and t=12 would correspond to t=6.5, but we don't care about time 6.5
# so let's say t = 7 (July); to estimate mt, we average months 1-12, and months 2-13, and then average those two together
# now, July is actually the center and not 6.5; since we are still dividing by 12, the first and last terms (1 & 13) are halved because they are used once not twice
# why is it called a moving average? the equation above is if t = 7
# if we next did t = 8, we would be averaging months 2-13 and 3-14, which puts 8 at the center
# since we have kind of shifted over one term, and are calculating a different set of averages, it is a moving average

# estimating monthly additive effect at time t: st = xt - mt (We have just calculated an estimation for mt above)
# for multiplicative series: st = xt/mt
# if our time series encompasses many years, we can average the effects for each month to get a better idea of the effect
# associated with each month
# if additive, the monthly effects should average close to 0 but not exactly 0
# typically, we apply an adjustment to make it equal zero by subtracting this ~0 average from each monthly effect (call this st_bar)
# sometimes, we want to report figures that are accounted for seasonality, so a more general trend can be understood
# seasonal adjustment for additive: xt - st_bar
# seasonal adjustment for multiplicative: xt/st_bar
# so, if we were presenting some kind of figure that was adjusted for seasonality (Like unemployment), we would make this transformation
# by subtracting the seasonally-adjusted mean (st_bar) for the month corresponding to time t

# the centered moving average described above is a type of smoothing
# another type of smoothing is loess, which takes a few points before and after t, and does a weighted average
# these two smoothing techniques cannot be used to forecast in the future like you can for a linear regression line

# decomposing a time series (the decompose() function uses moving averages as its method); use stl() for loess decomposition
Elec.decom <- decompose(Elec.ts, type = 'mult')
plot(Elec.decom) # multiplicative model is more likely if the variance appears to be increasing as the trend increases
                 # don't ignore the random part; if the variance is increasing you may want to do a log transformation


########################################## Exercise #################################################

### 1

## a

# time plot of the data
beer.TS <- ts(commodity[, 2], freq = 12)
plot(beer.TS)

# aggregated annual series
plot(aggregate(beer.TS))

# boxplot for each season
boxplot(beer.TS ~ cycle(beer.TS))

## b

# decompose into components and seasonal trends with residuals
beer_decomp <- decompose(beer.TS, type = 'mult')
plot(beer_decomp)

# plot of trend with superimposed seasonal effect
trend <- beer_decomp$trend
seasonal <- beer_decomp$seasonal
ts.plot(cbind(trend, trend*seasonal), lty = 1:2)













