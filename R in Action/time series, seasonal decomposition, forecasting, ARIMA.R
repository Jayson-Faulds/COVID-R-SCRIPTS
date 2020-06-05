# making a time series object
sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20, 22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
tsales <- ts(sales, start = c(2003, 1), frequency = 12) # starts Jan, 2003; frequency set to monthly observations
tsales
plot(tsales) # 2003.5 indicates halfway through the year (July 1)
start(tsales)
end(tsales)
frequency(tsales)
tsales.subset <- window(tsales, start = c(2003, 5), end = c(2004, 6))
tsales.subset 

# smoothing time series plots using simple moving averages
# this means we replace a point with the average of itself, the previous point, and the point that comes afterward; this smoothes the plot
library(forecast)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main = 'raw time series')
plot(ma(Nile, 3), main = 'simple moving averages (k = 3)', ylim = ylim)
plot(ma(Nile, 7), main = 'simple moving averages (k = 7)', ylim = ylim)
plot(ma(Nile, 15), main = 'simple moving averages (k = 15)', ylim = ylim) # the goal is to find the general pattern/trend without smoothing too much
par(opar)

# seasonal decomposition
# time series can usually be decomposed into a trend component, seasonal component, and irregular component (error)
# we usually want to separate these effects to get a truer sense of what is going on seasonally and over time
# the decomposition is either additive or multiplicative
# additive: sales tend to increase by 100 in Winter, and decrease by 150 in Summer; this effect will always be added or subtracted
# multiplicative: sales tend to be 10% higher in the Winter; here the trend is multiplicative, and multiplying 1.1*100 is very different from 1.1*500
plot(AirPassengers)                                 # this time series is multiplicative: the variability increases as the general level gets higher
lAirPassengers <- log(AirPassengers)                # the upcoming stl() method does not work with multiplicative, so we take the log to make it additive: log(Y) = log(trend) + log(seasonal) + log(irregular)
plot(lAirPassengers, ylab = 'log(AirPassengers)')   # after taking the log, the variance is quite uniform now
fit <- stl(lAirPassengers, s.window = 'period')     # setting s.window = 'period' holds the seasonal components constant
plot(fit)                                           # general positive trend over time; seems like there is a spike during the summers
exp(fit$time.series)
                                                    # notice how the seasonal effects are constant for each month; number increases by 24% in July

# 2 other ways to visualize seasonal decomposition
library(forecase)
monthplot(AirPassengers, xlab = '', ylab = '') # all of the observations for each month are connected; its going up which implies a general trend upwards but you can still see the seasonality
seasonplot(AirPassengers, year.labels = 'TRUE', main = '') # every year is plotted, allowing you to see the monthly trends while comparing each year

# simple exponential model forecasting (weighted average of existing values to make short-term prediction for future values)
# it is called exponential because the importance of earlier values get exponentially lower as you go farther back 
# this model is for time series WITHOUT a general trend or seasonal component
library(forecast)
fit <- ets(nhtemp, model = 'ANN')                   # nhtemp is a time series object; ANN means additive for error type, None for trend and seasonal types
fit                                                 # alpha is the decay parameter: alpha close to 1 means more weight given to recent observations
forecast(fit, 1)                                    # makes a prediction for the next period; forecast is 51.87 with given confidence intervals
plot(forecast(fit, 1), clab = 'Year', ylab = expression(paste('Temperature (', degree*F,')',)),
     main = 'New Haven Annual Mean Temperature')    # plots the forecasted value with the time series
accuracy(fit)                                       # shows several accuracy metrics for our forecasting method used on each observation

# exponential smoothing including seasonal and trend components
library(forecast)
fit <- ets(log(AirPassengers), model = 'AAA')       # recall this was the multiplicative ts, so we take the log so that we can make it additive
fit                                                 # beta and gamma are decay parameters for trend and seasonal components
accuracy(fit)
pred <- forecast(fit, 5)
pred
plot(pred, main = 'Forecast for Air Travel', ylab = 'Log(AirPassengers)', xlab = 'Time')
pred$mean <- exp(pred$mean) # we exponentiate to undo the log
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c('mean', 'Lo 80', 'Lo 95', 'Hi 80', 'Hi 95')
p # we predict 450,000 for January

# Other way to do multiplicative modeling with ets()
fit <- ets(AirPassengers, model = 'MAM') # this way we don't log our ts, meaning forecasts are in the correct metric
fit
forecast(fit, 5)

# using ets() to automatically select best-fitting exponential model
library(forecast)
fit <- ets(JohnsonJohnson) # running ets() without any other parameters will let R pick the best model
fit
plot(forecast(fit), main = 'Johnson & Johnson Forecasts', ylab = 'Quarterly Earnings (Dollars)',
     xlab = 'Time', flty = 2)


# Some notes about ARIMA (Autoregressive integrated moving average) forecasting models
# predicted values are a linear function of recent actual values and recent residuals
# lag- shifting a time series back by a given number of observations:
header <- c('Lag', '1869', '1870', '1871', '1872', '1873', '1874', '1875')
lag0 <- c(0, NA, NA, 1120, 1160, 963, 1210, 1160)
lag1 <- c(1, NA, 1120, 1160, 963, 1210, 1160, 1160)
lag2 <- c(2, 1120, 1160, 963, 1210, 1160, 1160, 813)
lagtable <- rbind(lag0, lag1, lag2)
colnames(lagtable) <- header
lagtable # notice how the values get shifted back depending on the lag value

# Autocorrelation- measures the way observations relate to each other in a time series
# AC1 is the correlation between the lag0 and lag1 time series
# partial autocorrelation: correlation between the lags, with all the effect of lags inbetween removed (I think?)
# stationary time series- time series that that have a constant mean and variance over time
# this means the time series has to resemble an additive model, and must not show general trends upward or down
# stationary models are required for ARIMA models to be fit
# what if our model is not stationary? To fix unconstant variance, we do transformations like log or box-cox
# to fix unconstant mean, we use a technique called differencing, which does this: Yt -> Yt-1 - Yt
# this is what differencing looks like using our dataframe:
lagtable[1, ] - lagtable[2, ]

# steps in ARIMA forecasting: 1) Ensure a time series is stationary, 2) Identify a reasonable model to fit,
# 3) fit the model, 4) evaluate model fit, 5) make forecasts using the model

# ensuring a time series is stationary
library(forecast)
library(tseries)
plot(Nile)        # here the variance looks stable, but there might be somewhat of a downward trend
ndiffs(Nile)      # this tells us we should do differencing once
dNile <- diff(Nile)
plot(dNile)       # this looks stationary
adf.test(dNile)   # insignificant test suggests stationary

# Idenifying a reasonable model to fit
Acf(dNile)       # auto-correlation function plot (acf)
Pacf(dNile)      # partial auto-correlation function plot (pacf)
Model <- c('ARIMA(p, d, 0)', 'ARIMA(0, d, q)', 'ARIMA(p, d, q)')
ACF <- c('Trails off to 0', 'Zero after lag q', 'Trails off to zero')
PACF <- c('Zero after lag p', 'Trails off to zero', 'Trails off to zero')
ARIMA_guidelines <- cbind(Model, ACF, PACF)
ARIMA_guidelines # from our ACF/PACF plots, we would choose ARIMA(0, 1, 1) (d is number of times we differenced, which is 1)
# after an initial large lag at lag1, ACF heads toward zero. PACF trails toward zero as the lags get higher

# fitting the model
library(forecast)
fit <- arima(Nile, order = c(0, 1, 1))
fit
accuracy(fit) # use AIC and accuracy measures to compare different models

# evaluating model fit
qqnorm(fit$residuals)
qqline(fit$residuals) # residuals look good
Box.test(fit$residuals, type = 'Ljung-Box') # insignificant test means the autocorrelations don't differ from 0 which is good

# making forecasts with the model
forecast(fit, 3)
plot(forecast(fit, 3), xlab = 'Year', ylab = 'Annual Flow') # basically the same as for exponential models

# using auto.arima() to automatically select best-fitting ARIMA model
library(forecast)
fit <- auto.arima(sunspots) # auto.arima() will select the combination of p,d,q that minimizes AIC 
fit
forecast(fit, 3)
accuracy(fit)

# let's try it for the Nile time series
library(forecast)
fit <- auto.arima(Nile)
fit # interestingly, this method chose ARIMA(1,1,1)

















