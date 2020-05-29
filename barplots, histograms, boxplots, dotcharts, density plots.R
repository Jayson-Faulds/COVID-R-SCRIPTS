# basic barplot
library(vcd)
counts <- table(Arthritis$Improved)
counts
barplot(counts, horiz = TRUE, main = 'basic horizontal bar chart', xlab = 'frequency', ylab = 'improvement')

# stacked barplot
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts
barplot(counts, main = 'grouped bar chart', xlab = 'treatment', ylab = 'frequency', col = c('red', 'yellow', 'green'),
        legend = rownames(counts))
# do this to change legend position a bit
barplot(counts, main = 'grouped bar chart', xlab = 'treatment', ylab = 'frequency', col = c('red', 'yellow', 'green'))
legend('topright', legend = rownames(counts), col = c('red', 'yellow', 'green'), pch = 15, pt.cex = 1.5)

# Grouped barplot
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts
barplot(counts, main = 'grouped bar chart', xlab = 'treatment', ylab = 'frequency', col = c('red', 'yellow', 'green'),
        beside = TRUE)
legend('topright', legend = rownames(counts), col = c('red', 'yellow', 'green'), pch = 15, pt.cex = 1.5)

# barplots for aggregations like mean
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by = list(state.region), mean)
means <- means[order(means$x), ]
means
barplot(means$x, names.arg = means$Group.1, main = 'mean illiteracy rate')

# fitting labels in barplot
opar <- par()
par(mar = c(5, 8, 4, 2))
par(las = 2)
counts <- table(Arthritis$Improved)
barplot(counts, main = 'treatment outcome', horiz = TRUE, cex.names = 0.8, names.arg = c('No Improvement',
                                                                                         'Some Improvement',
                                                                                         'Marked Improvement'))
par(opar)

# Spinograms (stacked bars but the bar always has length 1)
library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = 'spinogram example')
detach(Arthritis)

# Basic pie chart
par(mfrow = c(2, 2))
slices <- c(10, 12, 4, 16, 8)
lbls <- c('US', 'UK', 'Australia', 'Germany', 'France')
pie(slices, labels = lbls, main = 'simple pie chart')

# pie chart with percentages
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, ' ', pct, '%', sep = '')
pie(slices, labels = lbls2, col = rainbow(length(slices)), main = 'pie chart with percentages')

# 3D pie chart
library(plotrix)
pie3D(slices, labels = lbls, explode = 0.1, main = '3D pie chart')

# pie chart from a table
mytable <- table(state.region)
lbls3 <- paste(names(mytable), '\n', mytable, sep = '')
pie(mytable, labels = lbls3, main = 'pie chart from a table\n (with sample sizes')

# fan plot (A better version of a pie chart but probably still worse than a bar chart)
library(plotrix)
slices <- c(10, 12, 4, 16, 8)
lbls <- c('US', 'UK', 'Australia', 'Germany', 'France')
fan.plot(slices, labels = lbls, main = 'fan plot')

# basic histogram
par(mfrow = c(2, 2))
hist(mtcars$mpg)

# adjusting bin count
hist(mtcars$mpg, breaks = 12, col = 'red', xlab = 'miles per gallon', main = 'colored histogram with 12 bins')

# adding a rug plot and density curve
hist(mtcars$mpg, freq = FALSE, breaks = 12, col = 'red', xlab = 'miles per gallon', main = 'histogram, rug plot, density curve')
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col = 'blue', lwd = 2)

# adding a normal curve instead of a density curve
x <- mtcars$mpg
h <- hist(x, breaks = 12, col = 'red', xlab = 'miles per gallon',
          main = 'histogram with normal curve and box')
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col = 'blue', lwd = 2)
box()

# basic kernel density plot
par(mfrow = c(2, 1))
d <- density(mtcars$mpg)
plot(d)

# filled density plot with rug plot
d <- density(mtcars$mpg)
plot(d, main = 'kernel density of miles per gallon')
polygon(d, col = 'red', border = 'blue')
rug(mtcars$mpg, col = 'brown')

# comparing different density plots
library(sm)
attach(mtcars)
cyl.f <- factor(cyl, levels = c(4, 6, 8),
                labels = c('4 cylinder', '6 cylinder', '8 cylinder'))
sm.density.compare(mpg, cyl, xlab = 'miles per gallon', main = 'mpg distribution by car cylinders')
colfill <- c(2: (1 + length(levels(cyl.f))))
legend('topright', levels(cyl.f), fill = colfill)

# basic boxplot
boxplot(mtcars$mpg, main = 'box plot', ylab = 'miles per gallon')

# grouped boxplots
boxplot(mpg ~ cyl, data = mtcars, main = 'car mileage data', xlab = 'number of cylinders', ylab = 'miles per gallon')

# adding a notch, making the width of the plots proportional to their sample size
boxplot(mpg ~ cyl, data = mtcars, notch = TRUE, varwidth = TRUE, col = 'red',
        main = 'car mileage data', xlab = 'number of cylinders', ylab = 'miles per gallon')

# box plots for more than one grouped variable
mtcars$cyl.f <- factor(mtcars$cyl, levels = c(4, 6, 8), labels = c('4', '6', '8'))
mtcars$am.f <- factor(mtcars$am, levels = c(0, 1), labels = c('auto', 'standard'))
boxplot(mpg ~ am.f*cyl.f, data = mtcars, varwidth = TRUE, col = c('gold', 'darkgreen'),
        main = 'mpg distribution by auto type', xlab = 'auto type', ylab = 'miles per gallon')

# violin plots (basically a box plot combined with a density plot)
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]
vioplot(x1, x2, x3, names = c('4 cyl', '6 cyl', '8 cyl'), col = 'gold',
        title = 'violin plots of miles per gallon', xlab = 'number of cylinders', ylab = 'miles per gallon')

# basic dotplot
dotchart(mtcars$mpg, labels = row.names(mtcars), cex = 0.7, main = 'gas mileage for car models',
         xlab = 'miles per gallon')

# dotplot sorted, grouped, and colored
x <- mtcars[order(mtcars$mpg), ]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- 'red'
x$color[x$cyl == 6] <- 'blue'
x$color[x$cyl == 8] <- 'darkgreen'
dotchart(x$mpg, labels = row.names(x), cex = 0.7, groups = x$cyl,
         gcolor = 'black', color = x$color, pch = 19,
         main = 'gas mileage for car models\ngrouped by cylinder',
         xlab = 'miles per gallon')
















