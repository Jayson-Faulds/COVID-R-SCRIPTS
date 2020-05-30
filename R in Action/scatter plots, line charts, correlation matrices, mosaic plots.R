# scatter plot with regression line and loess curve
attach(mtcars)
plot(wt, mpg, main = 'Basic Scatter plot of MPG vs. Weight', xlab = 'Car Weight (lbs/100)',
     ylab = 'Miles Per Gallon ', pch = 19)
abline(lm(mpg ~ wt), col = 'red', lwd = 2, lty = 1)
lines(lowess(wt, mpg), col = 'blue', lwd = 2, lty = 2)
detach(mtcars)

# same scatterplot above but adding a categorical factor to the plot (I think its confusing personally)
attach(mtcars)
library(car)
scatterplot(mpg ~ wt | cyl, data = mtcars, lwd = 2, span = 0.75, main = 'Scatter Plot of MPG vs. Weight by # Cylinders',
            xlab = 'Weight of Car (lbs/1000)', ylab = 'Miles Per Gallon', legend.plot = TRUE, boxplots = 'xy')
detach(mtcars)

# basic scatterplot matrix
pairs(~mpg+disp+drat+wt, data = mtcars, main = 'Basic Scatter Plot Matrix')

# scatterplot matrix with regression lines and density curves
library(car)
scatterplotMatrix(~mpg+disp+drat+wt, data = mtcars, spread = FALSE, smoother.args = list(lty = 2),
                  main = 'Scatter Plot Matrix via car package')

# dealing with high density scatter plots (Too many dots to discern a relationship)
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean = 0, sd = 0.5), ncol = 2)
c2 <- matrix(rnorm(n, mean = 3, sd = 2), ncol = 2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c('x', 'y')
with(mydata, plot(x, y, pch = 19, main = 'scatter plot with 10,000 observations')) # makes the high density plot
with(mydata, smoothScatter(x, y, main = 'Scatter Plot colored by smoothed densities'))
# or
library(hexbin)
with(mydata, {
  bin <- hexbin(x, y, xbins = 50)
  plot(bin, main = 'Hexagonal Binning with 10,000 Obsercations') # this looks a lot nicer in my opinion
})

# 3D scatter plots
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg, main = 'Basic 3D Scatter Plot')
scatterplot3d(wt, disp, mpg, pch = 16, highlight.3d = TRUE, type = 'h', main = '3D Scatter Plot with Vertical Lines')
s3d <- scatterplot3d(wt, disp, mpg, pch = 16, highlight.3d = TRUE, type = 'h',
                     main = '3D Scatter Plot with Vertical Lines and Regression Plane')
fit <- lm(mpg ~ wt + disp)
s3d$plane3d(fit)
library(rgl)
plot3d(wt, disp, mpg, col = 'red', size = 5) # you can rotate this by clicking
detach(mtcars)

# Bubble Plots
attach(mtcars)
r <- sqrt(disp/pi) # we want the areas of the bubbles to be proportional (Not the radius) to displacement shifts, so we divide by pi and take the square root
symbols(wt, mpg, circle = r, inches = 0.3, fg = 'white', bg = 'lightblue',
        main = 'Bubble Plot with point size proportional to displacement',
        xlab = 'Weight of Car (lbs/1000)', ylab = 'Miles Per Gallon')
text(wt, mpg, rownames(mtcars), cex = 0.6)
detach(mtcars)

# Multi-faceted line charts
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)
plot(xrange, yrange, type = 'n', xlab = 'Age (days)', ylab = 'Circumference (mm)')
title('Tree Growth', 'example of line plot')
colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18, 18 + ntrees, 1)
for (i in 1:ntrees){
  tree <- subset(Orange, Tree == i)
  lines(tree$age, tree$circumference, type = 'b', lwd = 2, lty = linetype[i], col = colors[i], pch = plotchar[i])
}
legend(xrange[1], yrange[2], 1:ntrees, cex = 0.8, col = colors, pch = plotchar, lty = linetype, title = 'Tree')

# Corrgrams (Personally I do not like these)
library(corrgram)
corrgram(mtcars, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt,
         main = 'Corrgram of mtcars intercorralations')
corrgram(mtcars, order = TRUE, lower.panel = panel.ellipse, upper.panel = panel.pts, text.panel = panel.txt,
         diag.panel = panel.minmax, main = 'Corrgram of mtcars data using scatter plots and ellipses')

# Mosaic plots for categorical variable comparisons (Also not a huge fan but their alright)
library(vcd)
mosaic(Titanic, shade = TRUE, legend = TRUE)















