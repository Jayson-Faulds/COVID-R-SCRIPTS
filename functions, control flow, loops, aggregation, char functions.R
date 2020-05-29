# trimmed mean
mean(x, trim = 0.05, na.rm = TRUE)

# Lagged differences
x <- c(1, 5, 23, 29)
diff(x)

# Scaling data to (0, 1) and to (mu, sigma)
scale(x, center = TRUE, scale = TRUE)
scale(x)*sigma + mu

# Probability functions
# d for density, p for distribution, q for quantile, r for random generation
x <- pretty(c(-3, 3), 30)
y <- dnorm(x) 
plot(x, y, type = 'l', xlab = 'Normal Deviate', ylab = 'Density', yaxs = 'i')
pnorm(1.96)
qnorm(0.9, mean = 500, sd = 100)
rnorm(50, mean = 50, sd = 10)

# generating multivariate normal data
library(MASS)
options(digits = 3)
mean <- c(230.7, 146.7, 3.6)
sigma <- matrix(c(15360.8, 6721.1, -47.1,
                  6721.2, 4700.9, -16.5,
                  -47.1, -16.5, 0.3), nrow = 3, ncol = 3)
sigma
mydata <- mvrnorm(500, mean, sigma)
mydata <- as.data.frame(mydata)
names(mydata) <- c('y', 'x1', 'x2')
head(mydata, 10)

# Character functions
nchar('sup')
substr('abcdef', 2, 4)
grep('A', c('b', 'A', 'c', 'A', 'ABC'), fixed = TRUE)
sub('\\s', '.', 'Hello There')
sub('x', 'b', 'axc')
strsplit('abc', '')
unlist(strsplit('abc', ''))[2]
sapply(strsplit('abc', ''), '[', 2)
paste('x', 1:3, sep = '')
paste('x', 1:3, sep = 'z')
cat('Hello Bob', '\b.\n', 'Isn\'t R', '\t', 'GREAT?\n')

# continuous variable -> factor
cut(y, 5, ordered = TRUE)
pretty(y, 7)

# the apply function (1 refers to by row, 2 means by column)
mydata <- matrix(rnorm(30), nrow = 6)
mydata
apply(mydata, 1, mean)
apply(mydata, 2, mean)
apply(mydata, 2, mean, trim = 0.2)

# basic for loop
for (i in 1:10) {
  print('Hello')
}

# basic while loop
i <- 10
while (i > 0) {
  print('Hello')
  i <- i - 1
}

# basic if-else statement
x <- 'hello'
if (is.character(x)) {
  x <- as.factor(x)
}

if(is.character(x)) {
  x <- as.factor(x)
} else {
  print('X is already a factor')
}

# ifelse statement
ifelse(is.character(x), 'yes', 'no')

# switch statements
feelings <- c('sad', 'afraid')
for (i in feelings) {
  print(
    switch(i,
           happy = 'I am glad you are happy',
           afraid = 'There is nothing to fear',
           sad = 'cheer up',
           angry = 'Calm down now')
  )
}

# user-defined function
mystats <- function(x, parametric = TRUE, print = FALSE){
  if (parametric){
    center <- mean(x)
    spread <- sd(x)
  } else {
    center <- median(x)
    spread <- mad(x)
  }
  if (print & parametric){
    cat('Mean=', center, '\n', 'SD=', spread, '\n')
  } else if (print & !parametric){
    cat('Median=', center, '\n', 'MAD=', spread, '\n')
  }
  result <- list(center = center, spread = spread)
  return(result)
}
x <- rnorm(300)
mystats(x)
mystats(x, parametric = FALSE)

# user-defined function with switch struct
mydate <- function(type = 'long'){
  switch(type,
         long = format(Sys.time(), '%A %B %d %Y'),
         short = format(Sys.time(), '%m-%d-%y'),
         cat(type, 'is not a recognized type\n'))
}
mydate()
mydate('short')
mydate('month')

# transposing data
mydata
t(mydata)

# Aggregating data
options(digits = 3)
attach(mtcars)
aggdata <- aggregate(mtcars, by = list(cyl, gear), FUN = mean, na.rm = TRUE)
aggdata

# Melting and casting data
library(reshape2)
id <- c(1, 1, 2, 2)
time <- c(1, 2, 1, 2)
x1 <- c(5, 3, 6, 2)
x2 <- c(6, 5, 1, 4)
mydata <- data.frame(id, time, x1, x2)
mydata
md <- melt(mydata, id = c('id', 'time'))
md
dcast(md, id ~ variable, mean)
dcast(md, time~variable, mean)
dcast(md, id~time, mean)
dcast(md, id+time~variable)
dcast(md, id+variable~time)
dcast(md, id~variable+time)






