# atomic vectors (vectors that contain a single data type)
x <- 1:8
class(x)
print(x)
attr(x, 'dim') <- c(2, 4) # supplying the atomic vector an argument for 'dim' converted it into a matrix
class(x) # so, a matrix is an atomic vector with a 'dim' attribute
x

# generic vectors (same thing as lists)
head(iris)
class(iris)
unclass(iris) # as you can see, dataframes are just lists, where each column is an atomic vector
set.seed(1234)
fit <- kmeans(iris[1:4], 3) # fit a basic 3-means cluster
fit
unclass(fit) # fit is a list as well
names(fit)
sapply(fit, class)

# indexing lists
fit[1] # one square bracket indexes and returns as a list
fit[[1]] # double square brackets returns the object itself
fit$cluster # using the dollar notation works as well
fit[[1]][6] # you can index the object returned if you want to be more specific in your selection

# revisiting functions (Basic function)
f <- function(x, y, z = 1){
  results <- x + 2*y + 3*z
  return(results)
}
f(2, 3, 4)
f(2, 3)

# Scoping with functions
x <- 2
y <- 3
z <- 4
f <- function(w){
  z <- 2
  x <- w*y*z
  return(x)
}
f(x) # z is reassigned within the function, so it becomes 2*3*2
x    # notice how x is still 2 after the function ran; x <- 2 inside a function does NOT affect the object outside of the function
y
z    # like x, despite being reassigned in the function, it remains the same after the function is executed

# Reassigning global objects inside functions
x <- 2
y <- 3
z <- 4
f <- function(w){
  z <- 2
  x <<- w*y*z # this <<- notation will adjust the global object now
  return(x)
}
f(x) # still returns 12
x    # notice how x has been changed to 12
y
z    # z is still unaffected by the function reassignment

# Environments
x <- 5
myenv <- new.env()
myenv$x <- 'test'
x
myenv$x # we make another x object in our new environment and assign it 'test'; notice how in the global environment, x
        # is 5, but in our new environment, it is now 'test'. The global environment is the parent of myenv

# Different environments within functions
trim <- function(p){
  trimit <- function(x){
    n <- length(x)
    lo <- floor(n*p) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  trimit
}
x <- 1:10
trim10pct <- trim(0.1) # trim10pct is now the trimit function, where p = 0.1
y <- trim10pct(x)
y
trim20pct <- trim(0.2)
y <- trim20pct(x)
y

# generic functions (functions like summary() that yield different output based on the object passed in)
summary(women)
summary(lm(weight ~ height, women))
summary # looking at the code, we see the UseMethod('summary') function is telling R to use the summary method associated with that object
        # the objects class is identified, and the appropriate summary method is called
methods(summary) # I have 32 different summary methods for different objects; notice the one for data.frame and the one for lm

# an example of generic functions
mymethod <- function(x, ...) UseMethod('mymethod') # this is essentially the same as summary()
mymethod.a <- function(x) print('Using A')
mymethod.b <- function(x) print('Using B')
mymethod.default <- function(x) print('Using Default') # mymethod() generic functions are defined for objects of class a and b along with a default
x <- 1:5
y <- 6:10
z <- 10:15
class(x) <- 'a'
class(y) <- 'b' # we reassign the class of x to 'a', so now if we call mymethod() it will run mymethod.a()
mymethod(x)
mymethod(y)
mymethod(z) # default is used when class a or b is not present
class(z) <- c('a', 'b') # some objects can have more than one class
class(z)
mymethod(z) # when this is the case, the first element is prioritized for method selection
class(z) <- c('c', 'a', 'b')
class(z)
mymethod(z) # there is no method for c; instead of doing mymethod.default, R will move along and use the a class

# inputting data more efficiently/quickly
my.data.frame <- read.table(mytextfile, header = TRUE, sep = ',') # this is a normal import
my.data.frame <- read.table(mytextfile, header = TRUE, sep = ',',
                            colClasses = c('numeric', 'numeric', 'character', NULL, 'numeric', NULL, NULL))
# in the second instance, we specify the column classes; variables that we do not want are specified as NULL
# now when R is reading in the csv, it will automatically skip the NULL columns instead of reading it, thus being more efficient

# loops vs. vectorization
set.seed(1234)
mymatrix <- matrix(rnorm(1000000), ncol = 10)
accum <- function(x){
  sums <- numeric(ncol(x))
  for (i in 1:ncol(x)){
    for(j in 1:nrow(x)){
      sums[i] <- sums[i] + x[j, i]
    }
  }
}
system.time(accum(mymatrix))
system.time(colSums(mymatrix)) # the vectorized function colSums() is optimized for performance and is preferred

# correctly sizing objects to improve efficiency
set.seed(1234)
k <- 100000
x <- rnorm(k)
y <- 0
system.time(for (i in 1:length(x)) y[i] <- x[i]^2)
y <- numeric(length = k)
system.time(for (i in 1:k) y[i] <- x[i]^2)
y <- numeric(length = k)
system.time(y <- x^2) # vectorization and specifying the appropriate object size in advance was the fastest
























