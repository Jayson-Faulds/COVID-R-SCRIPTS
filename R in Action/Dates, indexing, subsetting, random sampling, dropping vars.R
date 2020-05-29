# Intro to dates (Check out lubridate package and timeDate package for more possibilities)
mydates <- as.Date(c('2007-06-22', '2004-02-13'))
dates <- as.Date(c('01/05/1965', '08/16/1975'), '%m/%d/%Y')
Sys.Date()
date()

# Change format of dates
today <- Sys.Date()
format(today, format = '%B %d %Y')
format(today, format = '%A')

# Arithmetic with dates
startdate <- as.Date('2004-02-13')
enddate <- as.Date('2011-01-22')
days <- enddate - startdate
days

# Arithmetic with units beside days
today <- Sys.Date()
dob <- as.Date('1956-10-12')
difftime(today, dob, units = 'weeks')

# sorting dataframes
x <- 1:4
y <- c('B', 'A', 'C', 'D')
z <- c('M', 'M', 'F', 'M')
df <- data.frame(x, y, z)
df
newdata <- df[order(df$y), ]
newdata

# sorting dataframes by multiple columns (One column by decreasing)
x <- 1:4
y <- c('B', 'A', 'C', 'D')
z <- c('M', 'M', 'F', 'M')
df <- data.frame(x, y, z)
df
newdata <- df[order(df$z, -df$x), ]
newdata

# merging dataframes
total <- merge(dataframeA, dataframeB, by = 'ID')
total <- merge(dataframeA, dataframeB, by = c('ID', 'Country'))

# ways to drop variables
myvars <- names(leadership) %in% c('q3', 'q4')
newdata <- leadership[!myvars] 
# OR
newdata <- leadership[c(-8. -9)]
# OR
leadership$q3 <- leadership$q4 <- NULL

# selecting rows by date slicing
startdate <- as.Date('2009-01-01')
enddate <- as.Date('2009-10-31')
newdata <- leadership[which(leadership$date >= startdate & leadership$date <= enddate), ]

# selecting rows and columns using subset()
newdata <- subset(leadership, age >= 35 | age < 24, select = c(q1, q2, q3, q4))
newdata <- subset(leadership, gender == 'M' & age > 25, select = gender:q4)

# Taking random samples (for more complex use sampling package and survey package)
mysample <- leadership[sample(1:nrow(leadership), 3, replace = FALSE), ]

# Manipulating dataframes with SQL statements
library(sqldf)
newdf <- sqldf('SELECT * FROM mtcars WHERE carb = 1 ORDER BY mpg', row.names = TRUE)














