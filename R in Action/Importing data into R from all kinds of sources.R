# Creating a dataframe
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c('Type1', 'Type2', 'Type1', 'Type1')
status <- c('Poor', 'Improved', 'Excellent', 'Poor')
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata
table(patientdata$diabetes, patientdata$status)

# importing from delimited text file
mydataframe <- read.table('studentgrades.csv', header = TRUE, row.names = 'StudentId', sep = ',')

# importing worksheet from Excel workbook
library(xlsx)
workbook <- 'c:/myworkboox.xlsx'
mydataframe <- read.xlsx(workbook, 1)

# importing from XML: use XML package

# importing from the web: try readLines() and then manipulate with grep() and gsub()
# otherwise try RCurl and XML packages

# importing from SPSS
library(Hmisc)
mydataframe <- spss.get('mydata.sav', use.value.labels = TRUE)

# importing from SAS
library(Hmisc)
datadir <- 'C"/mydata'
sasexe <- 'C:/Program Files/SASHome/SASFoundation/9.4/sas.exe'
mydata <- sas.get(libraryName = datadir, member = 'clients', sasprog = sasexe) # member is the dataset without .sas7bdat

# importing SAS files if you don't have SAS
library(sas7bdat)
mydata <- read.sas7bdat('C:/mydata/clients.sas7bdat')

# importing from Stata
library(foreign)
mydataframe <- read.dta('mydata.dta')

# importing from NetCDF
library(ncdf)
nc <- nc_open('mynetCDFfile')
myarray <- get.var.ncdf(nc, myvar)

# downloading the rhdf5 package to import from HDF5
source('http://bioconductor.org/biocLite.R')
biocLite('rhdf5')

# Accessing DBMSs, you need the proper drivers installed for your database and platform
library(RODBC)
myconn <- odbcConnect('mydsn', uid = 'Rob', pwd = 'aardvark') # mydsn is the data source name, uid is the user id, pwd is password
crimedat <- sqlFetch(myconn, Crime)
pundat <- sqlQuery(myconn, 'SELECT * FROM Punishment')
close(myconn)




















