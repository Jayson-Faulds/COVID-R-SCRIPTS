# prepare the data
loc <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/'
ds <- 'breast-cancer-wisconsin/breast-cancer-wisconsin.data'
url <- paste(loc, ds, sep = '')
breast <- read.table(url, sep = ',', header = FALSE, na.strings = '?')
names(breast) <- c('ID', 'clumpThickness', 'sizeUniformity', 'shapeUniformity', 'maginalAdhesion',
                   'singleEpithelialCellSize', 'bareNuclei', 'blandChromatin', 'normalNucleoli',
                   'mitosis', 'class')
df <- breast[-1]
df$class <- factor(df$class, levels = c(2, 4), labels = c('benign', 'malignant'))

# split the data into train and validate (here this means out of sample set)
set.seed(1234)
train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train, ]
df.validate <- df[-train, ]
table(df.train$class)
table(df.validate$class)

# logistic regression
fit.logit <- glm(class ~ ., data = df.train, family = binomial())
summary(fit.logit)
prob <- predict(fit.logit, df.validate, type = 'response')
logit.pred <- factor(prob > .5, levels = c(FALSE, TRUE), # notice we are specifying 0.5 probablity as the threshold but this can be adjusted
                     labels = c('benign', 'malignant'))  
logit.perf <- table(df.validate$class, logit.pred, dnn = c('Actual', 'Predicted'))
logit.perf

# using stepwise logistic regression for feature selection
logit.fit.reduced <- step(fit.logit)
prob <- predict(logit.fit.reduced, df.validate, type = 'response')
logit.reduced.pred <- factor(prob > .5, levels = c(FALSE, TRUE),
                             labels = c('benign', 'malignant'))
logit.reduced.perf <- table(df.validate$class, logit.reduced.pred, dnn = c('Actual', 'Predicted'))
logit.reduced.perf # the predictions got a bit better here

# basic decision tree
library(rpart)
set.seed(1234)
dtree <- rpart(class ~ ., data = df.train, method = 'class', parms = list(split = 'information'))
dtree$cptable
plotcp(dtree) # to prune trees, usually we select the most complex size that is above the dotted line (1 standard deviation above the minimum rel errer)
dtree.pruned <- prune(dtree, cp = 0.01764706) # size of tree is 4, which means 3 splits. then I enter the cp value to prune to
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104, fallen.leaves = TRUE, main =  'Decision Tree') # makes a nice looking plot
dtree.pred <- predict(dtree.pruned, df.validate, type = 'class')
dtree.perf <- table(df.validate$class, dtree.pred, dnn = c('Actual', 'Predicted'))
dtree.perf

# conditional inference trees (split is based on the variable with the lowest p-value with respect to the dependent variable)
library(party)
fit.ctree <- ctree(class ~ ., data = df.train)
plot(fit.ctree, main = 'Conditional Inference Tree')
ctree.pred <- predict(fit.ctree, df.validate, type = 'response') # notice how the party package prunes by default
ctree.perf <- table(df.validate$class, ctree.pred, dnn = c('Actual', 'Predicted'))
ctree.perf

# random forests
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class ~ ., data = df.train, na.action = na.roughfix, importance = TRUE) # nas are filled with column means or modes for categorical NA
fit.forest # because random forests do bootstrap sampling to build each tree, the observations not selected are therefore out-of-sample and are used to test the trees: see OOB estimate
importance(fit.forest, type = 2) # this will tell you which features were the most important
forest.pred <- predict(fit.forest, df.validate)
forest.perf <- table(df.validate$class, forest.pred, dnn = c('Actual', 'Predicted'))
forest.perf

# SVMs
library(e1071)
set.seed(1234)
fit.svm <- svm(class ~ ., data = df.train) # svm() will scale to (0,1) for you automatically
fit.svm
svm.pred <- predict(fit.svm, na.omit(df.validate)) # notice how we deal with NAs here
svm.perf <- table(na.omit(df.validate)$class, svm.pred, dnn = c('Actual', 'Predicted'))
svm.perf

# svm tuning using gridsearch
set.seed(1234)
tuned <- tune.svm(class ~ ., data = df.train, gamma = 10^(-6:1),
                  cost = 10^(-10:10)) # gamma is how widely a training sample reaches, while cost is the penalty for errors
tuned
fit.svm <- svm(class ~ ., data = df.train, gamma = 0.01, cost = 1)
svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class, svm.pred, dnn = c("Actual", 'Predicted'))
svm.perf

# different classification evaluation metrics
performance <- function(table, n = 2){
  if(!all(dim(table) == c(2,2)))
    stop('Must be a 2 x 2 table')
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp + fn)
  specificity = tn/(tn + fp)
  ppp = tp/(tp + fp)
  npp = tn/(tn + fn)
  hitrate = (tp + tn)/(tp + tn + fp + fn)
  result <- paste('Sensitivity = ', round(sensitivity, n) ,
                  '\nSpecificity = ', round(specificity, n) ,
                  '\nPositive Predictive Value = ', round(ppp, n),
                  '\nNegative Predictive Value = ', round(npp, n),
                  '\nAccuracy = ', round(hitrate, n), '\n', sep = '')
  cat(result)
}
performance(logit.perf)
performance(logit.reduced.perf)
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf)
performance(svm.perf)






