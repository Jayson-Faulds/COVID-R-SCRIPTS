# PCA: goal is to summarize or reduce the data dimensions by combining the variables together
# we get a smaller number of components that are uncorrelated with each other
# these new principal components are linear combinations of the original variables of the data

# basic scree plot for PCA
library(psych)
fa.parallel(USJudgeRatings[, -1], fa = 'pc', n.iter = 100, show.legend = FALSE,
            main = 'Scree plot with parallel analysis')
abline(h = 1) 
# this plot has 3 different tests: 1) the traditional bend test, which is at 1
# 2) for PCA, components above 1 are usually recommended to retain, which is also at 1
# 3) we get the mean eigenvalues for 100 random data matrices and plot that as the dashed line; usually retain if above that line

# extractinc principal components
library(psych)
pc <- principal(USJudgeRatings[, -1], nfactors = 1) # pass in the raw data or a correlation matrix
pc # pc1 column is the correlation between the original variable and the principal component
# h2 is the amount of variance in the original variable explained by the principal component, u2 is the amount not explained

# example with more than 1 principal component
library(psych)
fa.parallel(Harman23.cor$cov, n.obs = 302, fa = 'pc', n.iter = 100, show.legend = FALSE,
            main = 'Scree plot with parallel analysis') # when we pass in a correlation matrix, we need to specify number of observations
abline(h = 1) # here, we pick 2 Principal components
pc <- principal(r = Harman23.cor$cov, nfactors = 2, rotate = 'none')
pc # the first PC accounts for 58% of the variance, and PC2 22%; combined (cumulative) is 81%
# both PCs combined account for 88% of the variance for the height variable
# looking at the correlations, the first PC1 seems to be a very general factor, with positive correlations for all original variables
# PC2 is negative for the first 4, positive for the last 4; seems to be a length vs. volume thing; tough to interpret

# rotating principal components for better interpretability
rc <- principal(Harman23.cor$cov, nfactors = 2, rotate = 'varimax')
rc # notice that cumulative stays the same at 81%, and the variance explained by the components for each variable is the same
# the first component is primarily defined by the first 4 variables, and the second by the last 4

# obtaining principal component scores (to use in models)
library(psych)
pc <- principal(USJudgeRatings[, -1], nfactors = 1, score = TRUE)
head(pc$scores)

# obtaining principal component scoring coefficients (when you passed in correlation matrix instead of the raw data)
library(psych)
rc <- principal(Harman23.cor$cov, nfactors = 2, rotate = 'varimax')
round(unclass(rc$weights), 2) # PC1 = 0.28*height + 0.3*arm.span + ... + -0.04*chest.width

# Factor Analysis: goal is to analyze the correlations between the main set of variables to uncover a smaller set of
# more fundamental, unobserved variables
# whereas PCA is more for dimensionality reduction, factor analysis is useful when you're trying to understand the 
# relationships between a large number of variables and generate hypotheses

# basic scree plot for factor analysis
library(psych)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances) # converts covariance matrices to correlation matrices
fa.parallel(correlations, n.obs = 112, fa = 'both', n.iter = 100,
            main = 'Scree plots with parallel analysis')
abline(h = 0)
# by specifying both, we make scree plots for PCA and EFA analysis, triangles represent FA
# for PC, the recommended number could be either 1 or 2
# for FA, keep in mind that being over 0 is usually grounds to retain; here we would do 2 factors

# extracting factors
fa <- fa(correlations, nfactors = 2, rotate = 'none', fm = 'pa')
fa # output is the same as PCA, again the loadings are not easy to interpret

# rotating factors with orthogonal rotation (artificially force the two factors to be uncorrelated)
fa.varimax <- fa(correlations, nfactors = 2, rotate = 'varimax', fm = 'pa')
fa.varimax
# reading, vocab, and general loaded on the first factor; picture, blocks, maze, and general loaded on the second factor
# this may suggest a verbal intelligence factor and a non-verbal one

# rotating factors with oblique rotation (allows them to be correlated, usually more realistic)
fa.promax <- fa(correlations, nfactors = 2, rotate = 'promax', fm = 'pa')
fa.promax
# the PA1 and PA2 columns are no longer correlations but now standardized regression coefficients; but you can still use
# them to generate your factors names/meanings
# we see a correlation of 0.55 between the 2 factors; if this was low, then it'd be better to just use orthogonal rotation
# because it is the simpler method and those factors are uncorrelated
# the following code will generate the factor structure matrix which contains the correlations between the origina 
# variables and the new factors, which is what we are used to
fsm <- function(oblique){
  if(class(oblique)[2] == 'fa' & is.null(oblique$Phi)){
    warning('Object doesn\'t look like oblique EFA')
  }else{
    P <- unclass(oblique$loading)
    Q <- P %*% oblique$Phi
    colnames(Q) <- c('PA1', 'PA2')
    return(Q)
  }
}
fsm(fa.promax) # these are the correlations
factor.plot(fa.promax, labels = rownames(fa.promax$loadings)) # this shows you how the different variables compare for the factors
fa.diagram(fa.promax, simple = FALSE) # shows the largest loadings for each factor, as well as the correlation between
# the two factors (0.6); this is very nice when there are a bunch of factors (whereas the plot is limited to 2-d)













