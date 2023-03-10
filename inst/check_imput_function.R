idf <- matrix(rnorm(1000), ncol =  10)
idf[runif(1000) < 0.01] <- NA
# idf <- cbind(idf, rnorm(100))

pcaMethods::llsImpute(idf, allVariables = TRUE)

rMisbeta::remat(idf, cl = c(rep(1,5), rep(2,5)))

source('R/ImputationWithNetwork.R')

