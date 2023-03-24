
context("Testing imputation functions")

library(imputomics)
library(testthat)

# creates dataset
set.seed(11)
idf <- matrix(sample(1L:200, size = 60, replace = TRUE), ncol = 3)
idf[runif(60) < 0.2] <- NA
missing_data_set <- idf

# gets functions from package
all_functions <- ls("package:imputomics")
functions <- all_functions[substr(all_functions, 1, 7) == "impute_"]

# calculates result with all methods
results <- lapply(functions, function(ith_fun) {
  print(ith_fun)
  try({
    get(ith_fun)(missing_data_set)
  }, silent = TRUE)
})
names(results) <- functions

### TESTING

# check for errors
context("impute_* functions do not return error.")

lapply(functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " does not return error."), {
    expect_false(inherits(res, "try-error"))
  })
})

# check for class
context("impute_* functions return data.frame.")

lapply(functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " returns data.frame."), {
    expect_true(inherits(res, "data.frame"))
  })
})

# check for NA's
context("impute_* functions return output without NAs.")

lapply(functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " returns output without NAs."), {
    expect_false(any(is.na(res)))
  })
})

