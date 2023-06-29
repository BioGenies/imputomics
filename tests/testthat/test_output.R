
context("Testing imputation functions")

library(imputomics)
library(testthat)

# creates dataset
data(sim_miss)
data(sim_miss_large)
missing_data_set_large <- sim_miss_large + 1
missing_data_set <- sim_miss + 1

# gets functions from package
all_functions <- ls("package:imputomics")
imp_functions <- all_functions[substr(all_functions, 1, 7) == "impute_"]

proper_ds <- rep("missing_data_set", length(imp_functions))
names(proper_ds) <- imp_functions

# because mai and regimpute needs large data
proper_ds[["impute_mai"]] <- "missing_data_set_large"
proper_ds[["impute_regimpute"]] <- "missing_data_set_large"
# calculates result with all methods
results <- lapply(imp_functions, function(ith_fun) {
  try({
    get(ith_fun)(get(proper_ds[ith_fun]))
  }, silent = TRUE)
})
names(results) <- imp_functions

### TESTING

# check for errors
context("impute_* functions do not return error.")

lapply(imp_functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " does not return error."), {
    expect_false(inherits(res, "try-error"))
  })
})

# check for class
context("impute_* functions return data.frame.")

lapply(imp_functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " returns data.frame."), {
    expect_true(inherits(res, "data.frame"))
  })
})

# check for NA's
context("impute_* functions return output without NAs.")

lapply(imp_functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " returns output without NAs."), {
    expect_false(any(is.na(res)))
  })
})

# check for dimension
context("impute_* functions return output with proper dimension.")

lapply(imp_functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " returns output with proper dimension."), {
    expect_true(all(dim(res) == dim(get(proper_ds[ith_method]))))
  })
})

# check for right result
context("impute_* functions do not change the original data.")

lapply(imp_functions, function(ith_method) {
  res <- results[[ith_method]]
  test_that(paste0(ith_method, " do not change the original data."), {
    expect_true(all.equal(res[!is.na(get(proper_ds[ith_method]))],
                          get(proper_ds[ith_method])[!is.na(get(proper_ds[ith_method]))],
                          check.attributes = FALSE))
  })
})
