library(testthat)

source("functions/generate-data.R")
source("functions/imputing.R")

set.seed(1)

df <- create_df(n_metabolites = 50, n_samples = 20, frac_na = 0.05)

expect_true(any(is.na(all_safe_imp_funs[["safe_impute_mice_norm.boot"]](df))))
