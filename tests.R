library(testthat)

source("functions/generate-data.R")
source("functions/imputing.R")
source("functions/scaling.R")

set.seed(1)

mdf <- create_df(n_metabolites = 20, n_samples = 20, frac_na = 0.05)

expect_true(any(is.na(all_safe_imp_funs[["safe_impute_mice_norm.boot"]](mdf))))

expect_false(any(is.na(all_safe_imp_funs[["safe_impute_tknn"]](mdf))))

#all_safe_imp_funs[["safe_impute_gsimp"]](df)
#impute_gsimp(df)

scaled <- scale_df(mdf)
expect_equal(unscale_df(scaled), mdf)
