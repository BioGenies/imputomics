library(testthat)
library(magrittr)


source("_old/generate-data.R")
source("R/imputing.R")
source("_old/scaling.R")
source('R/GSimp_clean.R')
source("inst/test_func.R")

set.seed(2137)

create_df <- function(n_metabolites, n_samples, frac_na) {
  total <- n_metabolites * n_samples
  vals <- runif(total)
  vals[sample(1L:total, round(frac_na * total, 0))] <- NA

  data.frame(matrix(vals, nrow = n_samples))
}

mdf <- create_df(n_metabolites = 20, n_samples = 20, frac_na = 0.05)

expect_true(any(is.na(all_safe_imp_funs[["safe_impute_mean"]](mdf))))

expect_false(any(is.na(all_safe_imp_funs[["safe_impute_tknn"]](mdf))))

#all_safe_imp_funs[["safe_impute_gsimp"]](df)
#impute_gsimp(df)

scaled <- scale_df(mdf)
expect_equal(unscale_df(scaled), mdf)

set.seed(1410)

all_dfs <- unlist(lapply(c(10, 20), function(ith_metabolities)
  lapply(c(10, 20), function(ith_samples) {
    scale_df(create_df(ith_metabolities, ith_samples, 0.05))
  })), recursive = FALSE)

# test GSimp

set.seed(1996)

df <- create_df(n_metabolites = 20, n_samples = 30, frac_na = 0.05)

set.seed(1)
GS_first <- GS_impute(df)
set.seed(1)
GS_second <- GS_impute_clean(df)
expect_equal(GS_first, GS_second) # ok

microbenchmark::microbenchmark(GS_impute(df), GS_impute_clean(df), times = 5) -> czas
