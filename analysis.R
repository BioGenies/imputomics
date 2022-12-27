source("functions/generate-data.R")
source("functions/imputing.R")

df <- create_df(5, 40, 0.05)

pbapply::pblapply(names(all_imp_funs), function(ith_name) try({
    mb <- microbenchmark::microbenchmark(all_imp_funs[[ith_name]](df), times = 10)
  }, silent = TRUE))


set.seed(1)

all_dfs <- unlist(lapply(c(5, 10, 50), function(ith_metabolities)
  lapply(c(20, 40), function(ith_samples) {
    create_df(ith_metabolities, ith_samples, 0.05)
  })), recursive = FALSE)

res <- lapply(all_dfs, function(ith_df)
  pbapply::pblapply(names(all_imp_funs), function(ith_name) try({
    mb <- microbenchmark::microbenchmark(all_imp_funs[[ith_name]](ith_df), times = 10)
    data.frame(summary(mb, unit = "s"), 
               name = ith_name, 
               n_metabolites = ncol(ith_df),
               n_samples = nrow(ith_df))
  }, silent = TRUE))
)
