source("functions/generate-data.R")
source("functions/imputing.R")

set.seed(1)

all_dfs <- unlist(lapply(c(5, 10, 50), function(ith_metabolities)
  lapply(c(20, 40), function(ith_samples) {
    create_df(ith_metabolities, ith_samples, 0.05)
  })), recursive = FALSE)

res <- lapply(all_dfs[1L:2], function(ith_df)
  pbapply::pblapply(names(all_safe_imp_funs), function(ith_name) 
    lapply(1L:5, function(dummy) {
      imputation_time <- microbenchmark::microbenchmark(imputed_df <- all_safe_imp_funs[["safe_impute_softimpute"]](df),
                                                        times = 1, unit = "s")
      data.frame(summary(imputation_time), 
                 name = ith_name, 
                 n_metabolites = ncol(ith_df),
                 n_samples = nrow(ith_df),
                 converged = !any(is.na(imputed_df)))
    })
  )
)




