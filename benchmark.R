source("functions/generate-data.R")
source("functions/imputing.R")
source("functions/scaling.R")

require(magrittr)

set.seed(1410)

all_dfs <- unlist(lapply(c(10, 20, 50, 100), function(ith_metabolities)
  lapply(c(10, 20, 50, 100), function(ith_samples) {
    scale_df(create_df(ith_metabolities, ith_samples, 0.05))
  })), recursive = FALSE)

res <- lapply(all_dfs, function(ith_df)
  pbapply::pblapply(names(all_safe_imp_funs), function(ith_name) {
    lapply(1L:5, function(dummy) {
      imputation_time <- microbenchmark::microbenchmark(imputed_df <- all_safe_imp_funs[[ith_name]](ith_df),
                                                        times = 1, unit = "s")
      res <- data.frame(summary(imputation_time), 
                        name = ith_name, 
                        n_metabolites = ncol(ith_df),
                        n_samples = nrow(ith_df),
                        converged = !any(is.na(imputed_df)))
      write.csv(res, file = paste0("./results/time-benchmark/", 
                                  ith_name, "-", 
                                  dummy, "-", 
                                  ncol(ith_df), "-", 
                                  nrow(ith_df), ".csv"), row.names = FALSE)
      res
    })
  })
)

saveRDS("./results/second-time-benchmark.RDS")
