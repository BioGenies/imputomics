
set.seed(11)
idf <- matrix(sample(1L:200, size = 100, replace = TRUE), ncol = 5)
idf[runif(100) < 0.1] <- 0
idf[runif(100) < 0.2] <- NA
missing_data_set <- data.frame(idf)


impute_halfmin(missing_data_set)

all_functions <- ls("package:imputomics")
functions <- all_functions[substr(all_functions, 1, 7) == "impute_"]


results <- lapply(functions, function(ith_fun) {
  print(ith_fun)
  try({
    get(ith_fun)(missing_data_set)
  })
})
names(results) <- functions


error <- lapply(results, function(ith_method) {
  any(class(ith_method) == "try-error")
})
error_functions <- names(results[as.vector(unlist(error))])
non_error_functions <- names(results[!as.vector(unlist(error))])


class_res <- lapply(non_error_functions, function(ith_fun) {
  res <- results[[ith_fun]]
  any(class(res) == "data.frame")
})
non_df_functions <- non_error_functions[!(unlist(class_res))]
df_functions <- non_error_functions[(unlist(class_res))]


mat_res <- lapply(non_df_functions, function(ith_fun) {
  res <- results[[ith_fun]]
  any(class(res) == "matrix" || class(res) == "array")
})

non_mat_or_df <- non_df_functions[!(unlist(mat_res))]
mat_functions <- non_df_functions[(unlist(mat_res))]

na_res <- lapply(c(df_functions, mat_functions), function(ith_fun) {
  res <- results[[ith_fun]]
  any(is.na(res))
})

na_functions <- c(df_functions, mat_functions)[unlist(na_res)]
non_na_functions <- c(df_functions, mat_functions)[!unlist(na_res)]

bad_dim_res <- lapply(non_na_functions, function(ith_fun) {
  res <- results[[ith_fun]]
  all(dim(res) == dim(missing_data_set))
})

bad_dim_functions <- non_na_functions[!unlist(bad_dim_res)]
good_dim_fun <- non_na_functions[unlist(bad_dim_res)]


edit_res <- lapply(good_dim_fun, function(ith_fun) {
  res <- results[[ith_fun]]
  isTRUE(all.equal(res[!is.na(missing_data_set)],
                   missing_data_set[!is.na(missing_data_set)],
                   check.attributes = FALSE))
})


bad_edit_functions <- good_dim_fun[!unlist(edit_res)]
good_funs <- good_dim_fun[unlist(edit_res)]

#bugs:
error_functions
non_df_functions
non_mat_or_df
na_functions
bad_dim_functions
bad_edit_functions

c(df_functions, mat_functions)


impute_areg(missing_data_set)
impute_imputation_kNN(missing_data_set)
impute_mNMF(missing_data_set)
impute_PEMM(as.matrix(missing_data_set))[["Xhat"]]

impute_amelia(missing_data_set)
impute_BayesMetab(as.matrix(missing_data_set))
impute_corknn(missing_data_set)
impute_MA(missing_data_set)
impute_CM(missing_data_set)
impute_mice_mixed(missing_data_set)
results[["impute_corknn"]]

impute_MetabImpute_rmedian(missing_data_set)

a <- MetabImpute::Impute(data = missing_data_set,
                         method = 'GSIMP',
                         reps = 5,
                         local = TRUE,
                         rep_threshold = 1)


results[["impute_MetabImpute_GSIMP"]] == a

a == missing_data_set | is.na(missing_data_set)

dat_example <- missing_data_set
dat_example[is.na(dat_example)] <- results[["impute_MetabImpute_median"]][is.na(dat_example)]

results[["impute_MetabImpute_median"]] == dat_example
all.equal(results[["impute_MetabImpute_median"]], dat_example,
          check.attributes = FALSE)

dat_example <- missing_data_set
dat_example[is.na(dat_example)] <- impute_rmiMAE(missing_data_set)[is.na(dat_example)]

impute_rmiMAE(missing_data_set) == dat_example



impute_mice_mixed(missing_data_set)
dim(results[["impute_mice_mixed"]])




dim(impute_CM(missing_data_set))
