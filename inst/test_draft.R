
set.seed(10)
idf <- matrix(sample(1L:200, size = 100, replace = TRUE), ncol = 10)
idf[runif(100) < 0.2] <- NA
missing_data_set <- idf


functions <- ls("package:imputomics")

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


#bugs:
error_functions
non_df_functions
na_functions



results[["impute_mice_cart"]]
