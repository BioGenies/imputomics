all_names <- c("impute_min", "impute_mean", "impute_halfmin", "impute_median",
               "impute_zero", "impute_random",
               "impute_bpca", "impute_ppca", "impute_svd", "impute_nipals", "impute_nlpca",
               "impute_missmda_reg", "impute_missmda_em",
               "impute_amelia",
               "impute_missforest",
               "impute_mi",
               "impute_areg",
               "impute_knn",
               "impute_qrilc",
               "impute_mle",
               "impute_twlsa",
               "impute_softimpute",
               "impute_irmi",
               "impute_tknn",
               "impute_gsimp")

# need pan package
mice_methods <- c("pmm", "midastouch", "cart", "rf", "norm", "norm.nob", "norm.boot",
                  "norm.predict", "lasso.norm", "lasso.select.norm")

all_imp_funs <- c(lapply(mice_methods, function(ith_method)
  function(missing_data_set)
    impute_mice(missing_data_set, method = ith_method)),
  lapply(all_names, get))

names(all_imp_funs) <- c(paste0("impute_mice_", gsub(pattern = ".", replacement = "_", mice_methods, fixed = TRUE)),
                         all_names)

all_safe_imp_funs <- lapply(all_imp_funs, function(ith_imp) {
  function(missing_data_set) safe_impute(ith_imp, missing_data_set)
})

names(all_safe_imp_funs) <- paste0("safe_", names(all_imp_funs))
