#devtools::install_version("PEMM", version = "1.0", repos = "http://cran.us.r-project.org")

source("functions/Trunc_KNN.R")
source("functions/GSimp.R")


#' @param missing_data_set \code{data.frame} with missing data
#' @param constant_value a constant value to impute
impute_constant <- function(missing_data_set, constant_value) {
  missing_data_set[is.na(missing_data_set)] <- constant_value
  missing_data_set
}

impute_zero <- function(missing_data_set)
  impute_constant(missing_data_set, constant_value = 0)

impute_per_column <- function(missing_data_set, compute_values) {
  imputed_values_vector <- compute_values(missing_data_set)
  
  do.call(cbind, lapply(1L:ncol(missing_data_set), function(ith_column_id) {
    impute_constant(missing_data_set[ith_column_id], imputed_values_vector[[ith_column_id]])
  }))
}

compute_col_random <- function(x)
  lapply(x, function(ith_col) {
    id_nas <- is.na(ith_col)
    sample(x = ith_col[!id_nas], size = sum(id_nas), replace = TRUE)
  })  

compute_col_min <- function(x)
  lapply(x, min, na.rm = TRUE)  

compute_col_mean <- function(x)
  lapply(x, mean, na.rm = TRUE)  

compute_col_halfmin <- function(x)
  lapply(x, function(i) min(i, na.rm = TRUE)/2)  

compute_col_median <- function(x)
  lapply(x, mean, na.rm = TRUE) 

impute_random <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_random)

impute_min <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_min)

impute_mean <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_mean)

impute_halfmin <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_halfmin)

impute_median <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_median)

estimate_ncp <- function(missing_data_set) {
  estimated_ncp <- missMDA::estim_ncpPCA(missing_data_set, ncp.max = ncol(df) - 2)[["ncp"]]
  ifelse(estimated_ncp < 2, 2, estimated_ncp)
}

#' Convert an imputing function into its safe version. 
#' @param imputing_function a function that takes missing_data_set as an input
#' @param missing_data_set \code{data.frame} with missing data
#' @return A \code{data.frame} with imputed values or the \code{missing_data_set} 
#' if the imputing function failed to converge.
safe_impute <- function(imputing_function, missing_data_set) {
  imputed <- structure(structure(list(), class = "try-error"))
  n <- 1
  while(inherits(imputed, "try-error") & n < 101) {
    imputed <- try(imputing_function(missing_data_set), silent = TRUE)
    n <- n + 1
  }
  
  if(inherits(imputed, "try-error")) {
    missing_data_set
  } else {
    imputed
  }
}


impute_svd <- function(missing_data_set) { # sprawdzic czy to nie wymaga transpozycji
  imputed <- pcaMethods::pca(missing_data_set, method = "svdImpute", 
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}

impute_ppca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "ppca", 
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxIterations = 1e5)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}

impute_bpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "bpca", 
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none", 
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}

impute_nipals <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "nipals", 
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}

impute_nlpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "nlpca", 
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none", 
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}

impute_missmda_reg <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
                                method = "Regularized", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}

impute_missmda_em <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
                                method = "EM", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}

impute_mice <- function(missing_data_set, method) {
  imputed <- mice::mice(missing_data_set, method = method, m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

impute_amelia <- function(missing_data_set) {
  capture.output(imputed <- Amelia::amelia(missing_data_set, m = 1))
  imputed[["imputations"]][["imp1"]]
}

impute_missforest <- function(missing_data_set) {
  imputed <- missForest::missForest(missing_data_set, maxiter = 10, ntree = 500, replace = TRUE)
  imputed[["ximp"]]
}

impute_mi <- function(missing_data_set) {
  # requires betareg
  capture.output(imputed <- mi::mi(missing_data_set, n.chain = 1, 
                                   n.iter = 100, verbose = FALSE, parallel = FALSE))
  
  mi::complete(imputed)[colnames(missing_data_set)]
  
}

impute_areg <- function(missing_data_set) {
  capture.output(imputed <- Hmisc::aregImpute(formula = as.formula(paste0("~ ", paste0(colnames(df), collapse = " + "))), 
                                              data = df, tlinear = FALSE))
  data.frame(do.call(cbind, 
                     Hmisc::impute.transcan(imputed, imputation = 1,
                                            data = df, list.out = TRUE, 
                                            pr = FALSE, check = FALSE)))
}

impute_knn <- function(missing_data_set) {
  # this function has a default random seed, so we need to sample one
  imputed <- impute::impute.knn(as.matrix(missing_data_set), k = 10, 
                                rng.seed = sample(1L:1e9, 1))
  data.frame(imputed[["data"]])
}


impute_qrilc <- function(missing_data_set) {
  imputeLCMD::impute.QRILC(missing_data_set)[[1]]
}

impute_mle <- function(missing_data_set) {
  imputed <- imputeLCMD::impute.wrapper.MLE(as.matrix(missing_data_set))
  data.frame(imputed)
}

impute_twlsa <- function(missing_data_set) {
  imputed <- tWLSA::wlsMisImp(as.matrix(missing_data_set))
  data.frame(imputed)
}

impute_softimpute <- function(missing_data_set) {
  fit <- softImpute::softImpute(as.matrix(missing_data_set))
  data.frame(softImpute::complete(as.matrix(missing_data_set), fit))
}


impute_irmi <- function(missing_data_set) {
  NADIA::autotune_VIM_Irmi(missing_data_set, col_type = rep("numeric", ncol(missing_data_set)),
                           percent_of_missing = colMeans(is.na(missing_data_set))*100, maxit = 200)
}


impute_PEMM <- function(missing_data_set) {
  PEMM::PEMM_fun(missing_data_set, phi = 1)
}

impute_tknn <- function(missing_data_set) {
  imputed <- imputeKNN(as.matrix(missing_data_set), k = ceiling(nrow(missing_data_set)*0.05) + 1, distance = "truncation",
            rm.na = TRUE, rm.nan = FALSE, rm.inf = FALSE)
  data.frame(imputed)
}

impute_gsimp <- function(missing_data_set) {
  imputed <- GS_impute(missing_data_set, initial = "lsym", imp_model='glmnet_pred')
  imputed[["data_imp"]] 
}
  
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
