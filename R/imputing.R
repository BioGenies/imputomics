#devtools::install_version("PEMM", version = "1.0", repos = "http://cran.us.r-project.org")

source("R/Trunc_KNN.R")
source("R/GSimp_clean.R")

#' Helper function.
#' Imputes constant value.
#' @template param_missing_ds
#' @param constant_value a constant value to impute
impute_constant <- function(missing_data_set, constant_value) {
  missing_data_set[is.na(missing_data_set)] <- constant_value
  missing_data_set
}


#' \strong{zero} imputation.
#'
#' Replaces \code{NA} with zeros (0).
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{zero} method.
#' @export
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_zero(idf)
#' }
#'
impute_zero <- function(missing_data_set)
  impute_constant(missing_data_set, constant_value = 0)


#' Helper function.
#' Basic imputation of missing values into dataframe.
#' Zero, minimum, half-minimum, mean, median and random.
#'
#' @template param_missing_ds
#' @param compute_values one of the basic imputation methods
#'
impute_per_column <- function(missing_data_set, compute_values) {
  imputed_values_vector <- compute_values(missing_data_set)

  do.call(cbind, lapply(1L:ncol(missing_data_set), function(ith_column_id) {
    impute_constant(missing_data_set[ith_column_id], imputed_values_vector[[ith_column_id]])
  }))
}


#' Helper function. Random imputation.
#' @template param_x
#'
compute_col_random <- function(x)
  lapply(x, function(ith_col) {
    id_nas <- is.na(ith_col)
    sample(x = ith_col[!id_nas], size = sum(id_nas), replace = TRUE)
  })

#' Helper function. Minimum imputation.
#' @template param_x
#'
compute_col_min <- function(x)
  lapply(x, min, na.rm = TRUE)

#' Helper function. Mean imputation.
#' @template param_x
#'
compute_col_mean <- function(x)
  lapply(x, mean, na.rm = TRUE)

#' Helper function. Half-minimum imputation.
#' @template param_x
#'
compute_col_halfmin <- function(x)
  lapply(x, function(i) min(i, na.rm = TRUE)/2)

#' Helper function. Median imputation.
#' @template param_x
#'
compute_col_median <- function(x)
  lapply(x, mean, na.rm = TRUE)


#' \strong{random} imputation.
#'
#' Replaces \code{NA} with random values.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{random} method.
#' @export
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_random(idf)
#' }
#'
impute_random <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_random)


#' \strong{minimum} imputation.
#'
#' Replaces \code{NA} with minimal value of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{minimum} method.
#' @export
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_min(idf)
#' }
#'
impute_min <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_min)


#' \strong{mean} imputation.
#'
#' Replaces \code{NA} with mean value of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{mean} method.
#' @export
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_mean(idf)
#' }
#'
impute_mean <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_mean)


#' \strong{half-minimum} imputation.
#'
#' Replaces \code{NA} with the half of the minimum value of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{half-minimum} method.
#' @export
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_halfmin(idf)
#' }
#'
impute_halfmin <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_halfmin)


#' \strong{median} imputation.
#'
#' Replaces \code{NA} with median of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{median} method.
#' @export
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_median(idf)
#' }
#'
impute_median <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_median)


#' Helper function for \strong{missMDA}.
#'
#' @template param_missing_ds
#'
estimate_ncp <- function(missing_data_set) {
  estimated_ncp <- missMDA::estim_ncpPCA(missing_data_set, ncp.max = ncol(df) - 2)[["ncp"]]
  ifelse(estimated_ncp < 2, 2, estimated_ncp)
}

#' Convert an imputing function into its safe version.
#' @param imputing_function a function (imputation method) that takes missing_data_set as an input
#' @template param_missing_ds
#' @return A \code{data.frame} with imputed values or the \strong{missing_data_set}
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



#' \strong{SVD} imputation.
#'
#' Singular Value Decomposition.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods:pca]{pcaMethods::pca()}} svdImpute method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods:pca]{pcaMethods::pca()}} svdImpute method.
#' @export
#' @seealso \code{\link[pcaMethods:pca]{pcaMethods::pca()}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_svd(idf)
#' }
#'
impute_svd <- function(missing_data_set) { # sprawdzic czy to nie wymaga transpozycji
  imputed <- pcaMethods::pca(missing_data_set, method = "svdImpute",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{PPCA} imputation.
#'
#' Probabilistic Principal Component Analysis.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods:pca]{pcaMethods::pca()}} PPCA method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods:pca]{pcaMethods::pca()}} PPCA method.
#' @export
#' @seealso \code{\link[pcaMethods:pca]{pcaMethods::pca()}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_ppca(idf)
#' }
#'
impute_ppca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "ppca",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxIterations = 1e5)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{BPCA} imputation.
#'
#' Bayesian Principal Component Analysis.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods:pca]{pcaMethods::pca()}} BPCA method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods:pca]{pcaMethods::pca()}} BPCA method.
#' @export
#' @seealso \code{\link[pcaMethods:pca]{pcaMethods::pca()}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_bpca(idf)
#' }
#'
impute_bpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "bpca",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{NIPALS} imputation.
#'
#' Nonlinear Iterative Partial Least Squares.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods:pca]{pcaMethods::pca()}} NIPALS method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods:pca]{pcaMethods::pca()}} NIPALS method.
#' @export
#' @seealso \code{\link[pcaMethods:pca]{pcaMethods::pca()}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_nipals(idf)
#' }
#'
impute_nipals <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "nipals",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{NLPCA} imputation.
#'
#' Nonlinear Principal Component Analysis.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods:pca]{pcaMethods::pca()}} NLPCA method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods:pca]{pcaMethods::pca()}} NLPCA method.
#' @export
#' @seealso \code{\link[pcaMethods:pca]{pcaMethods::pca()}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_nlpca(idf)
#' }
#'
impute_nlpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "nlpca",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{missMDA regularized} imputation.
#'
#' PCA method with regularized argument.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[missMDA:imputePCA]{missMDA::imputePCA}} regularized method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[missMDA:imputePCA]{missMDA::imputePCA}} regularized method.
#' @export
#' @seealso \code{\link[missMDA:imputePCA]{missMDA::imputePCA}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missmda_reg(idf)
#' }
#'
impute_missmda_reg <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
                                method = "Regularized", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}


#' \strong{missMDA EM} imputation.
#'
#' PCA method with EM argument.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[missMDA:imputePCA]{missMDA::imputePCA}} EM method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[missMDA:imputePCA]{missMDA::imputePCA}} EM method.
#' @export
#' @seealso \code{\link[missMDA:imputePCA]{missMDA::imputePCA}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missmda_em(idf)
#' }
#'
impute_missmda_em <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
                                method = "EM", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}


#' \strong{MICE} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[mice:mice]{mice::mice}}.
#'
#' @template param_missing_ds
#' @param method Imputation method for mice. One or vector of pmm, midastouch,
#' cart, rf, norm, norm.nob, norm.boot, norm.predict, lasso.norm, lasso.select.norm
#' @returns A \code{data.frame} with imputed values by \code{\link[mice:mice]{mice::mice}}.
#' @export
#' @seealso \code{\link[mice:mice]{mice::mice}}
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice(idf, method = "pmm")
#' impute_mice(idf, method = "rf")
#' }
#'
impute_mice <- function(missing_data_set, method) {
  imputed <- mice::mice(missing_data_set, method = method, m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}


#' \strong{Amelia} imputation.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[Amelia:amelia]{Amelia::amelia}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[Amelia:amelia]{Amelia::amelia}}.
#' @export
#' @seealso \code{\link[Amelia:amelia]{Amelia::amelia}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_amelia(idf)
#' }
#'
impute_amelia <- function(missing_data_set) {
  capture.output(imputed <- Amelia::amelia(missing_data_set, m = 1))
  imputed[["imputations"]][["imp1"]]
}


#' \strong{MissForest} imputation.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[missForest:missForest]{missForest::missForest}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[missForest:missForest]{missForest::missForest}}.
#' @export
#' @seealso \code{\link[missForest:missForest]{missForest::missForest}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missforest(idf)
#' }
#'
impute_missforest <- function(missing_data_set) {
  imputed <- missForest::missForest(missing_data_set, maxiter = 10, ntree = 500, replace = TRUE)
  imputed[["ximp"]]
}


#' \strong{mi} imputation.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[mi:mi]{mi::mi}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[mi:mi]{mi::mi}}.
#' @export
#' @seealso \code{\link[mi:mi]{mi::mi}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_mi(idf)
#' }
#'
impute_mi <- function(missing_data_set) {
  # requires betareg
  capture.output(imputed <- mi::mi(missing_data_set, n.chain = 1,
                                   n.iter = 100, verbose = FALSE, parallel = FALSE))

  mi::complete(imputed)[colnames(missing_data_set)]

}


#' \strong{Hmisc areg} imputation.
#'
#' Multiple Imputation using Additive Regression, Bootstrapping, and Predictive Mean Matching.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[Hmisc:aregImpute]{Hmisc::aregImpute}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[Hmisc:aregImpute]{Hmisc::aregImpute}}.
#' @export
#' @seealso \code{\link[Hmisc:aregImpute]{Hmisc::aregImpute}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_areg(idf)
#' }                                                                             ############ dont work, check it!!!!!!!!!!!!!!!!!!!!!!!
#'
#'
impute_areg <- function(missing_data_set) {
  capture.output(imputed <- Hmisc::aregImpute(formula = as.formula(paste0("~ ", paste0(colnames(df), collapse = " + "))),
                                              data = df, tlinear = FALSE))
  data.frame(do.call(cbind,
                     Hmisc::impute.transcan(imputed, imputation = 1,
                                            data = df, list.out = TRUE,
                                            pr = FALSE, check = FALSE)))
}


#' \strong{kNN} imputation.
#'
#' K Nearest Neighbors.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[impute:impute.knn]{impute::impute.knn}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[impute:impute.knn]{impute::impute.knn}}.
#' @export
#' @seealso \code{\link[impute:impute.knn]{impute::impute.knn}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' set.seed(2137)
#' impute_knn(idf)
#' }
#'
impute_knn <- function(missing_data_set) {
  # this function has a default random seed, so we need to sample one
  imputed <- impute::impute.knn(as.matrix(missing_data_set), k = 10,
                                rng.seed = sample(1L:1e9, 1))
  data.frame(imputed[["data"]])
}


#' \strong{QRLIC} imputation.
#'
#' Quantile Regression Imputation Of Left-Censored Data.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[imputeLCMD:impute.QRILC]{imputeLCMD::impute.QRILC}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[imputeLCMD:impute.QRILC]{imputeLCMD::impute.QRILC}}.
#' @export
#' @seealso \code{\link[imputeLCMD:impute.QRILC]{imputeLCMD::impute.QRILC}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_qrilc(idf)
#' }
#'
impute_qrilc <- function(missing_data_set) {
  imputeLCMD::impute.QRILC(missing_data_set)[[1]]
}


#' \strong{MLE} imputation.
#'
#' Imputation Using The EM Algorithm.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[imputeLCMD:impute.wrapper.MLE]{imputeLCMD::impute.wrapper.MLE}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[imputeLCMD:impute.wrapper.MLE]{imputeLCMD::impute.wrapper.MLE}}.
#' @export
#' @seealso \code{\link[imputeLCMD:impute.wrapper.MLE]{imputeLCMD::impute.wrapper.MLE}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_mle(idf)
#' }
#'
impute_mle <- function(missing_data_set) {
  imputed <- imputeLCMD::impute.wrapper.MLE(as.matrix(missing_data_set))
  data.frame(imputed)
}


#' \strong{tWLSA} imputation.
#'
#' Two Way Weighted Least Square Approach.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[tWLSA:wlsMisImp]{tWLSA::wlsMisImp}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[tWLSA:wlsMisImp]{tWLSA::wlsMisImp}}.
#' @export
#' @seealso \code{\link[tWLSA:wlsMisImp]{tWLSA::wlsMisImp}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_twlsa(idf)
#' }
#'
impute_twlsa <- function(missing_data_set) {
  imputed <- tWLSA::wlsMisImp(as.matrix(missing_data_set))
  data.frame(imputed)
}


#' \strong{SoftImpute} imputation.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[softImpute:softImpute]{softImpute::softImpute}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[softImpute:softImpute]{softImpute::softImpute}}.
#' @export
#' @seealso \code{\link[softImpute:softImpute]{softImpute::softImpute}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_softimpute(idf)
#' }
#'
impute_softimpute <- function(missing_data_set) {
  fit <- softImpute::softImpute(as.matrix(missing_data_set))
  data.frame(softImpute::complete(as.matrix(missing_data_set), fit))
}


#' \strong{IRMI} imputation.
#'
#' Iterative Robust Model-Based Imputation.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[NADIA:autotune_VIM_Irmi]{NADIA::autotune_VIM_Irmi}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[NADIA:autotune_VIM_Irmi]{NADIA::autotune_VIM_Irmi}}.
#' @export
#' @seealso \code{\link[NADIA:autotune_VIM_Irmi]{NADIA::autotune_VIM_Irmi}}
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_irmi(idf)
#' }
#'
impute_irmi <- function(missing_data_set) {
  NADIA::autotune_VIM_Irmi(missing_data_set, col_type = rep("numeric", ncol(missing_data_set)),
                           percent_of_missing = colMeans(is.na(missing_data_set))*100, maxit = 200)
}


#' \strong{PEMM} imputation.
#'
#' Penalized EM Algorithm.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[PEMM:PEMM_fun]{PEMM::PEMM_fun}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[PEMM:PEMM_fun]{PEMM::PEMM_fun}}.
#' @export
#' @seealso \code{\link[PEMM:PEMM_fun]{PEMM::PEMM_fun}}
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_PEMM(as.matrix(idf))
#' }
#'
impute_PEMM <- function(missing_data_set) {
  PEMM::PEMM_fun(missing_data_set, phi = 1)
}


#' \strong{tkNN} imputation.
#'
#' Truncated K Nearest Neighbors.
#'
#' Replaces \code{NA} values in the data frame using \strong{tkNN} method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{tkNN} method.
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_tknn(idf)
#' }
#'
impute_tknn <- function(missing_data_set) {
  imputed <- imputeKNN(as.matrix(missing_data_set), k = ceiling(nrow(missing_data_set)*0.05) + 1, distance = "truncation",
            rm.na = TRUE, rm.nan = FALSE, rm.inf = FALSE)
  data.frame(imputed)
}


#' \strong{GSimp} imputation.
#'
#' Gibbs Sampler Based Left-Censored Missing Value Imputation.
#'
#' Replaces \code{NA} values in the data frame using \strong{GSimp} method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{GSimp} method.
#' @export
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_gsimp(idf)
#' }
#'
impute_gsimp <- function(missing_data_set) {
  imputed <- GS_impute_clean(missing_data_set, initial = "lsym", imp_model='glmnet_pred')
  imputed[["data_imp"]]
}
