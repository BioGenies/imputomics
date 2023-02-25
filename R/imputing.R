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
#' A function to replace \code{NA} in the data frame by  zeros (0).
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
#' A function to replace \code{NA} in the data frame by  random values.
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
#' A function to replace \code{NA} in the data frame by minimum values.
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
#' A function to replace \code{NA} in the data frame by  mean values.
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
#' A function to replace \code{NA} in the data frame by the half minimum values.
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
#' A function to replace \code{NA} in the data frame by a median values.
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
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()] with method = "svdImpute".
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()] with method = "svdImpute".
#' @export
#' @seealso [pcaMethods::pca()]
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_svd(idf)
#' }
#'
impute_svd <- function(missing_data_set) { # sprawdzic czy to nie wymaga transpozycji
  imputed <- pcaMethods::pca(missing_data_set, method = "svdImpute",
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{PPCA} imputation.
#'
#' Probabilistic Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()] with method = "ppca".
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()] with method = "ppca".
#' @export
#' @seealso [pcaMethods::pca()]
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_ppca(idf)
#' }
#'
impute_ppca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "ppca",
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxIterations = 1e5)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{BPCA} imputation.
#'
#' Bayesian Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()] with method = "bpca".
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()] with method = "bpca".
#' @export
#' @seealso [pcaMethods::pca()]
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_bpca(idf)
#' }
#'
impute_bpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "bpca",
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{NIPALS} imputation.
#'
#' Nonlinear Iterative Partial Least Squares.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()] with method = "nipals".
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()] with method = "nipals".
#' @export
#' @seealso [pcaMethods::pca()]
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_nipals(idf)
#' }
#'
impute_nipals <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "nipals",
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{NLPCA} imputation.
#'
#' Nonlinear Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()] with method = "nlpca".
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()] with method = "nlpca".
#' @export
#' @seealso [pcaMethods::pca()]
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
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{missMDA regularized} imputation.
#'
#' PCA method with regularized argument.
#'
#' A function to replace \code{NA} in the data frame by [missMDA::imputePCA()] with method = "regularized".
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [missMDA::imputePCA()] with method = "regularized".
#' @export
#' @seealso [missMDA::imputePCA()]
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missmda_reg(idf)
#' }
#'
impute_missmda_reg <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set,
                                method = "Regularized", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}


#' \strong{missMDA EM} imputation.
#'
#' PCA method with EM argument.
#'
#' A function to replace \code{NA} in the data frame by [missMDA::imputePCA()] with method = "em".
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [missMDA::imputePCA()] with method = "em".
#' @export
#' @seealso [missMDA::imputePCA()]
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missmda_em(idf)
#' }
#'
impute_missmda_em <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set,
                                method = "EM", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}


#' \strong{MICE} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by [mice::mice()].
#'
#' @template param_missing_ds
#' @param method Imputation method for mice. One or vector of pmm, midastouch,
#' cart, rf, norm, norm.nob, norm.boot, norm.predict, lasso.norm, lasso.select.norm
#' @returns A \code{data.frame} with imputed values by [mice::mice()].
#' @export
#' @seealso [mice::mice()]
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
#' A function to replace \code{NA} in the data frame by [Amelia::amelia()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [Amelia::amelia()].
#' @export
#' @seealso [Amelia::amelia()]
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
#' A function to replace \code{NA} in the data frame by [missForest::missForest()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [missForest::missForest()].
#' @export
#' @seealso [missForest::missForest()]
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
#' A function to replace \code{NA} in the data frame by [mi::mi()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [mi::mi()].
#' @export
#' @seealso [mi::mi()]
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
#' A function to replace \code{NA} in the data frame by [Hmisc::aregImpute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [Hmisc::aregImpute()].
#' @export
#' @seealso [Hmisc::aregImpute()]
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
#' A function to replace \code{NA} in the data frame by [impute::impute.knn()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [impute::impute.knn()].
#' @export
#' @seealso [impute::impute.knn()]
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
#' A function to replace \code{NA} in the data frame by [imputeLCMD::impute.QRILC()].
#'
#' @importFrom imputeLCMD impute.QRILC
#'
#' @template param_missing_ds
#'
#' @returns A \code{data.frame} with imputed values by [imputeLCMD::impute.QRILC()].
#'
#' @seealso [imputeLCMD::impute.QRILC()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_qrilc(idf)
#' }
#'
#' @export
#'
impute_qrilc <- function(missing_data_set) {
  imputeLCMD::impute.QRILC(missing_data_set)[[1]]
}


#' \strong{MLE} imputation.
#'
#' Imputation Using The EM Algorithm.
#'
#' A function to replace \code{NA} in the data frame by [imputeLCMD::impute.wrapper.MLE()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [imputeLCMD::impute.wrapper.MLE()].
#' @export
#' @seealso [imputeLCMD::impute.wrapper.MLE()]
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
#' A function to replace \code{NA} in the data frame by [tWLSA::wlsMisImp()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [tWLSA::wlsMisImp()].
#' @export
#' @seealso [tWLSA::wlsMisImp()]
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
#' A function to replace \code{NA} in the data frame by [softImpute::softImpute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [softImpute::softImpute()].
#' @export
#' @seealso [softImpute::softImpute()]
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
#' A function to replace \code{NA} in the data frame by [NADIA::autotune_VIM_Irmi()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [NADIA::autotune_VIM_Irmi()].
#' @export
#' @seealso [NADIA::autotune_VIM_Irmi()]
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
#' A function to replace \code{NA} in the data frame by [PEMM::PEMM_fun()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [PEMM::PEMM_fun()].
#' @export
#' @seealso [PEMM::PEMM_fun()]
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
#' A function to replace \code{NA} in the data frame by \strong{tkNN} method.
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
#' A function to replace \code{NA} in the data frame by \strong{GSimp} method.
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

#' \strong{kNN} imputation.
#'
#' K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by [VIM::kNN()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [VIM::kNN()].
#' @export
#' @seealso [VIM::kNN()]
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_vim_knn(idf)
#' }
#'
impute_vim_knn <- function(missing_data_set) {
  imputed <- VIM::kNN(missing_data_set, k = 10)[,1:ncol(missing_data_set)]
  data.frame(imputed)
}

#' \strong{MetabImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by [MetabImpute::Impute()] or [MetabImpute::imputeMulti()].
#'
#' @template param_missing_ds
#' @param method Imputation method for mice. One or vector of RF, BPCA, QRILC,
#' GSIMP, RHM, RMEAN, RMEDIAN, RMIN, RZERO, RRF, RGSIMP, RQRILC, RBPCA, min,
#' halfmin, mean, median, zero
#' @returns A \code{data.frame} with imputed values by [MetabImpute::Impute()] or [MetabImpute::imputeMulti()].
#' @export
#' @seealso [MetabImpute::Impute()], [MetabImpute::imputeMulti()]]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute(idf, method = "median")
#' impute_MetabImpute(idf, method = c("median", "RF"))
#' }
#'
impute_MetabImpute <- function(missing_data_set, method) {
  if(length(method) > 1){
    imputed <- MetabImpute::imputeMulti(data = missing_data_set,
                                        methods = method, reps = 5)
    imputed
  }else{
    imputed <- MetabImpute::Impute(data = missing_data_set, method = method,
                                   reps = 5)
    data.frame(imputed)
  }
}

#' \strong{MA} imputation.
#'
#' Mechanism-Aware imputation
#'
#' A function to replace \code{NA} in the data frame by [MAI::MAI()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [MAI::MAI()].
#' @export
#' @seealso [MAI::MAI()]
#' @examples
#' \dontrun{
#' idf <- matrix(rnorm(10000), ncol =  50)
#' idf[runif(10000) < 0.1] <- NA
#' impute_nsKNN(idf)
#' }
#'
impute_nsKNN <- function(missing_data_set) {
  imputed <- MAI::MAI(missing_data_set, MCAR_algorithm = 'Multi_nsKNN',
                      MNAR_algorithm = 'nsKNN')
  data.frame(imputed[["Imputed_data"]])
}
