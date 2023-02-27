#devtools::install_version("PEMM", version = "1.0", repos = "http://cran.us.r-project.org")

source("R/Trunc_KNN.R")
source("R/GSimp_clean.R")
source('R/rmiMAE.R')

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
                             nPcs = estimate_ncp(missing_data_set),
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
                             nPcs = estimate_ncp(missing_data_set),
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
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
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
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
                                method = "EM", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}

#' \strong{MICE PMM} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by predictive mean matching
#' (pmm) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by pmm used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.pmm()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_pmm(idf)
#' }
#'
impute_mice_pmm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'pmm', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE midastouch} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by weighted predictive mean
#' matching (midastouch) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by midastouch used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.midastouch()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_midastouch(idf)
#' }
#'
impute_mice_midastouch <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'midastouch', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE cart} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by classification and regression
#' trees (cart) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by cart used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.cart()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_cart(idf)
#' }
#'
impute_mice_cart <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'cart', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE rf} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by random forest imputations
#' (rf) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by rf used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.rf()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_rf(idf)
#' }
#'
impute_mice_rf <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'rf', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE norm} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by Bayesian linear regression
#' (norm) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by norm used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.norm()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm(idf)
#' }
#'
impute_mice_norm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'norm', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE norm.nob} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by linear regression ignoring
#' model error (norm.nob) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by norm.nob used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.norm.nob()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm.nob(idf)
#' }
#'
impute_mice_norm.nob <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'norm.nob', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE norm.boot} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by linear regression using
#' bootstrap (norm.boot) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by norm.boot used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.norm.boot()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm.boot(idf)
#' }
#'
impute_mice_norm.boot <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'norm.boot', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE norm.predict} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by predicted values from
#' linear regression (norm.predict) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by norm.predict used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.norm.predict()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm.predict(idf)
#' }
#'
impute_mice_norm.predict <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'norm.predict', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE lasso.norm} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by LASSO linear regression
#' (lasso.norm) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by lasso.norm used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.lasso.norm()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_lasso.norm(idf)
#' }
#'
impute_mice_lasso.norm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'lasso.norm', m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE lasso.select.norm} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by LASSO selection with linear
#' regression regression (lasso.select.norm) used [mice::mice()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by lasso.select.norm used [mice::mice()].
#' @export
#' @seealso [mice::mice()], [mice::mice.impute.lasso.select.norm()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_lasso.select.norm(idf)
#' }
#'
impute_mice_lasso.select.norm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set, method = 'lasso.select.norm', m = 1, maxit = 100, printFlag = FALSE,
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
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [imputeLCMD::impute.QRILC()].
#' @export
#' @seealso [imputeLCMD::impute.QRILC()]
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

#' \strong{corkNN} imputation.
#'
#' Correlation K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by \strong{corkNN} method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{corkNN} method.
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_tknn(idf)
#' }
#'
impute_corknn <- function(missing_data_set) {
  imputed <- imputeKNN(as.matrix(missing_data_set), k = ceiling(nrow(missing_data_set)*0.05) + 1, distance = "correlation",
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

#' \strong{MetabImpute RF} imputation.
#'
#' A function to replace \code{NA} in the data frame by RF from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of Random Forest by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_RF(idf)
#' }
#'
impute_MetabImpute_RF <- function(missing_data_set) {
    imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RF',
                                   reps = 5, local = TRUE)
    data.frame(imputed)
}

#' \strong{MetabImpute BPCA} imputation.
#'
#' A function to replace \code{NA} in the data frame by Bayesian PCA from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of Bayesian PCA by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_BPCA(idf)
#' }
#'
impute_MetabImpute_BPCA <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'BPCA',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute QRILC} imputation.
#'
#' A function to replace \code{NA} in the data frame by QRILC from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of QRILC by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_QRILC(idf)
#' }
#'
impute_MetabImpute_QRILC <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'QRILC',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute GSIMP} imputation.
#'
#' A function to replace \code{NA} in the data frame by GSIMP from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of GSIMP by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_GSIMP(idf)
#' }
#'
impute_MetabImpute_GSIMP <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'GSIMP',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by minimum from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of minimum by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_min(idf)
#' }
#'
impute_MetabImpute_min <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'min',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by half-minimum from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of half-minimum by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_halfmin(idf)
#' }
#'
impute_MetabImpute_halfmin <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'halfmin',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate half-minimum
#' from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate half-minimum
#' by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rhalfmin(idf)
#' }
#'
impute_MetabImpute_rhalfmin <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RHM',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by mean from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of mean by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_mean(idf)
#' }
#'
impute_MetabImpute_mean <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'mean',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute median} imputation.
#'
#' A function to replace \code{NA} in the data frame by median from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of median by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_median(idf)
#' }
#'
impute_MetabImpute_median <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'median',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by zero from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of zero by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_zero(idf)
#' }
#'
impute_MetabImpute_zero <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'zero',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate mean from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate mean by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rmean(idf)
#' }
#'
impute_MetabImpute_rmean <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RMEAN',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate median} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate median from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate median by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rmedian(idf)
#' }
#'
impute_MetabImpute_rmedian <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RMEDIAN',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate min} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate min from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate min by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rmin(idf)
#' }
#'
impute_MetabImpute_rmin <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RMIN',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate zero from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate zero by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rzero(idf)
#' }
#'
impute_MetabImpute_rzero <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RZERO',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate Random Forest} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate Random Forest
#' from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate Random Forest
#' by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rrf(idf)
#' }
#'
impute_MetabImpute_rrf <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RRF',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate GSIMP} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate GSIMP from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate GSIMP by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rGSIMP(idf)
#' }
#'
impute_MetabImpute_rGSIMP <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RGSIMP',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate QRILC} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate QRILC from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate QRILC by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rGSIMP(idf)
#' }
#'
impute_MetabImpute_rQRILC <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RQRILC',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate BPCA} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate BPCA from [MetabImpute::Impute()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values of replicate BPCA by [MetabImpute::Impute()].
#' @export
#' @seealso [MetabImpute::Impute()]
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rBPCA(idf)
#' }
#'
impute_MetabImpute_rBPCA <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set, method = 'RBPCA',
                                 reps = 5, local = TRUE)
  data.frame(imputed)
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

#' \strong{kNN-Euclidean} imputation.
#'
#' A function to replace \code{NA} in the data frame based on
#' \emph{Jasmit S. Shah (https://doi.org/10.1186/s12859-017-1547-6)}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by kNN-Euclidean imputation.
#' @export
#' @seealso \emph{Jasmit S. Shah (https://doi.org/10.1186/s12859-017-1547-6)}
#' @examples
#' \dontrun{
#' idf <- matrix(rnorm(10000), ncol =  50)
#' idf[runif(10000) < 0.1] <- NA
#' impute_eucknn(idf)
#' }
#'
impute_eucknn <- function(missing_data_set) {
  imputed <- KNNEuc(as.matrix(missing_data_set), k = ceiling(nrow(missing_data_set)*0.05) + 1,
                    rm.na = TRUE, rm.nan = TRUE, rm.inf = TRUE)
  data.frame(imputed)
}

#' \strong{mice mixed} imputation.
#'
#' A function to replace \code{NA} in the data frame by [missCompare::impute_data()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by [missCompare::impute_data()].
#' @export
#' @seealso [missCompare::impute_data()]
#' @examples
#' \dontrun{
#' idf <- matrix(rnorm(1000), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_mice_mixed(idf)
#' }
#'
impute_mice_mixed <- function(missing_data_set) {
  imputed <- missCompare::impute_data(missing_data_set, scale = TRUE, n.iter = 10,
                                      sel_method = 11)
  data.frame(imputed[['mice_mixed_imputation']])
}

#' \strong{rmiMAE} imputation.
#'
#' A function to replace \code{NA} in the data frame by outlier robust missing
#' imputation technique by minimizing twoway empirical mean absolute error.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by rmiMAE.
#' @export
#' @seealso \emph{doi: 10.2174/1574893612666171121154655}
#' @examples
#' \dontrun{
#' idf <- matrix(rnorm(1000), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_rmiMAE(idf)
#' }
#'
impute_rmiMAE <- function(missing_data_set) {
  imputed <- rmiMAE(as.matrix(missing_data_set), contRate = 99)
  data.frame(imputed[['x']])
}

#' \strong{RegImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by imputation using Glmnet
#' ridge regression (RegImpute) from [DreamAI::DreamAI()].
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by RegImpute
#' @export
#' @seealso [DreamAI::DreamAI()]
#' @examples
#' \dontrun{
#' idf <- matrix(rnorm(1000), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_RegImpute(idf)
#' }
#'
impute_RegImpute <- function(missing_data_set) {
  imputed <- DreamAI::DreamAI(missing_data_set, k = 10, maxiter_MF = 10, ntree = 100,
                     maxnodes = NULL, maxiter_ADMIN = 30, tol = 10^(-2),
                     gamma_ADMIN = NA, gamma = 50, CV = FALSE,
                     fillmethod = "row_mean", maxiter_RegImpute = 10,
                     conv_nrmse = 1e-06, iter_SpectroFM = 40,
                     method = c("RegImpute"), out = c("Ensemble"))
  data.frame(imputed[['Ensemble']])
}
