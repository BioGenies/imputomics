#devtools::install_version("PEMM", version = "1.0", repos = "http://cran.us.r-project.org")

# source("R/Trunc_KNN.R")
# source("R/GSimp_clean.R")
# source('R/rmiMAE.R')

#' Helper function.
#' Checks if provided input is right
#'
#' @param missing_data_set a data frame with missing values to be imputed
#' containing features in columns and samples in rows.
#'
#' @keywords internal

impute_constant <- function(missing_data_set, constant_value) {
  missing_data_set[is.na(missing_data_set)] <- constant_value
  missing_data_set
}

#' Helper function.
#' Imputes constant value.
#'
#' @param missing_data_set a matrix or data frame with missing values to be
#' imputed containing features in columns and samples in rows.
#'
#' @param constant_value a constant value to impute
#'
#' @keywords internal

impute_constant <- function(missing_data_set, constant_value) {
  missing_data_set[is.na(missing_data_set)] <- constant_value
  missing_data_set
}


#' \strong{zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by  zeros (0).
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{zero} method.
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_zero(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_zero <- function(missing_data_set)
  impute_constant(missing_data_set, constant_value = 0)


#' Helper function.
#' Basic imputation of missing values into dataframe.
#' Zero, minimum, half-minimum, mean, median and random.
#'
#' @inheritParams impute_constant
#'
#' @param compute_values one of the basic imputation methods
#'
#' @keywords internal

impute_per_column <- function(missing_data_set, compute_values) {
  imputed_values_vector <- compute_values(missing_data_set)

  do.call(cbind, lapply(1L:ncol(missing_data_set), function(ith_column_id) {
    impute_constant(missing_data_set[ith_column_id],
                    imputed_values_vector[[ith_column_id]])
  }))
}

#' Helper function. Random imputation.
#'
#' @param x list with values.
#'
#' @keywords internal

compute_col_random <- function(x)
  lapply(x, function(ith_col) {
    id_nas <- is.na(ith_col)
    sample(x = ith_col[!id_nas], size = sum(id_nas), replace = TRUE)
  })

#' Helper function. Minimum imputation.
#'
#' @inheritParams compute_col_random
#'
#' @keywords internal

compute_col_min <- function(x)
  lapply(x, min, na.rm = TRUE)

#' Helper function. Mean imputation.
#' @inheritParams compute_col_random
#'
compute_col_mean <- function(x)
  lapply(x, mean, na.rm = TRUE)

#' Helper function. Half-minimum imputation.
#'
#' @inheritParams compute_col_random
#'
#' @keywords internal

compute_col_halfmin <- function(x)
  lapply(x, function(i) min(i, na.rm = TRUE)/2)

#' Helper function. Median imputation.
#'
#' @inheritParams compute_col_random
#'
#' @keywords internal

compute_col_median <- function(x)
  lapply(x, mean, na.rm = TRUE)


#' \strong{minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by minimum values.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{minimum} method.
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_min(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_min <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_min)


#' \strong{mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by  mean values.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{mean} method.
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_mean(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_mean <- function(missing_data_set) {
  missing_data_set <- data.frame(missing_data_set)
  impute_per_column(missing_data_set, compute_col_mean)

}


#' \strong{half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by the half minimum values.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{half-minimum}
#' method.
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_halfmin(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_halfmin <- function(missing_data_set) {
  missing_data_set <- data.frame(missing_data_set)
  impute_per_column(missing_data_set, compute_col_halfmin)
}


#' \strong{median} imputation.
#'
#' A function to replace \code{NA} in the data frame by a median values.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{median} method.
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_median(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_median <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_median)


#' Convert an imputing function into its safe version.
#'
#' @inheritParams impute_constant
#'
#' @param imputing_function a function (imputation method) that takes
#' missing_data_set as an input
#'
#' @return A \code{data.frame} with imputed values or the
#' \strong{missing_data_set} if the imputing function failed to converge.
#'
#' @keywords internal

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
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()]
#' with method = "svdImpute".
#'
#' @importFrom pcaMethods pca
#' @importFrom pcaMethods completeObs
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()]
#' with method = "svdImpute".
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' \dontrun{
# idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
# values2 = c(21, 32, 48, NA, 59))
# impute_svd(idf)
#' }
#'
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export

impute_svd <- function(missing_data_set) { # TODO:: sprawdzic czy to nie wymaga transpozycji
  imputed <- pcaMethods::pca(missing_data_set,
                             method = "svdImpute",
                             verbose = FALSE)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}

#' \strong{PPCA} imputation.
#'
#' Probabilistic Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()] with
#' method = "ppca".
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()] with
#' method = "ppca".
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#'
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_ppca(idf)
#' }
#'
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export

impute_ppca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set,
                             method = "ppca",
                             verbose = FALSE,
                             maxIterations = 1e5)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{BPCA} imputation.
#'
#' Bayesian Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()]
#' with method = "bpca".
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()]
#' with method = "bpca".
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' \dontrun{
# idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
# values2 = c(21, 32, 48, NA, 59))
# impute_bpca(idf)
#' }
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export

impute_bpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set,
                             method = "bpca",
                             verbose = FALSE,
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{NIPALS} imputation.
#'
#' Nonlinear Iterative Partial Least Squares.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()]
#' with method = "nipals".
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()]
#' with method = "nipals".
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_nipals(idf)
#' }
#'
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export

impute_nipals <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set,
                             method = "nipals",
                             verbose = FALSE)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}



#' \strong{missMDA EM} imputation.
#'
#' PCA method with EM argument.
#'
#' A function to replace \code{NA} in the data frame by [missMDA::imputePCA()]
#' with method = "em".
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [missMDA::imputePCA()]
#' with method = "em".
#'
#' @seealso [missMDA::imputePCA()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missmda_em(idf)
#' }
#'
#' @references
#' \insertRef{josse_missmda_2016}{imputomics}
#'
#' @export

impute_missmda_em <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set,
                                method = "EM")
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
#' @importFrom mice mice
#' @importFrom mice quickpred
#' @importFrom mice mice.impute.pmm
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by pmm used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.pmm()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_pmm(idf)
#' }
#'
#' @references
#' \insertRef{buuren_mice_2011}{imputomics}
#'
#' @export

impute_mice_pmm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'pmm',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE cart} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by classification and
#' regression trees (cart) used [mice::mice()].
#'
#' @importFrom mice mice.impute.cart
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by cart used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.cart()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_cart(idf)
#' }
#'
#' @export

impute_mice_cart <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'cart',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}

#' \strong{MICE rf} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by random forest
#' imputations (rf) used [mice::mice()].
#'
#' @importFrom mice mice.impute.rf
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by rf used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.rf()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_rf(idf)
#' }
#'
#' @export

impute_mice_rf <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'rf',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}



#' \strong{Amelia} imputation.
#'
#' A function to replace \code{NA} in the data frame by [Amelia::amelia()].
#'
#' @importFrom Amelia amelia
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [Amelia::amelia()].
#'
#' @seealso [Amelia::amelia()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_amelia(idf)
#' }
#'
#' @references
#' \insertRef{honaker_amelia_2011}{imputomics}
#'
#' @export

impute_amelia <- function(missing_data_set) {
  capture.output(imputed <- Amelia::amelia(missing_data_set, m = 1))
  imputed[["imputations"]][["imp1"]]
}


#' \strong{MissForest} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [missForest::missForest()].
#'
#' @importFrom missForest missForest
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by
#' [missForest::missForest()].
#'
#' @seealso [missForest::missForest()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missforest(idf)
#' }
#'
#' @references
#' \insertRef{stekhoven_missforest_2012}{imputomics}
#'
#' @export

impute_missforest <- function(missing_data_set) {
  imputed <- missForest::missForest(missing_data_set,
                                    maxiter = 10,
                                    ntree = 500,
                                    replace = TRUE)
  imputed[["ximp"]]
}



#' \strong{Hmisc areg} imputation.
#'
#' Multiple Imputation using Additive Regression, Bootstrapping, and Predictive
#' Mean Matching.
#'
#' A function to replace \code{NA} in the data frame by [Hmisc::aregImpute()].
#'
#' @importFrom Hmisc aregImpute
#' @importFrom Hmisc impute.transcan
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [Hmisc::aregImpute()].
#'
#' @seealso [Hmisc::aregImpute()]
#'
#' @references
#' \insertRef{jr_hmisc_2023}{imputomics}
#'
#' @export

impute_areg <- function(missing_data_set) {
  capture.output(imputed <- Hmisc::aregImpute(
    formula = as.formula(paste0("~ ", paste0(colnames(missing_data_set),
                                             collapse = " + "))),
    data = missing_data_set,
    tlinear = FALSE)
  )
  data.frame(do.call(cbind,
                     Hmisc::impute.transcan(imputed,
                                            imputation = 1,
                                            data = missing_data_set,
                                            list.out = TRUE,
                                            pr = FALSE,
                                            check = FALSE)))
}


#' \strong{kNN} imputation.
#'
#' K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by [impute::impute.knn()].
#'
#' @importFrom impute impute.knn
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [impute::impute.knn()].
#'
#' @seealso [impute::impute.knn()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' set.seed(2137)
#' impute_knn(idf)
#' }
#'
#' @references
#' \insertRef{hastie_impute_2023}{imputomics}
#'
#' @export

impute_knn <- function(missing_data_set) {
  # this function has a default random seed, so we need to sample one
  imputed <- impute::impute.knn(as.matrix(missing_data_set),
                                k = 10,
                                rng.seed = sample(1L:1e9, 1))
  data.frame(imputed[["data"]])
}


#' \strong{QRLIC} imputation.
#'
#' Quantile Regression Imputation Of Left-Censored Data.
#'
#' A function to replace \code{NA} in the data frame by
#' [imputeLCMD::impute.QRILC()].
#'
#' @importFrom imputeLCMD impute.QRILC
#'
#' @importFrom imputeLCMD impute.QRILC
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by
#' [imputeLCMD::impute.QRILC()].
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
#' @references
#' \insertRef{lazar_imputelcmd_2022}{imputomics}
#'
#' @export
#'
impute_qrilc <- function(missing_data_set) {
  imputeLCMD::impute.QRILC(missing_data_set)[[1]]
}


#' \strong{tWLSA} imputation.
#'
#' Two Way Weighted Least Square Approach.
#'
#' A function to replace \code{NA} in the data frame by [tWLSA::wlsMisImp()].
#'
#' @importFrom tWLSA wlsMisImp
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [tWLSA::wlsMisImp()].
#'
#' @seealso [tWLSA::wlsMisImp()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_twlsa(idf)
#' }
#'
#' @references
#' \insertRef{kumar_kernel_2021}{imputomics}
#'
#' @export

impute_twlsa <- function(missing_data_set) {
  imputed <- tWLSA::wlsMisImp(as.matrix(missing_data_set))
  data.frame(imputed)
}


#' \strong{SoftImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [softImpute::softImpute()].
#'
#' @importFrom softImpute softImpute
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by
#' [softImpute::softImpute()].
#'
#' @seealso [softImpute::softImpute()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_softimpute(idf)
#' }
#'
#' @references
#' \insertRef{mazumder_softimpute_2021}{imputomics}
#'
#' @export

impute_softimpute <- function(missing_data_set) {
  fit <- softImpute::softImpute(as.matrix(missing_data_set))
  data.frame(softImpute::complete(as.matrix(missing_data_set), fit))
}




#' \strong{PEMM} imputation.
#'
#' Penalized EM Algorithm.
#'
#' A function to replace \code{NA} in the data frame by [PEMM::PEMM_fun()].
#'
#' @importFrom PEMM PEMM_fun
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [PEMM::PEMM_fun()].
#'
#' @seealso [PEMM::PEMM_fun()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_PEMM(as.matrix(idf))
#' }
#'
#' @references
#' \insertRef{chen_penalized_2014}{imputomics}
#'
#' @export

impute_PEMM <- function(missing_data_set) {
  #PEMM_fun requires matrix
  missing_data_set <- as.matrix(missing_data_set)
  imputed <- PEMM::PEMM_fun(missing_data_set,
                            phi = 1)
  data.frame(imputed[["Xhat"]])
}


#' \strong{tkNN} imputation.
#'
#' Truncated K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by \strong{tkNN} method.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{tkNN} method.
#'
#' @details This function was copied from https://github.com/WandeRum/GSimp and
#' contains kNN-TN algorithm and related functions developed by Jasmit S. Shah
#' (https://doi.org/10.1186/s12859-017-1547-6).
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_tknn(idf)
#' }
#'
#' @references
#' \insertRef{shah_distribution_2017}{imputomics}
#'
#' @export
#'
#'

impute_tknn <- function(missing_data_set) {
  imputed <- imputeKNN(as.matrix(missing_data_set),
                       k = ceiling(nrow(missing_data_set)*0.05) + 1,
                       distance = "truncation",
                       rm.na = TRUE,
                       rm.nan = FALSE,
                       rm.inf = FALSE)
  data.frame(imputed)
}

#' \strong{corkNN} imputation.
#'
#' Correlation K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by \strong{corkNN} method.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{corkNN} method.
#'
#' @details This function was copied from https://github.com/WandeRum/GSimp and
#' contains kNN-TN algorithm and related functions developed by Jasmit S. Shah
#' (https://doi.org/10.1186/s12859-017-1547-6).
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_tknn(idf)
#' }
#'
#' @export
impute_corknn <- function(missing_data_set) {
  imputed <- imputeKNN(as.matrix(missing_data_set),
                       k = ceiling(nrow(missing_data_set)*0.05) + 1,
                       distance = "correlation",
                       rm.na = TRUE,
                       rm.nan = FALSE,
                       rm.inf = FALSE)
  data.frame(imputed)
}


#' \strong{GSimp} imputation.
#'
#' Gibbs Sampler Based Left-Censored Missing Value Imputation.
#'
#' A function to replace \code{NA} in the data frame by \strong{GSimp} method.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{GSimp} method.
#'
#' @details  This function was copied from https://github.com/WandeRum/GSimp and
#' contains the GSimp algorithm and related functions developed by Rum Wei
#' (10.1371/journal.pcbi.1005973).
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_gsimp(idf)
#' }
#'
#' @references
#' \insertRef{wei_gsimp_2018}{imputomics}
#'
#' @export

impute_gsimp <- function(missing_data_set) {
  imputed <- GS_impute_clean(missing_data_set,
                             initial = "lsym",
                             imp_model = 'glmnet_pred')
  data.frame(imputed[["data_imp"]])
}

#' \strong{kNN} imputation.
#'
#' K Nearest Neighbors. A function to replace \code{NA} in the data frame by
#' [VIM::kNN()].
#'
#' @importFrom VIM kNN
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [VIM::kNN()].
#'
#' @seealso [VIM::kNN()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_vim_knn(idf)
#' }
#'
#' @references
#' \insertRef{kowarik_imputation_2016}{imputomics}
#'
#' @export
#'
impute_vim_knn <- function(missing_data_set) {
  imputed <- VIM::kNN(missing_data_set, k = 10)[,1:ncol(missing_data_set)]
  data.frame(imputed)
}


#' \strong{MetabImpute RF} imputation.
#'
#' A function to replace \code{NA} in the data frame using random forest.
#'
#' @importFrom MetabImpute imputeMulti
#' @importFrom MetabImpute Impute
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [MetabImpute::Impute()]
#' or [MetabImpute::imputeMulti()].
#'
#' @seealso [MetabImpute::Impute()], [MetabImpute::imputeMulti()]]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_RF(idf)
#' }
#'
#' @export
#'
impute_MetabImpute_RF <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RF',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute BPCA} imputation.
#'
#' A function to replace \code{NA} in the data frame by Bayesian PCA.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of Bayesian PCA by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_BPCA(idf)
#' }
#'
#' @export

impute_MetabImpute_BPCA <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'BPCA',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute QRILC} imputation.
#'
#' A function to replace \code{NA} in the data frame by QRILC.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of QRILC by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_QRILC(idf)
#' }
#'
#' @export

impute_MetabImpute_QRILC <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'QRILC',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute GSIMP} imputation.
#'
#' A function to replace \code{NA} in the data frame by GSIMP.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of GSIMP by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_GSIMP(idf)
#' }
#'
#' @export

impute_MetabImpute_GSIMP <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'GSIMP',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by minimum.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of minimum by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_min(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_min <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'min',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by half-minimum from.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of half-minimum by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_halfmin(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_halfmin <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'halfmin',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate half-minimum.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate half-minimum
#' by [MetabImpute::Impute()]
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rhalfmin(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_rhalfmin <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RHM',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by mean.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of mean by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_mean(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_mean <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'mean',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute median} imputation.
#'
#' A function to replace \code{NA} in the data frame by median.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of median by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_median(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_median <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'median',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by zero.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of zero by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_zero(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_zero <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'zero',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate mean.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate mean by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rmean(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_rmean <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RMEAN',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate median} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate median.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate median by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rmedian(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_rmedian <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RMEDIAN',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate min} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate min.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate min by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rmin(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_rmin <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RMIN',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate zero.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate zero by
#' [MetabImpute::Impute()].\
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rzero(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_rzero <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RZERO',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate Random Forest} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate Random Forest.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate Random Forest
#' by [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rrf(idf)
#' }
#'
#' @export

impute_MetabImpute_rrf <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RRF',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate GSIMP} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate GSIMP.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate GSIMP by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rGSIMP(idf)
#' }
#'
#' @export

impute_MetabImpute_rGSIMP <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RGSIMP',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate QRILC} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate QRILC.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate QRILC by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rGSIMP(idf)
#' }
#'
#' @export

impute_MetabImpute_rQRILC <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RQRILC',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}

#' \strong{MetabImpute replicate BPCA} imputation.
#'
#' A function to replace \code{NA} in the data frame by replicate BPCA.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values of replicate BPCA by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_rBPCA(idf)
#' }
#'
#' @export

impute_MetabImpute_rBPCA <- function(missing_data_set) {
  imputed <- MetabImpute::Impute(data = missing_data_set,
                                 method = 'RBPCA',
                                 reps = 5,
                                 local = TRUE)
  data.frame(imputed)
}


#' \strong{Mechanism-Aware} imputation.
#'
#' A function to replace \code{NA} in the data frame by [MAI::MAI()] with random
#' forest and single imputation.
#'
#' @importFrom MAI MAI
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [MAI::MAI()].
#'
#' @seealso [MAI::MAI()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(10000) < 0.1] <- NA
#' impute_MA(idf)
#' }
#'
#' @references
#' \insertRef{dekermanjian_mechanismaware_2022}{imputomics}
#'
#' @export

impute_MA <- function(missing_data_set) {
  imputed <- MAI::MAI(missing_data_set,
                      MCAR_algorithm = 'random_forest',
                      MNAR_algorithm = 'Single')
  data.frame(imputed[["Imputed_data"]])
}


#' \strong{kNN-Euclidean} imputation.
#'
#' A function to replace \code{NA} in the data frame based on
#' \emph{Jasmit S. Shah (https://doi.org/10.1186/s12859-017-1547-6)}.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by kNN-Euclidean imputation.
#'
#' @details This function was copied from https://github.com/WandeRum/GSimp and
#' contains kNN-TN algorithm and related functions developed by Jasmit S. Shah
#' (https://doi.org/10.1186/s12859-017-1547-6).
#'
#' @seealso \emph{Jasmit S. Shah (https://doi.org/10.1186/s12859-017-1547-6)}
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(10000) < 0.1] <- NA
#' impute_eucknn(idf)
#' }
#'
#' @references
#' \insertRef{shah_distribution_2017}{imputomics}
#'
#' @export

impute_eucknn <- function(missing_data_set) {
  imputed <- KNNEuc(as.matrix(missing_data_set),
                    k = ceiling(nrow(missing_data_set)*0.05) + 1,
                    rm.na = TRUE,
                    rm.nan = TRUE,
                    rm.inf = TRUE)
  data.frame(imputed)
}

#' \strong{mice mixed} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [missCompare::impute_data()].
#'
#' @importFrom missCompare impute_data
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by
#' [missCompare::impute_data()].
#'
#' @seealso [missCompare::impute_data()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_mice_mixed(idf)
#' }
#'
#' @export

impute_mice_mixed <- function(missing_data_set) {
  imputed <- missCompare::impute_data(missing_data_set,
                                      scale = TRUE,
                                      n.iter = 10,
                                      sel_method = 11)
  data.frame(imputed[['mice_mixed_imputation']])
}

#' \strong{rmiMAE} imputation.
#'
#' A function to replace \code{NA} in the data frame by outlier robust missing
#' imputation technique by minimizing two-way empirical mean absolute error.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by rmiMAE.
#'
#' @details Robust Missing imputation by minimizing two way mean absolute error
#' (RMIMAE) Function URL:
#' https://github.com/NishithPaul/missingImputation/blob/main/rmiMAE.R
#' Author: NishithPaul
#'
#' @seealso \emph{doi: 10.2174/1574893612666171121154655}
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_rmiMAE(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_rmiMAE <- function(missing_data_set) {
  imputed <- rmiMAE(as.matrix(missing_data_set), contRate = 99)
  data.frame(imputed[['x']])
}

#' \strong{RegImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by imputation using Glmnet
#' ridge regression (RegImpute) from [DreamAI::DreamAI()].
#'
#' @importFrom DreamAI DreamAI
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by RegImpute
#'
#' @seealso [DreamAI::DreamAI()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_RegImpute(idf)
#' }
#'
#' @export

impute_RegImpute <- function(missing_data_set) {
  imputed <- DreamAI::DreamAI(missing_data_set,
                              k = 10,
                              maxiter_MF = 10,
                              ntree = 100,
                              maxnodes = NULL,
                              maxiter_ADMIN = 30,
                              tol = 10^(-2),
                              gamma_ADMIN = NA,
                              gamma = 50,
                              CV = FALSE,
                              fillmethod = "row_mean",
                              maxiter_RegImpute = 10,
                              conv_nrmse = 1e-06,
                              iter_SpectroFM = 40,
                              method = c("RegImpute"),
                              out = c("Ensemble"))
  data.frame(imputed[['Ensemble']])
}


#' \strong{Singular Value Decomposition - SVD} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [bcv::impute.svd()].
#'
#' @importFrom bcv impute.svd
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by SVD.
#'
#' @seealso [bcv::impute.svd()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_bcv_svd(idf)
#' }
#'
#' @export

impute_bcv_svd <- function(missing_data_set){
  imputed <- bcv::impute.svd(missing_data_set)
  data.frame(imputed[['x']])
}


#' \strong{kNN} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [imputation::kNNImpute()].
#'
#' @importFrom imputation kNNImpute
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by kNN.
#'
#' @seealso [imputation::kNNImpute()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_imputation_kNN(idf)
#' }
#'
#' @export

impute_imputation_kNN <- function(missing_data_set){
  # kNNImpute needs data to be a matrix
  missing_data_set <- as.matrix(missing_data_set)
  imputed <- imputation::kNNImpute(missing_data_set,
                                   k = min(nrow(missing_data_set),
                                           ncol(missing_data_set),
                                           11) - 1)
  data.frame(imputed[['x']])
}

#' \strong{Metabolomic Non-negative Matrix Factorization - mNMF} imputation.
#'
#' A function to replace \code{NA} in the data frame based on
#' \emph{Jingjing Xu (https://doi.org/10.3390/molecules26195787)}.
#'
#' @importFrom NMF nmf.getOption
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by mNMF.
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_mNMF(idf)
#'}
#' @export

impute_mNMF <- function(missing_data_set){

  # samples in columns and features in rows
  missing_data_set <- t(missing_data_set)
  k_group <- unique(round(seq(1,
                              min(ncol(missing_data_set),
                                  nrow(missing_data_set)),
                              length.out = min(20, ncol(missing_data_set))), 0))
  imputed <- nmf_opt(IMP = missing_data_set,
                     M = NMF::nmf.getOption('default.algorithm'),
                     kgroup = k_group,
                     initialType = "mean")
  data.frame(t(imputed))
}



#' \strong{Compound Minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by [GMSimpute::GMS.Lasso()]
#'
#' @inheritParams impute_constant
#' @importFrom GMSimpute GMS.Lasso
#'
#' @returns A \code{data.frame} with imputed values by CM.
#'
#' @seealso [GMSimpute::GMS.Lasso()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_CM(idf)
#'}
#'
#' @keywords constant
#'
#' @export

impute_CM <- function(missing_data_set){
  # samples in columns and features in rows
  missing_data_set <- t(missing_data_set)
  imputed <- GMSimpute::GMS.Lasso(missing_data_set, TS.Lasso = FALSE)
  data.frame(imputed)
}



#' \strong{BayesMetab} imputation.
#'
#' A function to replace \code{NA} in the data frame based on
#' \emph{Jasmit Shah (10.1186/s12859-019-3250-2)}.
#'
#' @importFrom SimDesign rmvnorm
#' @importFrom truncnorm rtruncnorm
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by BayesMetab
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_CM(idf)
#'}
#' @export

impute_BayesMetab <- function(missing_data_set){
  # MCMC.Factor requires matrix
  missing_data_set <- as.matrix(missing_data_set)
  imputed <- MCMC.Factor(missing_data_set,
                         M = 100,
                         miss.pattern = !is.na(missing_data_set),
                         K.max = ncol(missing_data_set))
  data.frame(imputed[[5]])
}

