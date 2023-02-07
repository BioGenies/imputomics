#devtools::install_version("PEMM", version = "1.0", repos = "http://cran.us.r-project.org")

source("R/Trunc_KNN.R")
source("R/GSimp_clean.R")



#' @template param_missing_ds
#' @param constant_value a constant value to impute
impute_constant <- function(missing_data_set, constant_value) {
  missing_data_set[is.na(missing_data_set)] <- constant_value
  missing_data_set
}


#' Imputes missing values into a data frame with \strong{zero} method.
#'
#' Replaces \code{NA} with zeros (0).
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{zero} method.
#' @export
#' @examples
#'
#'
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


#' Imputes missing values into a data frame with \strong{random} method.
#'
#' Replaces \code{NA} with random values.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{random} method.
#' @export
#' @examples
#'
#'
impute_random <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_random)


#' Imputes missing values into a data frame with \strong{minimum} method.
#'
#' Replaces \code{NA} with minimal value of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{minimum} method.
#' @export
#' @examples
#'
#'
impute_min <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_min)


#' Imputes missing values into a data frame with \strong{mean} method.
#'
#' Replaces \code{NA} with mean value of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{mean} method.
#' @export
#' @examples
#'
#'
impute_mean <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_mean)


#' Imputes missing values into a data frame with \strong{half-minimum} method.
#'
#' Replaces \code{NA} with the half of the minimum value of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{half-minimum} method.
#' @export
#' @examples
#'
#'
impute_halfmin <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_halfmin)


#' Imputes missing values into a data frame with \strong{median} method.
#'
#' Replaces \code{NA} with median of data frame.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{median} method.
#' @export
#' @examples
#'
#'
impute_median <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_median)

estimate_ncp <- function(missing_data_set) {
  estimated_ncp <- missMDA::estim_ncpPCA(missing_data_set, ncp.max = ncol(df) - 2)[["ncp"]]
  ifelse(estimated_ncp < 2, 2, estimated_ncp)
}

#' Convert an imputing function into its safe version.
#' @param imputing_function a function that takes missing_data_set as an input
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



#' Imputes missing values into a data frame with \strong{SVD} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods]{pca}} svdImpute method.
#' \strong{SVD} stands for Singular Value Decomposition.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods]{pca}} svdImpute method.
#' @export
#' @seealso \code{\link{pcaMethods}()}
#' @examples
#'
#'
impute_svd <- function(missing_data_set) { # sprawdzic czy to nie wymaga transpozycji
  imputed <- pcaMethods::pca(missing_data_set, method = "svdImpute",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' Imputes missing values into a data frame with \strong{PPCA} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods]{pca}} PPCA method.
#' \strong{PPCA} stands for Probabilistic Principal Component Analysis.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods]{pca}} PPCA method.
#' @export
#' @seealso \code{\link{pcaMethods}()}
#' @examples
#'
#'
impute_ppca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "ppca",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxIterations = 1e5)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' Imputes missing values into a data frame with \strong{BPCA} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods]{pca}} BPCA method.
#' \strong{BPCA} stands for Bayesian Principal Component Analysis.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods]{pca}} BPCA method.
#' @export
#' @seealso \code{\link{pcaMethods}()}
#' @examples
#'
#'
impute_bpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "bpca",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' Imputes missing values into a data frame with \strong{NIPALS} method.
#'
#' \strong{NIPALS} stands for Nonlinear Iterative Partial Least Squares.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods]{pca}} NIPALS method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods]{pca}} NIPALS method.
#' @export
#' @seealso \code{\link{pcaMethods}()}
#' @examples
#'
#'
impute_nipals <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "nipals",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none")
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' Imputes missing values into a data frame with \strong{NLPCA} method.
#' \strong{NLPCA} stands for Nonlinear Principal Component Analysis.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[pcaMethods]{pca}} NLPCA method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[pcaMethods]{pca}} NLPCA method.
#' @export
#' @seealso \code{\link{pcaMethods}()}
#' @examples
#'
#'
impute_nlpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set, method = "nlpca",
                             nPcs = estimate_ncp(missing_data_set),
                             verbose = FALSE, center = FALSE, scale = "none",
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' Imputes missing values into a data frame with \strong{missMDA regularized} method.
#' Function for imputation uses PCA method with regularized argument.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[missMDA]{imputePCA}} regularized method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[missMDA]{imputePCA}} regularized method.
#' @export
#' @seealso \code{\link{missMDA}()}
#' @examples
#'
#'
impute_missmda_reg <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
                                method = "Regularized", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}


#' Imputes missing values into a data frame with \strong{missMDA EM} method.
#'
#' Function for imputation uses PCA method with EM argument.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[missMDA]{imputePCA}} EM method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[missMDA]{imputePCA}} EM method.
#' @export
#' @seealso \code{\link{missMDA}()}
#' @examples
#'
#'
impute_missmda_em <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set, ncp = estimate_ncp(missing_data_set),
                                method = "EM", scale = FALSE)
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}


#' Imputes missing values into a data frame with \strong{MICE} method.
#' \strong{MICE}  stands for Multiple Imputation by Chained Equations.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[mice]{mice}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[mice]{mice}}.
#' @export
#' @seealso \code{\link{mice}()}
#' @examples
#'
#'
impute_mice <- function(missing_data_set, method) {
  imputed <- mice::mice(missing_data_set, method = method, m = 1, maxit = 100, printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}


#' Imputes missing values into a data frame with \strong{Amelia} method.
#' \strong{Amelia} is an R package for the multiple imputation.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[Amelia]{amelia}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[Amelia]{amelia}}.
#' @export
#' @seealso \code{\link{Amelia}()}
#' @examples
#'
#'
impute_amelia <- function(missing_data_set) {
  capture.output(imputed <- Amelia::amelia(missing_data_set, m = 1))
  imputed[["imputations"]][["imp1"]]
}


#' Imputes missing values into a data frame with \strong{MissForest} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[missForest]{missForest}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[missForest]{missForest}}.
#' @export
#' @seealso \code{\link{missForest}()}
#' @examples
#'
#'
impute_missforest <- function(missing_data_set) {
  imputed <- missForest::missForest(missing_data_set, maxiter = 10, ntree = 500, replace = TRUE)
  imputed[["ximp"]]
}


#' Imputes missing values into a data frame with \strong{mi} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[mi]{mi}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[mi]{mi}}.
#' @export
#' @seealso \code{\link{mi}()}
#' @examples
#'
#'
impute_mi <- function(missing_data_set) {
  # requires betareg
  capture.output(imputed <- mi::mi(missing_data_set, n.chain = 1,
                                   n.iter = 100, verbose = FALSE, parallel = FALSE))

  mi::complete(imputed)[colnames(missing_data_set)]

}


#' Imputes missing values into a data frame with \strong{Hmisc areg} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[Hmisc]{aregImpute}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[Hmisc]{aregImpute}}.
#' @export
#' @seealso \code{\link{Hmisc}()}
#' @examples
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


#' Imputes missing values into a data frame with \strong{kNN} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[impute]{impute.knn}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[impute]{impute.knn}}.
#' @export
#' @seealso \code{\link{impute}()}
#' @examples
#'
#'
impute_knn <- function(missing_data_set) {
  # this function has a default random seed, so we need to sample one
  imputed <- impute::impute.knn(as.matrix(missing_data_set), k = 10,
                                rng.seed = sample(1L:1e9, 1))
  data.frame(imputed[["data"]])
}


#' Imputes missing values into a data frame with \strong{QRLIC} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[imputeLCMD]{impute.QRILC}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[imputeLCMD]{impute.QRILC}}.
#' @export
#' @seealso \code{\link{imputeLCMD}()}
#' @examples
#'
#'
impute_qrilc <- function(missing_data_set) {
  imputeLCMD::impute.QRILC(missing_data_set)[[1]]
}


#' Imputes missing values into a data frame with \strong{MLE} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[imputeLCMD]{impute.wrapper.MLE}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[imputeLCMD]{impute.wrapper.MLE}}.
#' @export
#' @seealso \code{\link{imputeLCMD}()}
#' @examples
#'
#'
impute_mle <- function(missing_data_set) {
  imputed <- imputeLCMD::impute.wrapper.MLE(as.matrix(missing_data_set))
  data.frame(imputed)
}


#' Imputes missing values into a data frame with \strong{tWLSA} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[tWLSA]{wlsMisImp}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[tWLSA]{wlsMisImp}}.
#' @export
#' @seealso \code{\link{tWLSA}()}
#' @examples
#'
#'
impute_twlsa <- function(missing_data_set) {
  imputed <- tWLSA::wlsMisImp(as.matrix(missing_data_set))
  data.frame(imputed)
}


#' Imputes missing values into a data frame with \strong{SoftImpute} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[softImpute]{softImpute}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[softImpute]{softImpute}}.
#' @export
#' @seealso \code{\link{softImpute}()}
#' @examples
#'
#'
impute_softimpute <- function(missing_data_set) {
  fit <- softImpute::softImpute(as.matrix(missing_data_set))
  data.frame(softImpute::complete(as.matrix(missing_data_set), fit))
}


#' Imputes missing values into a data frame with \strong{IRMI} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[NADIA]{autotune_VIM_Irmi}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[NADIA]{autotune_VIM_Irmi}}.
#' @export
#' @seealso \code{\link{NADIA}()}
#' @examples
#'
#'
impute_irmi <- function(missing_data_set) {
  NADIA::autotune_VIM_Irmi(missing_data_set, col_type = rep("numeric", ncol(missing_data_set)),
                           percent_of_missing = colMeans(is.na(missing_data_set))*100, maxit = 200)
}


#' Imputes missing values into a data frame with \strong{PEMM} method.
#'
#' Replaces \code{NA} values in the data frame using \code{\link[PEMM]{PEMM_fun}}.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \code{\link[PEMM]{PEMM_fun}}.
#' @export
#' @seealso \code{\link{PEMM}()}
#' @examples
#'
#'
impute_PEMM <- function(missing_data_set) {
  PEMM::PEMM_fun(missing_data_set, phi = 1)
}


#' Imputes missing values into a data frame with \strong{tkNN} method.
#'
#' Replaces \code{NA} values in the data frame using \strong{tkNN} method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{tkNN} method.
#' @export
#' @examples
#'
#'
impute_tknn <- function(missing_data_set) {
  imputed <- imputeKNN(as.matrix(missing_data_set), k = ceiling(nrow(missing_data_set)*0.05) + 1, distance = "truncation",
            rm.na = TRUE, rm.nan = FALSE, rm.inf = FALSE)
  data.frame(imputed)
}


#' Imputes missing values into a data frame with \strong{GSimp} method.
#'
#' Replaces \code{NA} values in the data frame using \strong{GSimp} method.
#'
#' @template param_missing_ds
#' @returns A \code{data.frame} with imputed values by \strong{GSimp} method.
#' @export
#' @examples
#'
#'
impute_gsimp <- function(missing_data_set) {
  imputed <- GS_impute_clean(missing_data_set, initial = "lsym", imp_model='glmnet_pred')
  imputed[["data_imp"]]
}
