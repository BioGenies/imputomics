eval_pcaMethods_calls <- function(missdf, method, ...) {
  check_missdf(missdf)
  
  all_args <- extend_arglist(list(...), list(object = missdf, method = method), list(verbose = FALSE))
  
  imputed <- do.call(pcaMethods::pca, all_args)
  
  data.frame(pcaMethods::completeObs(imputed))
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
#' @inheritParams impute_zero
#' @param ... other parameters of [pcaMethods::pca()] besides \code{method} and
#' \code{object}.
#'
#' @section Silent verbose:
#' If \code{verbose} is not defined in the function call, it is set to 
#' \code{FALSE}.
#' 
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()]
#' with method = "svdImpute".
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' data(sim_miss)
#' impute_svd(sim_miss)
#'
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export
impute_svd <- function(missdf, ...) 
  eval_pcaMethods_calls(missdf, method = "svdImpute", ...)


#' \strong{PPCA} imputation.
#'
#' Probabilistic Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()] with
#' method = "ppca".
#'
#' @inheritParams impute_zero
#' @inheritParams impute_svd
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()] with
#' method = "ppca".
#' 
#' @inheritSection impute_svd Silent verbose
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' data(sim_miss)
#' impute_ppca(sim_miss)
#'
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export
impute_ppca <- function(missdf, ...) 
  eval_pcaMethods_calls(missdf, method = "ppca", ...)


#' \strong{BPCA} imputation.
#'
#' Bayesian Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()]
#' with method = "bpca". 
#'
#' @inheritParams impute_zero
#' @inheritParams impute_svd
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()]
#' with method = "bpca".
#' 
#' @inheritSection impute_svd Silent verbose
#' 
#' @section Silent maxSteps: 
#' 
#' If \code{maxSteps} is not declared, it is automatically
#' set to 500 (in contrast to default 100).
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' data(sim_miss)
#' impute_bpca(sim_miss)
#' 
#' # bring back the default maximum number of steps
#' impute_bpca(sim_miss, maxSteps = 100)
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export
impute_bpca <- function(missdf, ...) {
  all_args <- extend_arglist(list(...), list(missdf = missdf, method = "bpca"), list(maxSteps = 500))
  
  do.call(eval_pcaMethods_calls, all_args)
}


#' \strong{NIPALS} imputation.
#'
#' Nonlinear Iterative Partial Least Squares.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()]
#' with method = "nipals".
#'
#' @inheritParams impute_zero
#' @inheritParams impute_svd
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()]
#' with method = "nipals".
#' 
#' @inheritSection impute_svd Silent verbose
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' data(sim_miss)
#' impute_nipals(sim_miss)
#'
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export
impute_nipals <- function(missdf, ...) 
  eval_pcaMethods_calls(missdf, method = "nipals", ...)
