eval_MetabImpute_calls <- function(missdf, method, verbose, ...) {
  check_missdf(missdf)
  
  all_args <- extend_arglist(list(...), 
                             list(data = missdf, method = method, 
                                  local = FALSE, reps = NULL), 
                             list())
  
  silence_function(verbose)(imputed <- do.call(MetabImpute::Impute, all_args))
  
  imputed
}


#' \strong{MetabImpute RF} imputation.
#'
#' A function to replace \code{NA} in the data frame using random forest.
#'
#' @importFrom MetabImpute imputeMulti
#' @importFrom MetabImpute Impute
#'
#' @inheritParams impute_zero
#' @param verbose boolean, if \code{TRUE}, prints the typical prompts of 
#' [MetabImpute::Impute()].
#' @param ... other parameters of [MetabImpute::Impute()] besides \code{method} and
#' \code{data}.
#'
#' @returns A \code{data.frame} with imputed values by [MetabImpute::Impute()].
#'
#' @section No replicates allowed:
#' \code{Impute()} allows users to improve the quality of the imputation by 
#' providing the number of replications of the experiment. As one of the aims of 
#' the \code{imputomics} is to standardize the 
#' input and the output, our wrappers do not allow for this behavior.
#'
#' @seealso [MetabImpute::Impute()].
#' 
#' @references
#' \insertRef{davis_addressing_2022}{imputomics}
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_rf(sim_miss)
#'
#' @export
impute_metabimpute_rf <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "RF", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute BPCA} imputation.
#'
#' A function to replace \code{NA} in the data frame by Bayesian PCA.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#' 
#' @returns A \code{data.frame} with imputed values of Bayesian PCA by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_bpca(sim_miss)
#'
#' @export
impute_metabimpute_bpca <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "BPCA", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute QRILC} imputation.
#'
#' A function to replace \code{NA} in the data frame by QRILC.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#'
#' @returns A \code{data.frame} with imputed values of QRILC by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_qrilc(sim_miss)
#'
#' @export
impute_metabimpute_qrilc <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "QRILC", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute GSIMP} imputation.
#'
#' A function to replace \code{NA} in the data frame by GSIMP.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#'
#' @returns A \code{data.frame} with imputed values of GSIMP by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_gsimp(sim_miss)
#'
#' @export
impute_metabimpute_gsimp <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "GSIMP", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by minimum.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#'
#' @returns A \code{data.frame} with imputed values of minimum by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_min(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_metabimpute_min <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "min", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by half-minimum from.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#'
#' @returns A \code{data.frame} with imputed values of half-minimum by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_halfmin(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_metabimpute_halfmin <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "halfmin", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by mean.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#' 
#' @returns A \code{data.frame} with imputed values of mean by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_mean(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_metabimpute_mean <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "mean", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute median} imputation.
#'
#' A function to replace \code{NA} in the data frame by median.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#'
#' @returns A \code{data.frame} with imputed values of median by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_median(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_metabimpute_median <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "median", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by zero.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_metabimpute_rf
#' @inherit impute_metabimpute_rf references seealso
#' @inheritSection impute_metabimpute_rf No replicates allowed
#'
#' @returns A \code{data.frame} with imputed values of zero by
#' [MetabImpute::Impute()].
#'
#' @examples
#' data(sim_miss)
#' impute_metabimpute_zero(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_metabimpute_zero <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "zero", 
                         verbose = verbose, ...)
}
