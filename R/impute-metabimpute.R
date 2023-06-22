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
#' @examples
#' data(sim_miss)
#' impute_MetabImpute_rf(sim_miss)
#'
#' @export
impute_MetabImpute_rf <- function(missdf, verbose = FALSE, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "RF", 
                         verbose = verbose, ...)
}


#' \strong{MetabImpute BPCA} imputation.
#'
#' A function to replace \code{NA} in the data frame by Bayesian PCA.
#'
#' @inheritParams impute_zero
#' @inheritParams impute_MetabImpute_rf
#' 
#' @returns A \code{data.frame} with imputed values of Bayesian PCA by
#' [MetabImpute::Impute()].
#' 
#' @inheritSection impute_MetabImpute_rf No replicates allowed
#' 
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' data(sim_miss)
#' impute_MetabImpute_bpca(sim_miss)
#'
#' @export
impute_MetabImpute_bpca <- function(missdf, ...) {
  eval_MetabImpute_calls(missdf = missdf, method = "BPCA", 
                         verbose = verbose, ...)
}

#' \strong{MetabImpute QRILC} imputation.
#'
#' A function to replace \code{NA} in the data frame by QRILC.
#'
#' @inheritParams impute_zero
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

impute_MetabImpute_QRILC <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'QRILC',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute GSIMP} imputation.
#'
#' A function to replace \code{NA} in the data frame by GSIMP.
#'
#' @inheritParams impute_zero
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

impute_MetabImpute_GSIMP <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'GSIMP',
                                 reps = 1,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by minimum.
#'
#' @inheritParams impute_zero
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

impute_MetabImpute_min <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'min',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by half-minimum from.
#'
#' @inheritParams impute_zero
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

impute_MetabImpute_halfmin <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'halfmin',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by mean.
#'
#' @inheritParams impute_zero
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

impute_MetabImpute_mean <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'mean',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute median} imputation.
#'
#' A function to replace \code{NA} in the data frame by median.
#'
#' @inheritParams impute_zero
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

impute_MetabImpute_median <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'median',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by zero.
#'
#' @inheritParams impute_zero
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

impute_MetabImpute_zero <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'zero',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}
