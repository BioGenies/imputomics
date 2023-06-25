eval_mice_calls <- function(missdf, method, ...) {
  check_missdf(missdf)
  
  all_args <- extend_arglist(list(...), 
                             list(data = missdf, method = method), 
                             list(printFlag = FALSE, predictorMatrix = mice::quickpred(missdf)))
  
  imputed <- do.call(mice::mice, all_args)
  
  mice::complete(imputed)
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
#'
#' @inheritParams impute_zero
#' @param ... other parameters of [mice::mice()] besides \code{method} and 
#' \code{data}.
#' 
#' @section Silent defaults:
#' If \code{printFlag} is not defined in the function call, it is set to 
#' \code{FALSE}.
#' 
#' If \code{predictorMatrix} is not defined in the function call, it is set to 
#' [mice::quickpred].
#'
#' @returns A \code{data.frame} with imputed values by pmm used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.pmm()]
#'
#' @examples
#' data(sim_miss)
#' impute_mice_pmm(sim_miss)
#'
#' @references
#' \insertRef{buuren_mice_2011}{imputomics}
#'
#' @export
impute_mice_pmm <- function(missdf, ...) {
  eval_mice_calls(missdf = missdf, method = "pmm", ...)
}

#' \strong{MICE cart} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by classification and
#' regression trees (cart) used [mice::mice()].
#'
#' @inheritParams impute_zero
#' @inheritParams impute_mice_pmm
#'
#' @returns A \code{data.frame} with imputed values by cart used [mice::mice()].
#'
#' @inheritSection impute_mice_pmm Silent defaults
#'
#' @seealso [mice::mice()], [mice::mice.impute.cart()]
#'
#' @examples
#' data(sim_miss)
#' impute_mice_cart(sim_miss)
#'
#' @export
impute_mice_cart <- function(missdf, ...) {
  eval_mice_calls(missdf = missdf, method = "cart", ...)
}

#' \strong{MICE rf} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by random forest
#' imputations as provided by [mice::mice()].
#'
#' @inheritParams impute_zero
#' @inheritParams impute_mice_pmm
#'
#' @returns A \code{data.frame} with imputed values by random forest used [mice::mice()].
#'
#' @inheritSection impute_mice_pmm Silent defaults
#'
#' @seealso [mice::mice()], [mice::mice.impute.rf()]
#'
#' @examples
#' data(sim_miss)
#' impute_mice_rf(sim_miss)
#'
#' @export
impute_mice_rf <- function(missdf, ...) {
  eval_mice_calls(missdf = missdf, method = "rf", ...)
}

