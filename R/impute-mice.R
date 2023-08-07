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
#' 
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

#' @describeIn impute_mice_pmm An alias from the \code{missCompare} package.
#' @section Aliases:
#' \code{impute_mice_mixed} is a wrapper of [missCompare::impute_data()] with
#' the \code{method} set to \code{11} (which means that mice is automatically
#' selecting predictive mean matching for numerical data). The amount of iterations
#' \code{n.iter} is changed to 1 from default 10.
#' @export
impute_mice_mixed <- function(missdf) {
  check_missdf(missdf)
  
  imputed <- missCompare::impute_data(missdf,
                                      n.iter = 1,
                                      sel_method = 11)
  data.frame(imputed[["mice_mixed_imputation"]][[1]])
}
