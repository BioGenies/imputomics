#' Convert an imputing function into its safe version.
#'
#' @inheritParams impute_zero
#'
#' @param imputing_function a function (imputation method) that takes
#' missdf as an input
#'
#' @return A \code{data.frame} with imputed values or the
#' \strong{missdf} if the imputing function failed to converge.
#'
#' @keywords internal

safe_impute <- function(imputing_function, missdf) {
  imputed <- structure(structure(list(), class = "try-error"))
  n <- 1
  while(inherits(imputed, "try-error") & n < 101) {
    imputed <- try(imputing_function(missdf), silent = TRUE)
    n <- n + 1
  }
  
  if(inherits(imputed, "try-error")) {
    missdf
  } else {
    imputed
  }
}

#' Checks (asserts) if an object is a data.frame
#'
#' @param missdf an object.
#' @param above_zero boolean, if \code{TRUE} checks if all values are above zero
#' @noRd
#' @keywords internal
#' @importFrom checkmate testDataFrame testNumeric
#' 
check_missdf <- function(missdf, above_zero = FALSE) {
  if(!testDataFrame(missdf))
    stop("'missdf' must be a data.frame or tibble.")
  
  if(all(sapply(missdf, function(i) !is.na(i))))
    if(any(sapply(missdf, checkmate::testNumeric, lower = 0, upper = 0))) {
      warning("No NAs identified, but the data contains zeros.\nMake sure that you are marking missing values with 0.")
    } else {
      message("No NAs identified.")
    }
      
  
  if(above_zero) {
    if(!all(sapply(missdf, testNumeric, lower = 0)))
      stop("'missdf' must contain only numeric data above 0.")
  } else {
    if(!all(sapply(missdf, testNumeric)))
      stop("'missdf' must contain only numeric data.")
  }
  
}

# add a function for checking the output
