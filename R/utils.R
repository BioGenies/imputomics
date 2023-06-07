#' Checks (asserts) if an object is a data.frame
#'
#' @param missdf an object.
#' @param above_zero boolean, if \code{TRUE} checks if all values are above zero
#'
#' @keywords internal
#' @importFrom checkmate function
#' 
check_missdf <- function(missdf, above_zero = FALSE) {
  if(!testDataFrame(missdf))
    stop("'missdf' must be a data.frame or tibble.")
  
  if(!all(sapply(missdf, testNumeric)))
    stop("'missdf' must contain only numeric data.")
  
  if(above_zero) {
    if(!all(sapply(missdf, testNumeric, lower = 0)))
      stop("'missdf' must contain only numeric data above 0.")
  } else {
    if(!all(sapply(missdf, testNumeric)))
      stop("'missdf' must contain only numeric data.")
  }
    
}
