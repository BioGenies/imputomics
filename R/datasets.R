#' @name sim_miss
#' @title Simulated data for examples
#' @docType data
#' @usage sim_miss
#' @keywords datasets
#' @examples 
#' # code used to generate the data
#' set.seed(2137)
#' set.seed(2137)
#' df <- data.frame(matrix(runif(120), ncol = 6))
#' sim_miss <- insert_MCAR(df, ratio = 0.05)
NULL