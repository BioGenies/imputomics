#' @name sim_miss
#' @title Simulated data for examples
#' @docType data
#' @usage sim_miss
#' @keywords datasets
#' @examples 
#' # code used to generate the data
#' set.seed(17)
#' df <- data.frame(matrix(runif(120), ncol = 6))
#' sim_miss <- insert_MCAR(df, ratio = 0.05)
NULL

#' @name sim_miss_large
#' @title Much larger simulated data for examples
#' @docType data
#' @usage sim_miss_large
#' @keywords datasets
#' @examples 
#' # code used to generate the data
#' set.seed(1758)
#' df <- data.frame(matrix(runif(3000), ncol = 60))
#' sim_miss_large <- insert_MCAR(df, ratio = 0.25)
NULL

#' @name problematic_dataset
#' @title Tough data to impute
#' This dataset makes many methods (\code{cm}, \code{amelia}) fail.
#' @docType data
#' @usage problematic_dataset
#' @keywords datasets
NULL