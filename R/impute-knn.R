eval_knn_calls <- function(missdf, distance, k) {
  check_missdf(missdf)
  
  data.frame(imputeKNN(data = as.matrix(missdf), distance = distance, 
                       k = k, rm.na = TRUE, 
                       rm.nan = FALSE,
                       rm.inf = FALSE))
}


#' \strong{tkNN} imputation.
#'
#' Truncated K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by \strong{tkNN} method.
#'
#' @inheritParams impute_zero
#' @param k the number of neighbors.
#'
#' @returns A \code{data.frame} with imputed values by \strong{tkNN} method.
#'
#' @section Source: 
#' This function was copied from https://github.com/WandeRum/GSimp and
#' contains kNN-TN algorithm and related functions developed by Jasmit S. Shah
#' (https://doi.org/10.1186/s12859-017-1547-6).
#'
#' @examples
#' data(sim_miss)
#' impute_tknn(sim_miss)
#'
#' @references
#' \insertRef{shah_distribution_2017}{imputomics}
#'
#' @export
impute_tknn <- function(missdf, k = ceiling(nrow(missdf)*0.05) + 1) {
  eval_knn_calls(missdf = missdf, distance = "truncation", k = k)
}


#' \strong{corkNN} imputation.
#'
#' Correlation K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by \strong{corkNN} method.
#'
#' @inheritParams impute_tknn
#'
#' @returns A \code{data.frame} with imputed values by \strong{corkNN} method.
#'
#' @inheritSection impute_tknn Source
#'
#' @examples
#' data(sim_miss)
#' impute_corknn(sim_miss)
#'
#' @export
impute_corknn <- function(missdf, k = ceiling(nrow(missdf)*0.05) + 1) {
  eval_knn_calls(missdf = missdf, distance = "correlation", k = k)
}
