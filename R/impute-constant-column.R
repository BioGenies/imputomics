#' Helper function.
#' Imputes with a constant value
#'
#' @inheritParams impute_zero
#' @noRd
#' @keywords internal
impute_constant <- function(missdf, constant_value) {

  missdf[is.na(missdf)] <- constant_value
  missdf
}


#' Helper function.
#' Basic imputation of missing values into dataframe.
#' Zero, minimum, half-minimum, mean, median and random.
#'
#' @inheritParams impute_zero
#'
#' @param compute_values one of the basic imputation methods
#' @noRd
#' @keywords internal


impute_per_column <- function(missdf, compute_values) {
  check_missdf(missdf)

  imputed_values_vector <- compute_values(missdf)

  do.call(cbind, lapply(1L:ncol(missdf), function(ith_column_id) {
    impute_constant(missdf[ith_column_id],
                    imputed_values_vector[[ith_column_id]])
  }))
}


#' \strong{zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by  zeros (0).
#'
#' @param missdf a data frame with missing values to be imputed
#' containing features in columns and samples in rows.
#'
#' @returns A \code{data.frame} with imputed values by \strong{zero} method.
#'
#' @examples
#' data(sim_miss)
#' impute_zero(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_zero <- function(missdf) {
  check_missdf(missdf)

  impute_constant(missdf, constant_value = 0)
}


#' Helper function. Minimum imputation.
#'
#' @inheritParams compute_col_random
#' @noRd
#' @keywords internal
compute_col_min <- function(x)
  lapply(x, min, na.rm = TRUE)


#' Helper function. Mean imputation.
#' @inheritParams compute_col_random
#' @noRd
compute_col_mean <- function(x)
  lapply(x, mean, na.rm = TRUE)


#' Helper function. Half-minimum imputation.
#' @noRd
#' @inheritParams compute_col_random
#'
#' @keywords internal
compute_col_halfmin <- function(x)
  lapply(x, function(i) min(i, na.rm = TRUE)/2)


#' Helper function. Median imputation.
#' @noRd
#' @inheritParams compute_col_random
#'
#' @keywords internal
compute_col_median <- function(x)
  lapply(x, median, na.rm = TRUE)


#' \strong{minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by minimum values.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{minimum} method.
#'
#' @examples
#' data(sim_miss)
#' impute_min(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_min <- function(missdf)
  impute_per_column(missdf, compute_col_min)


#' \strong{mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by  mean values.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{mean} method.
#'
#' @examples
#' data(sim_miss)
#' impute_mean(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_mean <- function(missdf)
  impute_per_column(missdf, compute_col_mean)


#' \strong{half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by the half minimum values.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{half-minimum}
#' method.
#'
#' @examples
#' data(sim_miss)
#' impute_halfmin(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_halfmin <- function(missdf)
  impute_per_column(missdf, compute_col_halfmin)


#' \strong{median} imputation.
#'
#' A function to replace \code{NA} in the data frame by a median values.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{median} method.
#'
#' @examples
#' data(sim_miss)
#' impute_median(sim_miss)
#'
#' @keywords constant
#'
#' @export
impute_median <- function(missdf)
  impute_per_column(missdf, compute_col_median)

#' Helper function. Random imputation of a single column.
#'
#' @param x list with values.
#' @noRd
#' @keywords internal
compute_col_random <- function(x)
  lapply(x, function(ith_col) {
    id_nas <- is.na(ith_col)
    sample(x = ith_col[!id_nas], size = sum(id_nas), replace = TRUE)
  })

#' \strong{random} imputation.
#'
#' A function to replace \code{NA} in the data frame by  random values.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{random} method.
#'
#' @examples
#' data(sim_miss)
#' impute_random(sim_miss)
#'
#' @export
impute_random <- function(missdf)
  impute_per_column(missdf, compute_col_random)
