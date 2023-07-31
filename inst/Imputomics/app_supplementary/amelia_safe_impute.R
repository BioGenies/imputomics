

safe_impute_amelia <- function(imputing_function,
                               missing_data_set,
                               timeout = 300) {
  imputed <- structure(structure(list(), class = "try-error"))
  n <- 1
  while(inherits(imputed, "try-error") & n < 3) {
    imputed <- try({
      R.utils::withTimeout({
        callr::r(function(imputing_function,
                          missing_data_set,
                          timeout) {
          imputing_function(missing_data_set)
        }, args = list(imputing_function = imputing_function,
                       missing_data_set = missing_data_set),
        package = "imputomics")},
        timeout = timeout, onTimeout = "error")
    }, silent = TRUE)

    n <- n + 1
  }

  imputed
}
