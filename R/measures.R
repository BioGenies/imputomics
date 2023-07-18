
#' Helper function. Evaluate missing value imputation methods' performance.
#'
#' @param X_observed complete original data (from before amputation).
#' @param X_missing amputed data
#' @param X_imputed imputed data
#' @param measures a character vector with measures to calculate. You can choose
#' from `mae` (Mean Absolute Error), `rmse` (Root Mean Square Error), `nrmse`
#' (Normalized Root Mean Square Error ), `mpe` (Mean Percentage Error), `mape`
#' (Mean Absolute Percentage Error), `rsq` (R-squared), `ccc` (Concordance
#' Correlation Coefficient).
#'
#' @noRd
#' @keywords internal

calculate_measures <- function(X_observed,
                               X_missing,
                               X_imputed,
                               measures = c("mae", "rmse", "nrmse", "mpe",
                                            "mape", "rsq", "ccc")) {

  measures <- match.arg(measures,
                        c("mae", "rmse", "nrmse", "mpe", "mape", "rsq", "ccc"),
                        several.ok = TRUE)

  observed <- X_observed[is.na(X_missing)]
  imputed <- X_imputed[is.na(X_missing)]

  do.call(rbind,
          lapply(measures, function(ith_measure) {
            res <- get(ith_measure)(observed, imputed)

            data.frame(measure = ith_measure,
                       value = res)
          })
  )
}


#' Mean Absolute Error
#' @param observed a vector of true observed values
#' @param imputed a vector of imputed estimates
#'
#' @noRd
#' @keywords internal

mae <- function(observed, imputed)
  mean(abs(observed - imputed))



#' Root Mean Square Error
#' @inheritParams mae
#'
#' @noRd
#' @keywords internal

rmse <- function(observed, imputed)
  sqrt(mean((observed - imputed)^2))



#' Normalized Root Mean Square Error
#'
#' @inheritParams mae
#' @noRd
#' @keywords internal

nrmse <- function(observed, imputed)
  sqrt(mean((observed - imputed)^2) / var(observed))



#' Mean Percentage Error
#' @inheritParams mae
#'
#' @noRd
#' @keywords internal

mpe <- function(observed, imputed)
  mean((observed - imputed)/observed) * 100



#' Mean Absolute Percentage Error
#' @inheritParams mae
#'
#' @noRd
#' @keywords internal

mape <- function(observed, imputed)
  mean(abs((observed - imputed)/observed)) * 100



#' R-squared
#' @inheritParams mae
#'
#' @noRd
#' @keywords internal

rsq <- function(observed, imputed)
  cor(observed, imputed)^2



#' Concordance Correlation Coefficient
#' @inheritParams mae
#'
#' @noRd
#' @keywords internal

ccc <- function(observed, imputed) {
  sd_obs <- sd(observed)
  sd_imp <- sd(imputed)
  avg_obs <- mean(observed)
  avg_imp <- mean(imputed)
  cor_val <- cor(observed, imputed)

  (2 * cor_val * sd_obs * sd_imp)/(sd_obs^2 + sd_imp^2 + (avg_obs - avg_imp)^2)
}





