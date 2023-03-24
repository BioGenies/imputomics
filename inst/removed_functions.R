#' \strong{MLE} imputation.
#'
#' Imputation Using The EM Algorithm.
#'
#' A function to replace \code{NA} in the data frame by
#' [imputeLCMD::impute.wrapper.MLE()].
#'
#' @importFrom imputeLCMD impute.wrapper.MLE
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by
#' [imputeLCMD::impute.wrapper.MLE()].
#'
#' @seealso [imputeLCMD::impute.wrapper.MLE()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_mle(idf)
#' }
#'
#' @references
#' \insertRef{lazar_imputelcmd_2022}{imputomics}
#'
#' @export

impute_mle <- function(missing_data_set) {
  imputed <- imputeLCMD::impute.wrapper.MLE(as.matrix(missing_data_set))
  data.frame(imputed)
}



#' \strong{IRMI} imputation.
#'
#' Iterative Robust Model-Based Imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [NADIA::autotune_VIM_Irmi()].
#'
#' @importFrom NADIA autotune_VIM_Irmi
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by
#' [NADIA::autotune_VIM_Irmi()].
#'
#' @seealso [NADIA::autotune_VIM_Irmi()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_irmi(idf)
#' }
#'
#' @references
#' \insertRef{borowski_nadia_2022}{imputomics}
#'
#' @export

impute_irmi <- function(missing_data_set) {
  NADIA::autotune_VIM_Irmi(missing_data_set,
                           col_type = rep("numeric", ncol(missing_data_set)),
                           percent_of_missing = colMeans(
                             is.na(missing_data_set)
                           )*100,
                           maxit = 200)
}


#' \strong{MICE lasso.norm} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by LASSO linear regression
#' (lasso.norm) used [mice::mice()].
#'
#' @importFrom mice mice.impute.lasso.norm
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by lasso.norm used
#' [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.lasso.norm()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_lasso.norm(idf)
#' }
#'
#' @export

impute_mice_lasso.norm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'lasso.norm',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}



#' \strong{MICE lasso.select.norm} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by LASSO selection with
#' linear regression regression (lasso.select.norm) used [mice::mice()].
#'
#' @importFrom mice mice.impute.lasso.select.norm
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by lasso.select.norm used
#' [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.lasso.select.norm()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_lasso.select.norm(idf)
#' }
#'
#' @export

impute_mice_lasso.select.norm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'lasso.select.norm',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}


#' \strong{mi} imputation.
#'
#' A function to replace \code{NA} in the data frame by [mi::mi()].
#'
#' @importFrom mi mi
#' @importFrom mi complete
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [mi::mi()].
#'
#' @seealso [mi::mi()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_mi(idf)
#' }
#'
#' @references
#' \insertRef{su_multiple_2011}{imputomics}
#'
#' @export

impute_mi <- function(missing_data_set) {
  # requires betareg
  capture.output(imputed <- mi::mi(missing_data_set,
                                   n.chain = 1,
                                   n.iter = 100,
                                   verbose = FALSE,
                                   parallel = FALSE))

  mi::complete(imputed)[colnames(missing_data_set)]

}


#' \strong{MICE midastouch} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by weighted predictive mean
#' matching (midastouch) used [mice::mice()].
#'
#' @importFrom mice mice.impute.midastouch
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by midastouch used
#' [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.midastouch()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_midastouch(idf)
#' }
#'
#' @export

impute_mice_midastouch <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'midastouch',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}


#' \strong{NLPCA} imputation.
#'
#' Nonlinear Principal Component Analysis.
#'
#' A function to replace \code{NA} in the data frame by [pcaMethods::pca()]
#' with method = "nlpca".
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [pcaMethods::pca()]
#' with method = "nlpca".
#'
#' @seealso [pcaMethods::pca()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_nlpca(idf)
#' }
#'
#' @references
#' \insertRef{stacklies_pcamethods_2007}{imputomics}
#'
#' @export

impute_nlpca <- function(missing_data_set) {
  imputed <- pcaMethods::pca(missing_data_set,
                             method = "nlpca",
                             verbose = FALSE,
                             maxSteps = 500)
  # data.frame necessary because pcaMethods::pca returns matrix
  data.frame(pcaMethods::completeObs(imputed))
}


#' \strong{MICE norm} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by Bayesian linear
#' regression (norm) used [mice::mice()].
#'
#' @importFrom mice mice.impute.norm
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by norm used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.norm()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm(idf)
#' }
#'
#' @export

impute_mice_norm <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'norm',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}


#' \strong{MICE norm.boot} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by linear regression using
#' bootstrap (norm.boot) used [mice::mice()].
#'
#' @importFrom mice mice.impute.norm.boot
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by norm.boot used
#' [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.norm.boot()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm.boot(idf)
#' }
#'
#' @export

impute_mice_norm.boot <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'norm.boot',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}



#' \strong{MICE norm.nob} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by linear regression
#' ignoring model error (norm.nob) used [mice::mice()].
#'
#' @importFrom mice mice.impute.norm.nob
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by norm.nob used
#' [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.norm.nob()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm.nob(idf)
#' }
#'
#' @export

impute_mice_norm.nob <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'norm.nob',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}



#' \strong{MICE norm.predict} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by predicted values from
#' linear regression (norm.predict) used [mice::mice()].
#'
#' @importFrom mice mice.impute.norm.predict
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by norm.predict used
#' [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.norm.predict()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_norm.predict(idf)
#' }
#'
#' @export

impute_mice_norm.predict <- function(missing_data_set) {
  imputed <- mice::mice(missing_data_set,
                        method = 'norm.predict',
                        m = 1,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missing_data_set))
  mice::complete(imputed)
}


#' \strong{random} imputation.
#'
#' A function to replace \code{NA} in the data frame by  random values.
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by \strong{random} method.
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = c(11, 22, NA, 44, NA),
#' values2 = c(21, 32, 48, NA, 59))
#' impute_random(idf)
#' }
#'
#' @export

impute_random <- function(missing_data_set)
  impute_per_column(missing_data_set, compute_col_random)



#' \strong{missMDA regularized} imputation.
#'
#' PCA method with regularized argument.
#'
#' A function to replace \code{NA} in the data frame by [missMDA::imputePCA()]
#' with method = "regularized".
#'
#' @importFrom missMDA imputePCA
#'
#' @inheritParams impute_constant
#'
#' @returns A \code{data.frame} with imputed values by [missMDA::imputePCA()]
#' with method = "regularized".
#'
#' @seealso [missMDA::imputePCA()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missmda_reg(idf)
#' }
#'
#' @references
#' \insertRef{josse_missmda_2016}{imputomics}
#'
#' @export

impute_missmda_reg <- function(missing_data_set) {
  imputed <- missMDA::imputePCA(missing_data_set,
                                method = "Regularized")
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
}



