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






#' \strong{missMDA EM} imputation.
#'
#' PCA method with EM argument.
#'
#' A function to replace \code{NA} in the data frame by [missMDA::imputePCA()]
#' with method = "em".
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [missMDA::imputePCA()]
#' with method = "em".
#'
#' @seealso [missMDA::imputePCA()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missmda_em(idf)
#' }
#'
#' @references
#' \insertRef{josse_missmda_2016}{imputomics}
#'
#' @export

impute_missmda_em <- function(missdf) {
  imputed <- missMDA::imputePCA(missdf,
                                method = "EM")
  # data.frame necessary because missMDA::imputePCA returns matrix
  data.frame(imputed[["completeObs"]])
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
#' @importFrom mice mice.impute.pmm
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by pmm used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.pmm()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_pmm(idf)
#' }
#'
#' @references
#' \insertRef{buuren_mice_2011}{imputomics}
#'
#' @export

impute_mice_pmm <- function(missdf) {
  imputed <- mice::mice(missdf,
                        method = 'pmm',
                        m = 5,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missdf))
  mice::complete(imputed)
}

#' \strong{MICE cart} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by classification and
#' regression trees (cart) used [mice::mice()].
#'
#' @importFrom mice mice.impute.cart
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by cart used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.cart()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_cart(idf)
#' }
#'
#' @export

impute_mice_cart <- function(missdf) {
  imputed <- mice::mice(missdf,
                        method = 'cart',
                        m = 5,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missdf))
  mice::complete(imputed)
}

#' \strong{MICE rf} imputation.
#'
#' Multiple Imputation by Chained Equations.
#'
#' A function to replace \code{NA} in the data frame by random forest
#' imputations (rf) used [mice::mice()].
#'
#' @importFrom mice mice.impute.rf
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by rf used [mice::mice()].
#'
#' @seealso [mice::mice()], [mice::mice.impute.rf()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#'
#' impute_mice_rf(idf)
#' }
#'
#' @export

impute_mice_rf <- function(missdf) {
  imputed <- mice::mice(missdf,
                        method = 'rf',
                        m = 5,
                        maxit = 100,
                        printFlag = FALSE,
                        predictorMatrix = mice::quickpred(missdf))
  mice::complete(imputed)
}



#' \strong{Amelia} imputation.
#'
#' A function to replace \code{NA} in the data frame by [Amelia::amelia()].
#'
#' @importFrom Amelia amelia
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [Amelia::amelia()].
#'
#' @seealso [Amelia::amelia()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_amelia(idf)
#' }
#'
#' @references
#' \insertRef{honaker_amelia_2011}{imputomics}
#'
#' @export

impute_amelia <- function(missdf) {
  capture.output(imputed <- Amelia::amelia(missdf, m = 1))
  imputed[["imputations"]][["imp1"]]
}


#' \strong{MissForest} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [missForest::missForest()].
#'
#' @importFrom missForest missForest
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by
#' [missForest::missForest()].
#'
#' @seealso [missForest::missForest()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_missforest(idf)
#' }
#'
#' @references
#' \insertRef{stekhoven_missforest_2012}{imputomics}
#'
#' @export

impute_missforest <- function(missdf) {
  imputed <- missForest::missForest(missdf,
                                    maxiter = 10,
                                    ntree = 500,
                                    replace = TRUE)
  imputed[["ximp"]]
}



#' \strong{Hmisc areg} imputation.
#'
#' Multiple Imputation using Additive Regression, Bootstrapping, and Predictive
#' Mean Matching.
#'
#' A function to replace \code{NA} in the data frame by [Hmisc::aregImpute()].
#'
#' @importFrom Hmisc aregImpute
#' @importFrom Hmisc impute.transcan
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [Hmisc::aregImpute()].
#'
#' @seealso [Hmisc::aregImpute()]
#'
#' @references
#' \insertRef{jr_hmisc_2023}{imputomics}
#'
#' @export

impute_areg <- function(missdf) {
  capture.output(imputed <- Hmisc::aregImpute(
    formula = as.formula(paste0("~ ", paste0(colnames(missdf),
                                             collapse = " + "))),
    data = missdf,
    tlinear = FALSE)
  )
  data.frame(do.call(cbind,
                     Hmisc::impute.transcan(imputed,
                                            imputation = 1,
                                            data = missdf,
                                            list.out = TRUE,
                                            pr = FALSE,
                                            check = FALSE)))
}


#' \strong{kNN} imputation.
#'
#' K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by [impute::impute.knn()].
#'
#' @importFrom impute impute.knn
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [impute::impute.knn()].
#'
#' @seealso [impute::impute.knn()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' set.seed(2137)
#' impute_knn(idf)
#' }
#'
#' @references
#' \insertRef{hastie_impute_2023}{imputomics}
#'
#' @export

impute_knn <- function(missdf) {
  # this function has a default random seed, so we need to sample one
  imputed <- impute::impute.knn(as.matrix(missdf),
                                k = 10,
                                rng.seed = sample(1L:1e9, 1))
  data.frame(imputed[["data"]])
}


#' \strong{QRLIC} imputation.
#'
#' Quantile Regression Imputation Of Left-Censored Data.
#'
#' A function to replace \code{NA} in the data frame by
#' [imputeLCMD::impute.QRILC()].
#'
#' @importFrom imputeLCMD impute.QRILC
#'
#' @importFrom imputeLCMD impute.QRILC
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by
#' [imputeLCMD::impute.QRILC()].
#'
#' @seealso [imputeLCMD::impute.QRILC()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_qrilc(idf)
#' }
#' @references
#' \insertRef{lazar_imputelcmd_2022}{imputomics}
#'
#' @export
#'
impute_qrilc <- function(missdf) {
  imputeLCMD::impute.QRILC(missdf)[[1]]
}



#' \strong{SoftImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [softImpute::softImpute()].
#'
#' @importFrom softImpute softImpute
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by
#' [softImpute::softImpute()].
#'
#' @seealso [softImpute::softImpute()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_softimpute(idf)
#' }
#'
#' @references
#' \insertRef{mazumder_softimpute_2021}{imputomics}
#'
#' @export

impute_softimpute <- function(missdf) {
  fit <- softImpute::softImpute(as.matrix(missdf))
  data.frame(softImpute::complete(as.matrix(missdf), fit))
}




#' \strong{PEMM} imputation.
#'
#' Penalized EM Algorithm.
#'
#' A function to replace \code{NA} in the data frame by [PEMM::PEMM_fun()].
#'
#' @importFrom PEMM PEMM_fun
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [PEMM::PEMM_fun()].
#'
#' @seealso [PEMM::PEMM_fun()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_PEMM(as.matrix(idf))
#' }
#'
#' @references
#' \insertRef{chen_penalized_2014}{imputomics}
#'
#' @export

impute_PEMM <- function(missdf) {
  #PEMM_fun requires matrix
  missdf <- as.matrix(missdf)
  imputed <- PEMM::PEMM_fun(missdf,
                            phi = 1)
  data.frame(imputed[["Xhat"]])
}


#' \strong{tkNN} imputation.
#'
#' Truncated K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by \strong{tkNN} method.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{tkNN} method.
#'
#' @details This function was copied from https://github.com/WandeRum/GSimp and
#' contains kNN-TN algorithm and related functions developed by Jasmit S. Shah
#' (https://doi.org/10.1186/s12859-017-1547-6).
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_tknn(idf)
#' }
#'
#' @references
#' \insertRef{shah_distribution_2017}{imputomics}
#'
#' @export
#'
#'

impute_tknn <- function(missdf) {
  imputed <- imputeKNN(as.matrix(missdf),
                       k = ceiling(nrow(missdf)*0.05) + 1,
                       distance = "truncation",
                       rm.na = TRUE,
                       rm.nan = FALSE,
                       rm.inf = FALSE)
  data.frame(imputed)
}

#' \strong{corkNN} imputation.
#'
#' Correlation K Nearest Neighbors.
#'
#' A function to replace \code{NA} in the data frame by \strong{corkNN} method.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{corkNN} method.
#'
#' @details This function was copied from https://github.com/WandeRum/GSimp and
#' contains kNN-TN algorithm and related functions developed by Jasmit S. Shah
#' (https://doi.org/10.1186/s12859-017-1547-6).
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_tknn(idf)
#' }
#'
#' @export
impute_corknn <- function(missdf) {
  imputed <- imputeKNN(as.matrix(missdf),
                       k = ceiling(nrow(missdf)*0.05) + 1,
                       distance = "correlation",
                       rm.na = TRUE,
                       rm.nan = FALSE,
                       rm.inf = FALSE)
  data.frame(imputed)
}


#' \strong{GSimp} imputation.
#'
#' Gibbs Sampler Based Left-Censored Missing Value Imputation.
#'
#' A function to replace \code{NA} in the data frame by \strong{GSimp} method.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by \strong{GSimp} method.
#'
#' @details  This function was copied from https://github.com/WandeRum/GSimp and
#' contains the GSimp algorithm and related functions developed by Rum Wei
#' (10.1371/journal.pcbi.1005973).
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_gsimp(idf)
#' }
#'
#' @references
#' \insertRef{wei_gsimp_2018}{imputomics}
#'
#' @export

impute_gsimp <- function(missdf) {
  imputed <- GS_impute_clean(missdf,
                             initial = "lsym",
                             imp_model = 'glmnet_pred')
  data.frame(imputed[["data_imp"]])
}

#' \strong{kNN} imputation.
#'
#' K Nearest Neighbors. A function to replace \code{NA} in the data frame by
#' [VIM::kNN()].
#'
#' @importFrom VIM kNN
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [VIM::kNN()].
#'
#' @seealso [VIM::kNN()]
#'
#' @examples
#' \dontrun{
#' idf <- data.frame(values1 = rep(c(11, 22, NA, 44, NA), 10),
#' values2 = rep(c(21, 32, 48, NA, 59), 10),
#' values3 = rep(c(37, NA, 33, 44, 32), 10))
#' impute_vim_knn(idf)
#' }
#'
#' @references
#' \insertRef{kowarik_imputation_2016}{imputomics}
#'
#' @export
#'
impute_vim_knn <- function(missdf) {
  imputed <- VIM::kNN(missdf, k = 5)[,1:ncol(missdf)]
  data.frame(imputed)
}


#' \strong{MetabImpute RF} imputation.
#'
#' A function to replace \code{NA} in the data frame using random forest.
#'
#' @importFrom MetabImpute imputeMulti
#' @importFrom MetabImpute Impute
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [MetabImpute::Impute()]
#' or [MetabImpute::imputeMulti()].
#'
#' @seealso [MetabImpute::Impute()], [MetabImpute::imputeMulti()]]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_RF(idf)
#' }
#'
#' @export
#'
impute_MetabImpute_RF <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'RF',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute BPCA} imputation.
#'
#' A function to replace \code{NA} in the data frame by Bayesian PCA.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of Bayesian PCA by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_BPCA(idf)
#' }
#'
#' @export

impute_MetabImpute_BPCA <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'BPCA',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute QRILC} imputation.
#'
#' A function to replace \code{NA} in the data frame by QRILC.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of QRILC by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_QRILC(idf)
#' }
#'
#' @export

impute_MetabImpute_QRILC <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'QRILC',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute GSIMP} imputation.
#'
#' A function to replace \code{NA} in the data frame by GSIMP.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of GSIMP by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_GSIMP(idf)
#' }
#'
#' @export

impute_MetabImpute_GSIMP <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'GSIMP',
                                 reps = 1,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by minimum.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of minimum by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_min(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_min <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'min',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute half-minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by half-minimum from.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of half-minimum by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_halfmin(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_halfmin <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'halfmin',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute mean} imputation.
#'
#' A function to replace \code{NA} in the data frame by mean.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of mean by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_mean(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_mean <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'mean',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute median} imputation.
#'
#' A function to replace \code{NA} in the data frame by median.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of median by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_median(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_median <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'median',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}

#' \strong{MetabImpute zero} imputation.
#'
#' A function to replace \code{NA} in the data frame by zero.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values of zero by
#' [MetabImpute::Impute()].
#'
#' @seealso [MetabImpute::Impute()]
#'
#' @examples
#' \dontrun{
#' idf <- runif(100)
#' idf[sample(1L:100, round(4, 0))] <- NA
#' idf <- data.frame(matrix(idf, nrow = 10))
#' impute_MetabImpute_zero(idf)
#' }
#'
#' @keywords constant
#'
#' @export

impute_MetabImpute_zero <- function(missdf) {
  imputed <- MetabImpute::Impute(data = missdf,
                                 method = 'zero',
                                 reps = 5,
                                 local = TRUE,
                                 rep_threshold = 2/3)
  data.frame(imputed)
}


#' \strong{Mechanism-Aware} imputation.
#'
#' A function to replace \code{NA} in the data frame by [MAI::MAI()] with random
#' forest and single imputation.
#'
#' @importFrom MAI MAI
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by [MAI::MAI()].
#'
#' @seealso [MAI::MAI()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(10000) < 0.1] <- NA
#' impute_MA(idf)
#' }
#'
#' @references
#' \insertRef{dekermanjian_mechanismaware_2022}{imputomics}
#'
#' @export

impute_MA <- function(missdf) {
  imputed <- MAI::MAI(missdf,
                      MCAR_algorithm = 'random_forest',
                      MNAR_algorithm = 'Single')
  data.frame(imputed[["Imputed_data"]])
}


#' \strong{kNN-Euclidean} imputation.
#'
#' A function to replace \code{NA} in the data frame based on
#' \emph{Jasmit S. Shah (https://doi.org/10.1186/s12859-017-1547-6)}.
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by kNN-Euclidean imputation.
#'
#' @details This function was copied from https://github.com/WandeRum/GSimp and
#' contains kNN-TN algorithm and related functions developed by Jasmit S. Shah
#' (https://doi.org/10.1186/s12859-017-1547-6).
#'
#' @seealso \emph{Jasmit S. Shah (https://doi.org/10.1186/s12859-017-1547-6)}
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(10000) < 0.1] <- NA
#' impute_eucknn(idf)
#' }
#'
#' @references
#' \insertRef{shah_distribution_2017}{imputomics}
#'
#' @export

impute_eucknn <- function(missdf) {
  imputed <- KNNEuc(as.matrix(missdf),
                    k = ceiling(nrow(missdf)*0.05) + 1,
                    rm.na = TRUE,
                    rm.nan = TRUE,
                    rm.inf = TRUE)
  data.frame(imputed)
}

#' \strong{mice mixed} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [missCompare::impute_data()].
#'
#' @importFrom missCompare impute_data
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by
#' [missCompare::impute_data()].
#'
#' @seealso [missCompare::impute_data()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_mice_mixed(idf)
#' }
#'
#' @export

impute_mice_mixed <- function(missdf) {
  imputed <- missCompare::impute_data(missdf,
                                      scale = FALSE,
                                      n.iter = 10,
                                      sel_method = 11)
  data.frame(imputed[['mice_mixed_imputation']][[10]])
}


#' \strong{RegImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by imputation using Glmnet
#' ridge regression (RegImpute) from [DreamAI::DreamAI()].
#'
#' @importFrom DreamAI DreamAI
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by RegImpute
#'
#' @seealso [DreamAI::DreamAI()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_RegImpute(idf)
#' }
#'
#' @export

impute_RegImpute <- function(missdf) {
  imputed <- DreamAI::DreamAI(missdf,
                              k = 10,
                              maxiter_MF = 10,
                              ntree = 100,
                              maxnodes = NULL,
                              maxiter_ADMIN = 30,
                              tol = 10^(-2),
                              gamma_ADMIN = NA,
                              gamma = 50,
                              CV = FALSE,
                              fillmethod = "row_mean",
                              maxiter_RegImpute = 10,
                              conv_nrmse = 1e-06,
                              iter_SpectroFM = 40,
                              method = c("RegImpute"),
                              out = c("Ensemble"))
  data.frame(imputed[['Ensemble']])
}


#' \strong{Singular Value Decomposition - SVD} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [bcv::impute.svd()].
#'
#' @importFrom bcv impute.svd
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by SVD.
#'
#' @seealso [bcv::impute.svd()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_bcv_svd(idf)
#' }
#'
#' @export

impute_bcv_svd <- function(missdf){
  imputed <- bcv::impute.svd(missdf)
  data.frame(imputed[['x']])
}


#' \strong{kNN} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [imputation::kNNImpute()].
#'
#' @importFrom imputation kNNImpute
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by kNN.
#'
#' @seealso [imputation::kNNImpute()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_imputation_kNN(idf)
#' }
#'
#' @export

impute_imputation_kNN <- function(missdf){
  # kNNImpute needs data to be a matrix
  missdf <- as.matrix(missdf)
  imputed <- imputation::kNNImpute(missdf,
                                   k = min(nrow(missdf),
                                           ncol(missdf),
                                           11) - 1)
  data.frame(imputed[['x']])
}

#' \strong{Metabolomic Non-negative Matrix Factorization - mNMF} imputation.
#'
#' A function to replace \code{NA} in the data frame based on
#' \emph{Jingjing Xu (https://doi.org/10.3390/molecules26195787)}.
#'
#' @importFrom NMF nmf.getOption
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by mNMF.
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_mNMF(idf)
#'}
#' @export

impute_mNMF <- function(missdf){

  # samples in columns and features in rows
  missdf <- t(missdf)
  k_group <- unique(round(seq(1,
                              min(ncol(missdf),
                                  nrow(missdf)),
                              length.out = min(20, ncol(missdf))), 0))
  imputed <- nmf_opt(IMP = missdf,
                     M = NMF::nmf.getOption('default.algorithm'),
                     kgroup = k_group,
                     initialType = "mean")
  data.frame(t(imputed))
}



#' \strong{Compound Minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by [GMSimpute::GMS.Lasso()]
#'
#' @inheritParams impute_zero
#' @importFrom GMSimpute GMS.Lasso
#'
#' @returns A \code{data.frame} with imputed values by CM.
#'
#' @seealso [GMSimpute::GMS.Lasso()]
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_CM(idf)
#'}
#'
#' @keywords constant
#'
#' @export

impute_CM <- function(missdf){
  # samples in columns and features in rows
  missdf <- t(missdf)
  imputed <- GMSimpute::GMS.Lasso(missdf, TS.Lasso = FALSE)
  data.frame(imputed)
}



#' \strong{BayesMetab} imputation.
#'
#' A function to replace \code{NA} in the data frame based on
#' \emph{Jasmit Shah (10.1186/s12859-019-3250-2)}.
#'
#' @importFrom SimDesign rmvnorm
#' @importFrom truncnorm rtruncnorm
#'
#' @inheritParams impute_zero
#'
#' @returns A \code{data.frame} with imputed values by BayesMetab
#'
#' @examples
#' \dontrun{
#' idf <- matrix(round(runif(1000, 1000, 5000), 0), ncol =  10)
#' idf[runif(1000) < 0.1] <- NA
#' impute_CM(idf)
#'}
#' @export

impute_BayesMetab <- function(missdf){
  # MCMC.Factor requires matrix
  missdf <- as.matrix(missdf)
  imputed <- MCMC.Factor(missdf,
                         M = 100,
                         miss.pattern = !is.na(missdf),
                         K.max = ncol(missdf))
  data.frame(imputed[[5]])
}

