#' \strong{missMDA EM} imputation.
#'
#' PCA method with EM argument.
#'
#' A function to replace \code{NA} in the data frame by [missMDA::imputePCA()]
#' with method = "em".
#'
#' @inheritParams impute_zero
#' @param ... other parameters of [missMDA::imputePCA()] besides \code{method} and
#' \code{X}.
#'
#' @returns A \code{data.frame} with imputed values by [missMDA::imputePCA()]
#' with \code{method = "em"}.
#'
#' @seealso [missMDA::imputePCA()]
#'
#' @examples
#' data(sim_miss)
#' impute_missmda_em(sim_miss)
#'
#' @references
#' \insertRef{josse_missmda_2016}{imputomics}
#'
#' @export
impute_missmda_em <- function(missdf, ...) {
  check_missdf(missdf)

  imputed <- missMDA::imputePCA(X = missdf, method = "EM")

  data.frame(imputed[["completeObs"]])
}


#' \strong{Amelia} imputation.
#'
#' A function to replace \code{NA} in the data frame by [Amelia::amelia()].
#'
#' @importFrom Amelia amelia
#'
#' @inheritParams impute_zero
#' @param verbose boolean, if \code{TRUE}, prints the typical prompts of
#' [Amelia::amelia()].
#' @param ... other parameters of [Amelia::amelia()] besides \code{x} and
#' \code{m}.
#'
#' @details
#' \code{amelia()} allows users to customize the number of imputed datasets to
#' create. As one of the aims of the \code{imputomics} is to standardize the
#' input and the output, the \code{m} is being set to 1.
#'
#' @returns A \code{data.frame} with imputed values by [Amelia::amelia()].
#'
#' @seealso [Amelia::amelia()]
#'
#' @examples
#' data(sim_miss)
#' impute_amelia(sim_miss)
#'
#' @references
#' \insertRef{honaker_amelia_2011}{imputomics}
#'
#' @export
impute_amelia <- function(missdf, verbose = FALSE, ...) {
  check_missdf(missdf)

  silence_function(verbose)(imputed <- Amelia::amelia(missdf, m = 1, ...))

  if(is.null(imputed[["imputations"]][["imp1"]])) {
    stop(paste0("Amelia failed to impute missing values. Error code: ", imputed[["code"]], "."))
  }

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
#' @param ... other parameters of [missForest::missForest()] besides \code{xmis}.
#'
#' @returns A \code{data.frame} with imputed values by
#' [missForest::missForest()].
#'
#' @seealso [missForest::missForest()]
#'
#' @examples
#' data(sim_miss)
#' impute_missforest(sim_miss)
#'
#' @references
#' \insertRef{stekhoven_missforest_2012}{imputomics}
#'
#' @export
impute_missforest <- function(missdf, ...) {
  check_missdf(missdf)

  imputed <- missForest::missForest(xmis = missdf, ...)
  imputed[["ximp"]]
}


#' \strong{Hmisc areg} imputation.
#'
#' Multiple Imputation using Predictive Mean Matching.
#'
#' A function to replace \code{NA} in the data frame by [Hmisc::aregImpute()].
#'
#' @importFrom Hmisc aregImpute
#' @importFrom Hmisc impute.transcan
#'
#' @inheritParams impute_zero
#' @param verbose boolean, if \code{TRUE}, prints the typical prompts of
#' [Hmisc::aregImpute()].
#' @param ... other parameters of [Hmisc::aregImpute()] besides \code{formula},
#' \code{formula}, \code{data} and \code{type}.
#'
#' @section Silent defaults:
#' \code{burnin = 5} and \code{nk = 0}.
#'
#' @details
#' \code{aregImpute()} allows users to customize the number of imputed datasets to
#' create. As one of the aims of the \code{imputomics} is to standardize the
#' input and the output, the \code{n.impute} is being set to 1.
#'
#' @returns A \code{data.frame} with imputed values by [Hmisc::aregImpute()].
#'
#' @examples
#' data(sim_miss)
#' impute_areg(sim_miss)
#'
#' @seealso [Hmisc::aregImpute()]
#'
#' @references
#' \insertRef{jr_hmisc_2023}{imputomics}
#'
#' @export
impute_areg <- function(missdf, verbose = FALSE, ...) {
  check_missdf(missdf)

  all_args <- extend_arglist(list(...),
                             list(formula = stats::as.formula(paste("~", paste(colnames(missdf), collapse = "+"))),
                                  data = missdf, n.impute = 1, type = "pmm"),
                             list(burnin = 5, nk = 0))

  silence_function(verbose)(imputed <- do.call(Hmisc::aregImpute, all_args))

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
#' @param ... other parameters of [impute::impute.knn()] besides \code{data}.
#' @returns A \code{data.frame} with imputed values by [impute::impute.knn()].
#'
#' @section Silent defaults:
#' \code{impute.knn} sets its \code{rng.seed} by default to 362436069. To avoid it,
#' \code{imputomics} by default uses \code{sample(1L:1e9, 1)}.
#' @seealso [impute::impute.knn()]
#'
#' @examples
#' data(sim_miss)
#' impute_knn(sim_miss)
#'
#' @references
#' \insertRef{hastie_impute_2023}{imputomics}
#'
#' @export
impute_knn <- function(missdf, ...) {
  check_missdf(missdf)

  all_args <- extend_arglist(list(...),
                             list(data = t(as.matrix(missdf))),
                             list(rng.seed = sample(1L:1e9, 1)))

  imputed <- do.call(impute::impute.knn, all_args)

  data.frame(t(imputed[["data"]]))
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
#' @inheritParams impute_zero
#' @param ... other parameters of [imputeLCMD::impute.QRILC()] besides
#' \code{dataSet.mvs}.
#'
#' @returns A \code{data.frame} with imputed values by
#' [imputeLCMD::impute.QRILC()].
#'
#' @seealso [imputeLCMD::impute.QRILC()]
#'
#' @examples
#' data(sim_miss)
#' impute_qrilc(sim_miss)
#'
#' @references
#' \insertRef{lazar_imputelcmd_2022}{imputomics}
#'
#' @export
#'
impute_qrilc <- function(missdf, ...) {
  check_missdf(missdf)

  imputeLCMD::impute.QRILC(missdf, ...)[[1]]
}


#' \strong{SoftImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [softImpute::softImpute()].
#'
#' @importFrom softImpute softImpute
#'
#' @inheritParams impute_zero
#' @param ... other parameters of [softImpute::softImpute()] besides
#' \code{x}.
#'
#' @returns A \code{data.frame} with imputed values by
#' [softImpute::softImpute()].
#'
#' @seealso [softImpute::softImpute()]
#'
#' @examples
#' data(sim_miss)
#' impute_softimpute(sim_miss)
#'
#' @references
#' \insertRef{mazumder_softimpute_2021}{imputomics}
#'
#' @export
impute_softimpute <- function(missdf, ...) {
  check_missdf(missdf)

  fit <- softImpute::softImpute(as.matrix(missdf), ...)
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
#' @param phi See the documentation of [PEMM::PEMM_fun()].
#'
#' @returns A \code{data.frame} with imputed values by [PEMM::PEMM_fun()].
#'
#' @seealso [PEMM::PEMM_fun()]
#' @export
#' @examples
#' data(sim_miss)
#' impute_pemm(sim_miss)
#'
#' @references
#' \insertRef{chen_penalized_2014}{imputomics}
#'
#' @export
impute_pemm <- function(missdf, phi = 1) {
  check_missdf(missdf)

  missdf <- as.matrix(missdf)
  imputed <- PEMM::PEMM_fun(missdf, phi)
  data.frame(imputed[["Xhat"]])
}


#' \strong{GSimp} imputation.
#'
#' Gibbs Sampler Based Left-Censored Missing Value Imputation.
#'
#' A function to replace \code{NA} in the data frame by \strong{GSimp} method.
#'
#' @inheritParams impute_zero
#' @inheritParams GS_impute_clean
#' @returns A \code{data.frame} with imputed values by \strong{GSimp} method.
#'
#' @details This function and its documentation was copied from
#' https://github.com/WandeRum/GSimp and
#' contains the GSimp algorithm and related functions developed by Rum Wei
#' (10.1371/journal.pcbi.1005973).
#'
#' @examples
#' data(sim_miss)
#' impute_gsimp(sim_miss)
#'
#' @references
#' \insertRef{wei_gsimp_2018}{imputomics}
#'
#' @export
impute_gsimp <- function(missdf,
                         iters_each = 100,
                         iters_all = 20,
                         initial = 'qrilc',
                         lo = -Inf,
                         hi = 'min',
                         imp_model = 'glmnet_pred',
                         gibbs = data.frame(row = integer(), col=integer())) {
  check_missdf(missdf)

  imputed <- GS_impute_clean(data_miss = missdf,
                             iters_each = iters_each,
                             iters_all = iters_all,
                             initial = initial,
                             lo = lo,
                             hi = hi,
                             imp_model = imp_model,
                             gibbs = gibbs)

  imputed[["data_imp"]]
}

#' \strong{VIM kNN} imputation.
#'
#' K Nearest Neighbors. A function to replace \code{NA} in the data frame by
#' [VIM::kNN()].
#'
#' @importFrom VIM kNN
#'
#' @inheritParams impute_zero
#' @param ... other parameters of [VIM::kNN()] besides
#' \code{data}.
#'
#' @returns A \code{data.frame} with imputed values by [VIM::kNN()].
#'
#' @seealso [VIM::kNN()]
#'
#' @examples
#' data(sim_miss)
#' impute_vim_knn(sim_miss)
#'
#' @references
#' \insertRef{kowarik_imputation_2016}{imputomics}
#'
#' @export
impute_vim_knn <- function(missdf, ...) {
  check_missdf(missdf)

  # remove columns with indices of missing values
  VIM::kNN(missdf, ...)[, colnames(missdf)]
}


#' \strong{Mechanism-Aware} imputation.
#'
#' A function to replace \code{NA} in the data frame by [MAI::MAI()] with random
#' forest and single imputation.
#'
#' @importFrom MAI MAI
#'
#' @inheritParams impute_zero
#' @param ... other parameters of [MAI::MAI()] besides \code{data_miss}.
#'
#' @returns A \code{data.frame} with imputed values by [MAI::MAI()].
#'
#' @section Silent defaults:
#' \code{MCAR_algorithm} is set to \code{random_forest},
#' \code{MNAR_algorithm} is set to \code{single} and \code{verbose} is set to
#' \code{FALSE}.
#' @seealso [MAI::MAI()]
#'
#' @examples
#' data(sim_miss_large)
#' impute_mai(sim_miss_large)
#'
#' @references
#' \insertRef{dekermanjian_mechanismaware_2022}{imputomics}
#'
#' @export
impute_mai <- function(missdf, ...) {
  check_missdf(missdf)

  all_args <- extend_arglist(list(...),
                             list(data_miss = missdf),
                             list(MCAR_algorithm = "random_forest",
                                  MNAR_algorithm = "Single",
                                  verbose = FALSE))

  imputed <- data.frame(do.call(MAI::MAI, all_args)[["Imputed_data"]])
  colnames(imputed) <- colnames(missdf)

  imputed
}


#' \strong{RegImpute} imputation.
#'
#' A function to replace \code{NA} in the data frame by imputation using Glmnet
#' ridge regression (RegImpute) from [DreamAI::DreamAI()].
#'
#' @importFrom DreamAI DreamAI
#'
#' @inheritParams impute_zero
#' @param verbose boolean, if \code{TRUE}, prints the typical prompts of
#' [DreamAI::impute.RegImpute()].
#'
#' @inheritParams DreamAI::DreamAI
#'
#' @returns A \code{data.frame} with imputed values by RegImpute.
#'
#' @seealso [DreamAI::DreamAI()], [DreamAI::impute.RegImpute()].
#'
#' @examples
#' data(sim_miss_large)
#' impute_regimpute(sim_miss_large)
#'
#' @export
impute_regimpute <- function(missdf, verbose = FALSE, fillmethod = "row_mean",
                             maxiter_RegImpute = 10, conv_nrmse = 1e-6) {
  check_missdf(missdf)

  imputed <- silence_function(verbose)(DreamAI::impute.RegImpute(data = as.matrix(missdf),
                                                                     fillmethod = fillmethod,
                                                                     maxiter_RegImpute = maxiter_RegImpute,
                                                                     conv_nrmse = conv_nrmse))

  data.frame(imputed)
}


#' \strong{bcv SVD} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [bcv::impute.svd()].
#'
#' @importFrom bcv impute.svd
#'
#' @inheritParams impute_zero
#' @param ... other parameters of [bcv::impute.svd()] besides \code{x}.
#'
#' @returns A \code{data.frame} with imputed values by SVD.
#'
#' @seealso [bcv::impute.svd()]
#'
#' @examples
#' data(sim_miss)
#' impute_bcv_svd(sim_miss)
#'
#' @export
impute_bcv_svd <- function(missdf, ...) {
  check_missdf(missdf)

  imputed <- data.frame(bcv::impute.svd(x = missdf, ...)[["x"]])
  colnames(imputed) <- colnames(missdf)
  imputed
}


#' \strong{imputation kNN} imputation.
#'
#' A function to replace \code{NA} in the data frame by
#' [imputation::kNNImpute()].
#'
#' @importFrom imputation kNNImpute
#'
#' @inheritParams impute_zero
#' @param ... other parameters of [imputation::kNNImpute()] besides \code{x}.
#'
#' @section Silent defaults:
#' \code{verbose} is set to \code{FALSE} and \code{k} to 5 or the number of
#' columns of \code{x} (whichever is smaller).
#' @returns A \code{data.frame} with imputed values by k-nearest neighbors.
#'
#' @seealso [imputation::kNNImpute()]
#'
#' @examples
#' data(sim_miss)
#' impute_imputation_knn(sim_miss)
#'
#' @export
impute_imputation_knn <- function(missdf, ...) {
  check_missdf(missdf)

  all_args <- extend_arglist(list(...),
                             list(x = as.matrix(missdf)),
                             list(verbose = FALSE,
                                  k = min(ncol(missdf), 5)))

  imputed <- do.call(imputation::kNNImpute, all_args)
  data.frame(imputed[["x"]])
}


#' \strong{Compound Minimum} imputation.
#'
#' A function to replace \code{NA} in the data frame by [GMSimpute::GMS.Lasso()]
#'
#' @inheritParams impute_zero
#' @param verbose boolean, if \code{TRUE}, prints the typical prompts of
#' [GMSimpute::GMS.Lasso()]. These prompts contain the information if the algorithm
#' uses TS.Lasso or minimum per compund to impute, so it might be very relevant.
#' @param ... other parameters of [GMSimpute::GMS.Lasso()] besides
#' \code{input_data}.
#' @importFrom GMSimpute GMS.Lasso
#'
#' @returns A \code{data.frame} with imputed values by CM.
#'
#' @seealso [GMSimpute::GMS.Lasso()]
#'
#' @examples
#' data(sim_miss)
#' impute_cm(sim_miss)
#'
#' @export
impute_cm <- function(missdf, verbose = FALSE, ...){
  check_missdf(missdf)

  former_rownames <- rownames(missdf)
  rownames(missdf) <- NULL

  # samples in columns and features in rows, so transposition
  res <- silence_function(verbose)(GMSimpute::GMS.Lasso(t(as.matrix(missdf)), ...))

  # if the function switches to TS.Lasso = TRUE, the result needs to be transposed
  # (but not always)
  # so we detect if the colnames name contains string 'sample'

  if(all(grepl("^sample", colnames(res), fixed = FALSE))) {
    res <- t(res)
    rownames(res) <- former_rownames
  }

  data.frame(res)
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
#' @param M number of iterations.
#'
#' @returns A \code{data.frame} with imputed values by BayesMetab
#'
#' @examples
#' data(sim_miss)
#' impute_bayesmetab(sim_miss)
#' @export
impute_bayesmetab <- function(missdf, M = 100) {
  check_missdf(missdf)

  imputed <- MCMC.Factor(as.matrix(missdf),
                         M = M,
                         miss.pattern = !is.na(missdf),
                         K.max = ncol(missdf))

  data.frame(imputed[[5]])
}




#' \strong{mNMF} imputation.
#'
#' A function to replace \code{NA} in the data frame using
#' metabolomic Non-negative Matrix Factorization.
#'
#' @importFrom NMF nmf.getOption
#'
#' @inheritParams impute_zero
#' @param kgroup the range of k value.
#' @param initialType type of pre-imputation. Possible values are: \code{mean},
#' \code{median}, and \code{zero}.
#'
#' @section k:
#' If k is not defined, it becomes a range between 1 and the minimum of
#' number of columns and number of rows of \code{missdf} as advised in the
#' original article (see References).
#' @returns A \code{data.frame} with imputed values by mNMF.
#' @section Original implementation:
#' This function was adapted from https://github.com/freeoliver-jing/NMF.
#' @references
#' \insertRef{xu_nmfbased_2021}{imputomics}
#' @examples
#' data(sim_miss)
#' impute_mnmf(sim_miss + 100)
#' @export
impute_mnmf <- function(missdf, kgroup = NULL, initialType = "mean") {
  check_missdf(missdf, above_one = TRUE)

  # samples in columns and features in rows


  if(is.null(kgroup)) {
    kgroup <- unique(round(seq(1, min(ncol(missdf), nrow(missdf)),
                               length.out = min(20, nrow(missdf))), 0))
  }


  data.frame(t(nmf_opt(IMP = t(as.matrix(missdf)), kgroup = kgroup, initialType = initialType)))
}
