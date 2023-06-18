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
                             list(data = missdf),
                             list(rng.seed = sample(1L:1e9, 1)))
  
  imputed <- do.call(impute::impute.knn, all_args)

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
#' impute_PEMM(sim_miss)
#'
#' @references
#' \insertRef{chen_penalized_2014}{imputomics}
#'
#' @export
impute_PEMM <- function(missdf, phi = 1) {
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
#' @details  This function was copied from https://github.com/WandeRum/GSimp and
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
#'
impute_vim_knn <- function(missdf, ...) {
  check_missdf(missdf)
  
  imputed <- VIM::kNN(missdf, ...)[,1:ncol(missdf)]
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

