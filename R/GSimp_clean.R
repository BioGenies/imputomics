##################################################################################
#### This function was copied from https://github.com/WandeRum/GSimp
#### and contains the GSimp algorithm and related functions developed by
#### Rum Wei (10.1371/journal.pcbi.1005973).
##################################################################################

#require(missForest)
#require(imputeLCMD)
#require(foreach)
#require(doParallel)
#require(MASS)

#' Supplementary function for GSimp
#' Draw n samples from a truncated normal distribution \code{N(mu, std^2|lo, hi)}
#'
#' @inheritParams GS_impute_clean
#'
#' @keywords internal

rnorm_trunc <- function (n, mu, std, lo = -Inf, hi = Inf) {
  p_lo <- pnorm(lo, mu, std)
  p_hi <- pnorm(hi, mu, std)
  p_hi[p_hi < .01] <- .01
  u <- runif(n, p_lo, p_hi)
  return(qnorm(u, mu, std))
}

#' Supplementary function for GSimp
#' Initialize the missing data
#' lsym will draw samples from the right tail of the distribution and transformed to the left tail
#'
#' @inheritParams GS_impute_clean
#'
#' @keywords internal

miss_init <- function(miss_data,
                      method = c('lsym', 'qrilc', 'rsym')[1]) {
  init_data <- miss_data
  switch (method,
          lsym = {
            init_data <- apply(init_data, 2, function(ith_col){
              na_idx <- which(is.na(ith_col))
              prop <- mean(is.na(ith_col))
              min_temp <- min(ith_col, na.rm=TRUE)
              ith_col[na_idx] <- min_temp - 1
              med_temp <- median(ith_col)
              ith_col[na_idx] <- med_temp -
                (sample(ith_col[ith_col >= quantile(ith_col, 1-prop)],
                        length(na_idx),
                        replace = TRUE) - med_temp)
              ith_col
            })
          },
          rsym = {
            init_data <- apply(init_data, 2, function(ith_col){
              na_idx <- which(is.na(ith_col))
              prop <- mean(is.na(ith_col))
              max_temp <- max(ith_col, na.rm=TRUE)
              ith_col[na_idx] <- max_temp + 1
              med_temp <- median(ith_col)
              ith_col[na_idx] <- med_temp +
                (med_temp - sample(ith_col[ith_col <= quantile(ith_col, prop)],
                                   length(na_idx),
                                   replace=TRUE))
              ith_col
            })
          },
          qrilc = {
            init_data <- imputeLCMD::impute.QRILC(miss_data)[[1]]
          }
  )
  return(init_data)
}

#' Supplementary function for GSimp
#' Single missing variable imputation based on Gibbs sampler
#'
#' @importFrom missForest nrmse
#'
#' @inheritParams GS_impute_clean
#'
#' @keywords internal
#'
single_impute_iters <- function(x,
                                y,
                                y_miss,
                                y_real = NULL,
                                imp_model = 'glmnet_pred',
                                lo = -Inf,
                                hi = Inf,
                                iters_each = 100,
                                gibbs = c()) {
  y_res <- y
  x <- as.matrix(x)
  na_idx <- which(is.na(y_miss))
  imp_model_func <- getFunction(imp_model)
  nrmse_vec <- c()
  gibbs_res <- array(NA, dim = c(3, length(gibbs), iters_each))
  dimnames(gibbs_res) <- list(c('std', 'yhat', 'yres'), NULL, NULL)

  for (i in 1:iters_each) {
    y_hat <- imp_model_func(x, y_res)
    std <- sqrt(sum((y_hat[na_idx]-y_res[na_idx])^2)/length(na_idx))
    y_res[na_idx] <- rnorm_trunc(length(na_idx), y_hat[na_idx], std, lo, hi)
    if (length(gibbs)>0) {
      gibbs_res[1, , i] <- std
      gibbs_res[2, , i] <- y_hat[gibbs]
      gibbs_res[3, , i] <- y_res[gibbs]
    }
    ## The following code is for prediction function testing when y_real availabe ##
    if (!is.null(y_real)) {
      Sys.sleep(.5)
      par(mfrow=c(2, 2))
      nrmse_vec <- c(nrmse_vec, missForest::nrmse(y_res, y_miss, y_real))
      plot(y_real~y_res)
      plot(y_real~y_hat)
      plot(y_hat~y_res)
      plot(nrmse_vec)
    }
  }
  return(list(y_imp=y_res, gibbs_res=gibbs_res))
}


#' Supplementary function for GSimp
#' Multiple missing variables imputation
#'
#' @param data_miss data to impute
#' @param iters_each number (100); vector of numbers, e.g. rep(100, 20)
#' @param iters_all = 20
#' @param lo number; vector; functions like min/max/median/mean
#' @param hi number; vector; functions like min/max/median/mean
#' @param imp_model = \code{glmnet_pred},
#' @param initial character ('qrilc'/'lysm') initialized data matrix
#' @param gibbs = data.frame(row = integer(), col=integer())
#'
#' @keywords internal
#'
#'
GS_impute_clean <- function(data_miss,
                            iters_each = 100,
                            iters_all = 20,
                            initial = 'qrilc',
                            lo = -Inf,
                            hi = 'min',
                            imp_model = 'glmnet_pred',
                            gibbs = data.frame(row = integer(), col=integer())) {

  ## Make vector for iters_each ##
  if (length(iters_each) == 1) {
    iters_each <- rep(iters_each, iters_all)
  } else if (length(iters_each) == iters_all) {
    iters_each <- iters_each
  } else {stop('improper argument: iters_each')}

  ## Missing count in each column ##
  miss_count <- apply(data_miss, 2, function(x) sum(is.na(x)))
  ## Index of missing variables, sorted (increasing) by the number of missings
  miss_col_idx_temp <- order(miss_count, decreasing = TRUE)
  miss_col_idx <- rev(miss_col_idx_temp[1:sum(miss_count!=0)])

  # if (!all(gibbs$col %in% miss_col_idx)) {stop('improper argument: gibbs')} # tutaj pozbyc się %in% ale jak
  if (!all(setdiff(gibbs$col, miss_col_idx))) {stop('improper argument: gibbs')}
  gibbs_sort <- gibbs
  if (nrow(gibbs_sort)>0) {
    gibbs_sort$order <- c(1:nrow(gibbs_sort))
    gibbs_sort <- gibbs_sort[order(gibbs_sort$row), ]
    gibbs_sort <- gibbs_sort[order(match(gibbs_sort$col, miss_col_idx)), ]
  } else {gibbs_sort$order <- integer()}

  ## Make vectors for lo and hi ##
  if (length(lo)>1) {
    if (length(lo)!=ncol(data_miss))
      stop('Length of lo should equal to one or the number of variables')
    else {lo_vec <- lo}
  } else if (is.numeric(lo)) {
    lo_vec <- rep(lo, ncol(data_miss))
  } else if (is.character(lo)) {
    lo_fun <- getFunction(lo)
    lo_vec <- apply(data_miss, 2, function(x) lo_fun(na.omit(x)))
  }

  if (length(hi)>1) {
    if (length(hi)!=ncol(data_miss))
      stop('Length of hi should equal to one or the number of variables')
    else {hi_vec <- hi}
  } else if (is.numeric(hi)) {
    hi_vec <- rep(hi, ncol(data_miss))
  } else if (is.character(hi)) {
    hi_fun <- getFunction(hi)
    hi_vec <- apply(data_miss, 2, function(x) hi_fun(na.omit(x)))
  }

  # Check whether lo is lower than hi
  if(!all(lo_vec < hi_vec)) {stop('lo should be lower than hi')}

  ## Initialization using build-in method or input initial matrix ##
  if(is.character(initial)) {
    data_init <- miss_init(data_miss, method=initial)
  } else if(is.data.frame(initial) & identical(data_miss[!is.na(data_miss)],
                                               initial[!is.na(data_miss)])) {
    data_init <- initial
  } else {stop('improper argument: initial')}

  data_imp <- data_init
  gibbs_res_final <- array(NA, dim=c(3, nrow(gibbs), 0))

  ## Iterations for the whole data matrix ##
  for (i in 1:iters_all) {
    gibbs_res_j <- array(NA, dim=c(3, 0, iters_each[i]))
    for (j in miss_col_idx) {
      gibbs_sort_temp <- gibbs_sort[gibbs_sort$col==j, ]
      y_miss <- data_miss[, j]
      y_imp_res <- single_impute_iters(data_imp[, -j], data_imp[, j], y_miss,
                                       imp_model = imp_model, lo = lo_vec[j],
                                       hi = hi_vec[j], iters_each = iters_each[i],
                                       gibbs = gibbs_sort_temp$row)
      y_imp <- y_imp_res$y_imp
      gibbs_res_j <- abind::abind(gibbs_res_j, y_imp_res$gibbs_res, along=2)
      data_imp[is.na(y_miss), j] <- y_imp[is.na(y_miss)]
    }
    gibbs_res_final <- abind::abind(gibbs_res_final, gibbs_res_j, along=3)
  }
  gibbs_res_final_reorder <- gibbs_res_final[, gibbs_sort$order, ]
  return(list(data_imp=data_imp, gibbs_res=gibbs_res_final_reorder))
}

#' Supplementary function for GSimp
#'
#' @keywords internal
#'

glmnet_pred <- function(x, y, alpha=.5, lambda=.01) {
  x_mat <- as.matrix(x)
  model <- glmnet::glmnet(x=x_mat, y=y, alpha=alpha, lambda=lambda)
  y_hat <- predict(model, newx=x_mat)[, 1]
  return(y_hat)
}

# Parallel combination ----------------------------------------------------

#' Supplementary function for GSimp
#'
#' @keywords internal
#'

cbind_abind <- function(a, b) {
  res <- list()
  res$y_imp <- cbind(a$y_imp, b$y_imp)
  res$gibbs_res <- abind::abind(a$gibbs_res, b$gibbs_res, along=2)
  return(res)
}
