sample_indices <- function(x) x[sample(length(x), size = 1)]

get_missing_per_column <- function(dat, ratio = 0, thresh = 0.2) {
  total_missing <- round(nrow(dat) * ncol(dat) * ratio, 0)
  
  thresh_value <- ceiling(thresh * nrow(dat))
  nonmissing_value <- thresh_value - nrow(dat)
  
  if(total_missing > thresh_value * ncol(dat)) {
    stop(paste0("The total number of required missing values (", total_missing,
                ") is larger than the number of missing values allowed by threshold (",
                thresh_value * ncol(dat), ")"))
  }
  
  missing_per_column <- rmultinom(1, total_missing, rep(1/ncol(dat), ncol(dat)))[, 1]
  
  diffs <- missing_per_column - thresh_value
  
  while(any(diffs > 0)) {
    random_neg_column <- sample_indices(which(diffs < 0 & diffs >= nonmissing_value))
    random_pos_column <- sample_indices(which(diffs > 0))
    
    missing_per_column[random_neg_column] <- missing_per_column[random_neg_column] + 1
    missing_per_column[random_pos_column] <- missing_per_column[random_pos_column] - 1
    
    diffs <- missing_per_column - thresh_value
  }
  
  missing_per_column
}

######## MCAR

#' Inserting missing data of MCAR type
#'
#' This function inserts NA's to the provided matrix according to the
#' MCAR (Missing Completely At Random) pattern.
#'
#' @importFrom mice ampute
#'
#' @param dat a matrix or data.frame of data to be filled with some NA's.
#' @param ratio a number from 0 to 1 denoting the ratio of data to be
#' exchanged into NA's
#' @inheritParams insert_MCAR
#'
#' @returns A \code{matrix} with NA values inserted.
#'
#' @examples
#' set.seed(1)
#' m <- as.data.frame(matrix(rnorm(50), ncol = 10))
#' insert_MCAR(m, ratio = 0.1)
#'
#' @export insert_MCAR
#'
insert_MCAR <- function(dat, ratio = 0, thresh = 0.2) {
  
  missing_per_column <- get_missing_per_column(dat, ratio = ratio, thresh = thresh)
  
  res <- dat  
    
  for(i in 1L:ncol(dat)) {
    res[sample.int(n = nrow(dat), size = missing_per_column[i]), i] <- NA
  }
  
  res
}

######## MAR

#' Inserting missing data of MAR type
#'
#' This function inserts NA's to the provided matrix according to the
#' MAR (Missing At Random) patterns.
#'
#' @inheritParams insert_MCAR
#'
#' @details This function uses \code{\link[mice]{ampute}}. It firstly tries to
#' ampute missing data by metabolites (columns) and if it fails, it switches to
#' introduce missing values by samples (rows).
#'
#' @returns A \code{matrix} with NA values inserted.
#'
#' @examples
#' set.seed(1)
#' m <- as.data.frame(matrix(rnorm(10), 50, 5))
#' insert_MAR(m, ratio = 0.1)
#'
#' @export insert_MAR
#'
insert_MAR <- function(dat, ratio = 0) {
  # pattern <- matrix(sample(c(0, 1),
  #                          ncol(dat)*2,
  #                          replace = TRUE,
  #                          prob = c(0.7, 0.3)),
  #                   ncol = ncol(dat))
  
  res <- try(mice::ampute(data = dat, prop = ratio, 
                                         mech = "MAR", bycases = FALSE)[["amp"]], silent = TRUE)
  
  if(inherits(res, "try-error"))
    res <- mice::ampute(data = dat, prop = ratio, 
                                       mech = "MAR", bycases = TRUE)[["amp"]]
  
  res
}


######## MNAR

#' Inserting missing data of MNAR type
#'
#' This function inserts NA's to the provided metabolomic matrix according to
#' the MNAR (Missing Not At Random) mechanism.
#'
#' @inheritParams insert_MCAR
#'
#' @param thresh a value from 0 to 1: limit value indicating maximum ratio of
#' missing observations in one column
#'
#' @details LOD missing data is simulated by sampling possible limit of
#' detection (LOD) for each metabolite and truncates the observations below this
#' values. Thus, each metabolite has different truncation threshold. However,
#' all the removed data corresponds to the provided fraction.
#'
#' @returns A \code{matrix} with NA values inserted.
#'
#' @examples
#' set.seed(1)
#' m <- as.data.frame(matrix(rnorm(200), 10, 20))
#' insert_MNAR(m, ratio = 0.1)
#'
#' @export insert_MNAR
#'

insert_MNAR <- function(dat, ratio = 0.1, thresh = 0.2) {
  n <- ncol(dat)
  sum_value <- n * ratio
  ratio_cols <- runif(n - 1, 0, 0.9)
  p_vec <- diff(sort(c(ratio_cols, 0, 1))) * sum_value

  while(any(p_vec >= thresh)) {
    excess <- (sum_value - sum(p_vec[!(p_vec >= thresh)])) / n
    p_vec[p_vec >= thresh] <- 0
    p_vec <- p_vec + excess
  }

  do.call(cbind,
          lapply(1L:ncol(dat), function(ith_col_id) {
            ith_col <- dat[, ith_col_id]
            ith_col[ith_col < quantile(ith_col, p_vec[[ith_col_id]])] <- NA
            ith_col
          })
  )
}

######## simulate scenario

#' Inserting missing data
#'
#' This function inserts NA's to the provided metabolomic matrix based on the
#' MCAR (Missing Completely At Random), MAR (Missing At Random) and MNAR
#' (Missing Not At Random) patterns according to provided probabilities.
#'
#' @inheritParams insert_MCAR
#' @param mcar_ratio a number from (0, 1) interval. Ratio of the data missing
#' completely at random (MCAR).
#' @param mar_ratio a number from (0, 1) interval. Ratio of the data missing
#' at random (MAR).
#' @param mnar_ratioa number from (0, 1) interval. Ratio of the data missing
#' not at random (MCAR).
#'
#' @details This function uses \code{\link[mice]{ampute}} for simulating
#' the data MCAR and MAR, and \code{\link[imputomics]{insert_MNAR}}
#' implementation for simulation the data missing because of the limit of
#' detection (LOD).
#'
#' It's a wrapper for the following functions:
#'
#' - \code{\link[imputomics]{insert_MCAR}},
#'
#' - \code{\link[imputomics]{insert_MAR}},
#'
#' - \code{\link[imputomics]{insert_MNAR}}.
#'
#' The sum of \code{mcar_ratio} + \code{mar_ratio} + \code{mnar_ratio} should
#' not surpass 1, otherwise the function will throw an error.
#'
#' Note that the missing mechanisms are used in the following order: MAR, MNAR,
#' MCAR. It may happen that some of missing values will overlap themselves and
#' in the result missing ratio may be slightly smaller.
#'
#' @returns A \code{matrix} with NA values inserted.
#'
#' @seealso \code{\link[imputomics]{insert_MCAR}},
#' \code{\link[imputomics]{insert_MAR}},
#' \code{\link[imputomics]{insert_MNAR}}
#'
#' @examples
#' set.seed(1)
#' m <- as.data.frame(matrix(rnorm(10), 5, 2))
#' simulate_miss_value(m, mcar = 0.1, mar = 0.05, mnar = 0.15)
#'
#' @export
#'
simulate_miss_value <- function(data_set,
                                mcar = 0,
                                mar = 0,
                                mnar = 0){
  if(!("data.frame" %in% class(data_set)))
    stop("Variable data should be a data.frame")

  if ((mcar + mar + mnar > 1) || any(c(mcar, mar, mnar) < 0))
    stop("Sum of mcar, mar and mnar should be between 0 and 1.")

  amputed_mar <- amputed_mnar <- amputed_mcar <- data_set

  if(mar > 0)
    amputed_mar <- insert_MAR(data_set, mar)
  if(mnar > 0)
    amputed_mnar <- insert_MNAR(data_set, mnar)
  if(mcar > 0)
    amputed_mcar <- insert_MCAR(data_set, mcar)

  data_set[is.na(amputed_mar)] <- NA
  data_set[is.na(amputed_mnar)] <- NA
  data_set[is.na(amputed_mcar)] <- NA

  data_set
}


