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
impute_mNMF <- function(missdf, ...) {
  check_missdf(missdf)
  
  # samples in columns and features in rows
  missdf <- t(as.matrix(missdf))
  
  all_args <- extend_arglist(list(...),
                             list(IMP = t(as.matrix(missdf))),
                             list(kgroup = unique(round(seq(1,
                                                            min(ncol(missdf),
                                                                nrow(missdf)),
                                                            length.out = min(20, nrow(missdf))), 0)),
                                  initialType = "mean"))
  
  data.frame(t(do.call(nmf_opt, all_args)))
}
