% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputing.R
\name{impute_gsimp}
\alias{impute_gsimp}
\title{\strong{GSimp} imputation.}
\usage{
impute_gsimp(
  missdf,
  iters_each = 100,
  iters_all = 20,
  initial = "qrilc",
  lo = -Inf,
  hi = "min",
  imp_model = "glmnet_pred",
  gibbs = data.frame(row = integer(), col = integer())
)
}
\arguments{
\item{missdf}{a data frame with missing values to be imputed
containing features in columns and samples in rows.}

\item{iters_each}{number (100); vector of numbers, e.g. rep(100, 20)}

\item{iters_all}{= 20}

\item{initial}{character ('qrilc'/'lysm') initialized data matrix}

\item{lo}{number; vector; functions like min/max/median/mean}

\item{hi}{number; vector; functions like min/max/median/mean}

\item{imp_model}{= \code{glmnet_pred},}

\item{gibbs}{= data.frame(row = integer(), col=integer())}
}
\value{
A \code{data.frame} with imputed values by \strong{GSimp} method.
}
\description{
Gibbs Sampler Based Left-Censored Missing Value Imputation.
}
\details{
A function to replace \code{NA} in the data frame by \strong{GSimp} method.

This function and its documentation was copied from
https://github.com/WandeRum/GSimp and
contains the GSimp algorithm and related functions developed by Rum Wei
(10.1371/journal.pcbi.1005973).
}
\examples{
data(sim_miss)
impute_gsimp(sim_miss)

}
\references{
\insertRef{wei_gsimp_2018}{imputomics}
}
