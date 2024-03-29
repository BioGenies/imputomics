% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute-constant-column.R
\name{impute_random}
\alias{impute_random}
\title{\strong{random} imputation.}
\usage{
impute_random(missdf)
}
\arguments{
\item{missdf}{a data frame with missing values to be imputed
containing features in columns and samples in rows.}
}
\value{
A \code{data.frame} with imputed values by \strong{random} method.
}
\description{
A function to replace \code{NA} in the data frame by  random values.
}
\examples{
data(sim_miss)
impute_random(sim_miss)

}
