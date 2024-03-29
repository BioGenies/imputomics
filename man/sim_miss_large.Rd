% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{sim_miss_large}
\alias{sim_miss_large}
\title{Much larger simulated data for examples}
\usage{
sim_miss_large
}
\description{
Much larger simulated data for examples
}
\examples{
# code used to generate the data
set.seed(1758)
df <- data.frame(matrix(runif(3000), ncol = 60))
sim_miss_large <- insert_MCAR(df, ratio = 0.25)
}
\keyword{datasets}
