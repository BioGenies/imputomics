% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amputing.R
\name{insert_MCAR}
\alias{insert_MCAR}
\title{Inserting missing data of MCAR type}
\usage{
insert_MCAR(dat, ratio = 0, thresh = 0.2)
}
\arguments{
\item{dat}{a matrix or data.frame of data to be filled with some NA's.}

\item{ratio}{a number from 0 to 1 denoting the ratio of data to be
exchanged into NA's}

\item{thresh}{a value from 0 to 1: limit value indicating maximum ratio of
missing observations in one column}
}
\value{
A \code{matrix} with NA values inserted.
}
\description{
This function inserts NA's to the provided matrix according to the
MCAR (Missing Completely At Random) pattern.
}
\examples{
set.seed(1)
m <- as.data.frame(matrix(rnorm(50), ncol = 10))
insert_MCAR(m, ratio = 0.1)

}
