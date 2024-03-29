% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute-pcamethods.R
\name{impute_ppca}
\alias{impute_ppca}
\title{\strong{PPCA} imputation.}
\usage{
impute_ppca(missdf, ...)
}
\arguments{
\item{missdf}{a data frame with missing values to be imputed
containing features in columns and samples in rows.}

\item{...}{other parameters of \code{\link[pcaMethods:pca]{pcaMethods::pca()}} besides \code{method} and
\code{object}.}
}
\value{
A \code{data.frame} with imputed values by \code{\link[pcaMethods:pca]{pcaMethods::pca()}} with
method = "ppca".
}
\description{
Probabilistic Principal Component Analysis.
}
\details{
A function to replace \code{NA} in the data frame by \code{\link[pcaMethods:pca]{pcaMethods::pca()}} with
method = "ppca".
}
\section{Silent verbose}{

If \code{verbose} is not defined in the function call, it is set to
\code{FALSE}.
}

\examples{
data(sim_miss)
impute_ppca(sim_miss)

}
\references{
\insertRef{stacklies_pcamethods_2007}{imputomics}
}
\seealso{
\code{\link[pcaMethods:pca]{pcaMethods::pca()}}
}
