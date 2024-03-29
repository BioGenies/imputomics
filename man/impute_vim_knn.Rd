% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputing.R
\name{impute_vim_knn}
\alias{impute_vim_knn}
\title{\strong{VIM kNN} imputation.}
\usage{
impute_vim_knn(missdf, ...)
}
\arguments{
\item{missdf}{a data frame with missing values to be imputed
containing features in columns and samples in rows.}

\item{...}{other parameters of \code{\link[VIM:kNN]{VIM::kNN()}} besides
\code{data}.}
}
\value{
A \code{data.frame} with imputed values by \code{\link[VIM:kNN]{VIM::kNN()}}.
}
\description{
K Nearest Neighbors. A function to replace \code{NA} in the data frame by
\code{\link[VIM:kNN]{VIM::kNN()}}.
}
\examples{
data(sim_miss)
impute_vim_knn(sim_miss)

}
\references{
\insertRef{kowarik_imputation_2016}{imputomics}
}
\seealso{
\code{\link[VIM:kNN]{VIM::kNN()}}
}
