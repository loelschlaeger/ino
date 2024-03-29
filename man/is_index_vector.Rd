% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_index_vector}
\alias{is_index_vector}
\title{Check for proper index vector}
\usage{
is_index_vector(x, error = TRUE)
}
\arguments{
\item{x}{Any object.}

\item{error}{In the case that \code{x} is not a proper index vector, either \code{TRUE}
(default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.}
}
\value{
If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
}
\description{
This function checks whether the input is proper index vector, i.e., a
\code{vector} of positive \code{integer} values.
}
\keyword{utils}
