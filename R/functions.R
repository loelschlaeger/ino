#' The Ackley function
#'
#' @references
#' https://en.wikipedia.org/wiki/Ackley_function
#'
#' @details
#' The function has multiple local minima and one global minimum in the origin.
#'
#' @param x
#' A numeric vector of length 2.
#'
#' @examples
#' ino:::f_ackley(c(0, 0))
#'
#' @return
#' A numeric value.
#'
#' @keywords
#' function

f_ackley <- function(x) {
  stopifnot(is.numeric(x), length(x) == 2)
  -20 * exp(-0.2 * sqrt(0.5 * (x[1]^2 + x[2]^2))) - exp(0.5 * (cos(2 * pi * x[1]) + cos(2 * pi * x[2]))) + exp(1) + 20
}

#' The log-likelihood function of the mixed probit model
#'
#' @references
#' https://en.wikipedia.org/wiki/Probit_model
#'
#' @details
#' The {ino} package includes a data set that was sampled from a mixed probit
#' model, which can be accessed via \code{data()}.
#'
#' @param theta
#' A numeric vector of model parameters.
#' @param data
#' A data frame of discrete choice data.
#'
#' @examples
#' b <- c(-1,1)
#' Omega <- diag(2)
#' Sigma <- diag(2)
#' O <- t(chol(Omega))
#' L <- t(chol(Sigma))
#' theta <- c(b, O[lower.tri(O)], L[lower.tri(L)])
#'
#' @return
#' A log-likelihood value.
#'
#' @keywords
#' function

f_ll_mmnp <- function(theta, data) {
  stopifnot(is.numeric(theta), is.data.frame(data), "choice" %in% colnames(data))
  warning("Not implemented yet.")
}
