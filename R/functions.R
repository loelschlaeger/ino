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
#' ### The global minimum
#' f_ackley(c(0,0))
#'
#' @return
#' A numeric value.

f_ackley <- function(x) {
  -20*exp(-0.2*sqrt(0.5*(x[1]^2+x[2]^2))) -
    exp(0.5*(cos(2*pi*x[1])+cos(2*pi*x[2])))+exp(1)+20
}
