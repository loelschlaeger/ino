#' Set a function for the numerical optimization.
#'
#' @param x Either \code{NULL} or an object of class \code{ino}.
#' @param f An object of class \code{function}.
#' @param npar The number of parameters in \code{f}.
#' @param ... Additional arguments to be passed to \code{f}.
#'
#' @return An object of class \code{ino}.
#' @export
#'
#' @examples
#' himmelblau <- function(x, c, d) (x[1]^2 + x[2] + c)^2 + (x[1] + x[2]^2 + d)^2
#' set_f(f = himmelblau, npar = 2, c = -11, d = -7)
#'
#' @keywords
#' specification

set_f <- function(x = NULL, f, npar, ...) {

  ### initialize ino
  if (is.null(x)) {
    x <- new_ino()
  }

  ### check inputs
  if (class(f) != "function") {
    stop("'f' must be of class function.")
  }
  if (!is.numeric(npar) || npar %% 1 != 0 || npar < 1) {
    stop("'npar' must be a number.")
  }

  ### add optimizer to ino
  x$f <- list(f = f, npar = npar, add = list(...))

  ### return ino
  return(x)
}
