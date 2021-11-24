#' Set a function for the numerical optimization.
#'
#' @param f An object of class \code{function}.
#' @param npar The number of parameters in \code{f}.
#'
#' @return An object of class \code{ino_f}.
#' @export
#'
#' @examples
#' f <- function(x) x
#' set_f(f, 1)
set_f <- function(f, npar) {
  if(class(f) != "function")
    stop("'f' must be of class function.")
  if(!is.numeric(npar) || npar%%1 != 0 || npar < 1)
    stop("'npar' must be a number.")
  out <- list(f = f, npar = npar)
  class(out) <- "ino_f"
  return(out)
}
