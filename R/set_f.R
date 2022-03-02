#' Set a function for the numerical optimization
#'
#' @param x
#' Either \code{NULL} or an object of class \code{ino}.
#' @param f
#' An object of class \code{function}.
#' @param npar
#' The number of parameters in \code{f}.
#' @param ...
#' Additional arguments to be passed to \code{f}.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @export
#'
#' @examples
#' himmelblau <- function(x, c, d) (x[1]^2 + x[2] + c)^2 + (x[1] + x[2]^2 + d)^2
#' set_f(f = himmelblau, npar = 2, c = -11, d = -7)
#'
#' @keywords
#' specification

set_f <- function(x = NULL, f, npar, ...) {

  if (class(f) != "function") {
    stop("'f' must be of class function.")
  }
  if (!is.numeric(npar) || npar %% 1 != 0 || npar < 1) {
    stop("'npar' must be a number.")
  }

  add_f <- list(f = f, npar = npar, add = list(...))

  if (is.null(x)) {
    ### initialize ino object
    x <- new_ino()
    x[["f"]] <- add_f
  } else {
    ### ino object already exists
    if (class(x) != "ino") {
      stop("'x' must be of class 'ino'.")
    }
    if (identical(x[["f"]], NA)) {
      ### add f first time
      x[["f"]] <- add_f
    } else {
      ### add f again
      cat("The ino object already contains a function, what to do?\n")
      cat("1: Cancel\n")
      cat("2: Replace the old function by the new function\n")
      cat("\n")
      input <- readline(prompt = "Action: ")
      if(input == 1){
        return(x)
      } else if (input == 2) {
        x[["f"]] <- add_f
      }
    }
  }

  return(x)
}
