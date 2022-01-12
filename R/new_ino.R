#' Construction of \code{ino} objects.
#'
#' This function constructs an object of class \code{ino}.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @keywords
#' internal

new_ino <- function () {

  out <- list()

  ### components
  out$f <- list()
  out$data <- list()
  out$optimizer <- list()
  out$optimizations <- list()
  out$optima <- list()

  class(out) <- "ino"
  return(out)

}

#' @export
#' @noRd

print.ino <- function(x, ...) {
  cat("<ino>")
}

#' @export
#' @noRd

summary.ino <- function(object, ...) {
  stop("Not implemented yet.")
}

