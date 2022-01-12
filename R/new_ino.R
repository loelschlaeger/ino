#' Construction of \code{ino} objects.
#'
#' This function constructs an object of class \code{ino}.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @export
#'
#' @keywords
#' internal
#'
#' @examples
#' new_ino()

new_ino <- function () {

  out <- list()

  ### components
  out$f <- NA
  out$data <- list()
  out$optimizer <- list()
  out$optima <- list()

  class(out) <- "ino"
  return(out)

}

#' @export
#' @noRd

print.ino <- function(x, ...) {
  cat("<ino>")
}
