#' Constructor of \code{ino} objects.
#'
#' This function constructs an object of class \code{ino}.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @keywords
#' internal

new_ino <- function() {
  out <- list()

  ### components
  out$f <- NA
  out$data <- list()
  out$optimizer <- list()
  out$optimizations <- list()

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
  return(list("f_set" = !identical(object$f,NA),
              "no_data" = length(object$data),
              "no_optimizer" = length(object$optimizer),
              "no_optimizations" = length(object$optimizations)))
}

#' @export
#' @noRd

print.summary.ino <- function(x, ...) {
  print(x)
}
