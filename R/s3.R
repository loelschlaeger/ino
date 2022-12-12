#' @noRd
#' @exportS3Method

print.Nop <- function(x, ...) {
  x$print()
}

#' @noRd
#' @exportS3Method

summary.Nop <- function(object, ...) {
  object$summary()
}

#' @noRd
#' @exportS3Method

print.summary.Nop <- function(x, ...) {
  print(x)
}

#' @noRd
#' @exportS3Method

plot.Nop <- function(x, ...) {
  x$plot()
}
