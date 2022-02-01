#' Overview of optimization times.
#'
#' @description
#' This function provides an overview of the optimization times with respect to
#' the initialization strategies.
#'
#' @param x An object of class \code{ino}.
#'
#' @return
#' A data frame.
#' @export
#'
#' @examples
#' # optimization_time()
#'
#' @keywords
#' evaluation

optimization_time <- function(x) {

  out <- data.frame(
    "strategy" = sapply(x$optimizations, '[[', "strategy"),
    "time" = sapply(x$optimizations, '[[', "time")
  )

  return(out)
}
