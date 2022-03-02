#' Set data for the numerical optimization of likelihood functions.
#'
#' @param x
#' Either \code{NULL} or an object of class \code{ino}.
#'
#' @param data
#' A list of data sets.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @export
#'
#' @keywords
#' specification


set_data <- function(x, data) {

  ### initialize ino object
  if (is.null(x)) {
    x <- new_ino()
  }

  ### check inputs
  if (class(data) != "list") {
    stop("'data' must be of class list.")
  }

  ### add data to ino object
  x[["data"]] <- data

  ### return ino
  return(x)
}
