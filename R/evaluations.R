#' Variables overview
#'
#' @description
#' This function provides an overview of the available optimization variables.
#'
#' @param x
#' An object of class \code{ino}.
#'
#' @return
#' A data frame with columns
# TODO: add description
#' \describe{
#'   \item{name}{}
#'   \item{}{}
#' }
#'
#' @export
#'
#' @keywords
#' evaluation

overview_vars <- function(x) {
  if (nruns(x) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  # TODO: add available variables (including global), their type and length
}

#' Summary of initialization runs
#'
#' @description
#' This function gives an overview of the initialization runs in an \code{ino}
#' object.
#'
#' @details
#' The following variables are available for each \code{ino} object:
#' \describe{
#'   \item{.strategy}{the name of the initialization strategy}
#'   \item{.time}{the optimization time}
#'   \item{.optimum}{the function value at the optimum}
#'   \item{.optimizer}{the identifier of the optimizer}
#' }
#'
#' @param object
#' An object of class \code{ino}.
#' @param ...
# TODO: Write documentation
#' Named function for computing statistics.
#' See function ... for an overview.
#'
#' @return
#' A data frame, optimization runs as rows and variables as columns.
#'
#' @keywords evaluation
#'
#' @exportS3Method

summary.ino <- function(object, ...) {
  if (nruns(object) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  # TODO: build data frame from runs results
  structure(opt, class = c("summary.ino", "data.frame"))
}

#' @exportS3Method
#' @noRd
#' @keywords internal
#' @importFrom dplyr mutate_if

print.summary.ino <- function(x, digits = NULL, ...) {
  if (!is.null(digits)) {
    x <- dplyr::mutate_if(x, is.numeric, round, digits = digits)
  }
  print(x)
}

#' Optima overview
#'
#' @description
#' This function provides an overview of the identified optima.
#'
#' @param x
#' An object of class \code{ino}.
#' @param digits
#' The number of decimal places of the optima values. The default is \code{2}.
#'
#' @return
#' A data frame with columns
#' \describe{
#'   \item{optimum}{the unique optima (with respect to \code{digits})}
#'   \item{frequency}{the number of runs the optima was reached}
#' }
#'
#' @export
#'
#' @keywords evaluation

overview_optima <- function(x, digits = 2) {
  if (nruns(x) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  structure(
    data.frame(table(round(sapply(x$runs, `[[`, ".optimum"), digits = digits))),
    names = c("optimum", "frequency")
  )
}
