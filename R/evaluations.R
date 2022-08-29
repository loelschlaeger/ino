overview_pars <- function(x) {
  out_1 <- out_2 <- data.frame(name = NULL)
  table <- x$runs$table
  out_1$name <- colnames(table)
  for (n in out_1$name) {

  }
  pars <- x$runs$pars
  out_2 <- unique(unlist(lapply(pars, names)))
  for (n in out_2$name) {

  }
  rbind(out_1, out_2)
}

rename_pars <- function(x, old_name, new_name, merge = TRUE) {

}


#' Summary of initialization runs
#'
#' @description
#' This function gives an overview of the initialization runs in an \code{ino}
#' object.
#'
#' @details
#' The following values are available for each \code{ino} object:
#' * \code{.strategy}, the name of the initialization strategy
#' * \code{.time}, the optimization time
#' * \code{.optimum}, the function value at the optimum
#' * \code{.optimizer}, the identifier of the optimizer
#'
#' @param object
#' An object of class \code{ino}.
#' @param group
#' A character vector for grouping the optimization results, or \code{NULL}
#' (default) for no grouping.
#' @param ...
#' Named functions for computing statistics. Ignored if \code{group = NULL}.
#' For example
#' \preformatted{
#' dgp = function(global, par) sqrt(sum((global - par) ^ 2))
#' }
#'
#' @return
#' A data frame.
#'
#' @keywords
#' evaluation
#'
#' @export

summary.ino <- function(object, ...) {
  if (nrow(object$runs$table) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  structure(opt, class = c("summary.ino", "data.frame"))
}

#' @noRd
#' @export
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
#' The number of digits of the optima values.
#'
#' @return
#' A data frame.
#'
#' @export
#'
#' @keywords
#' evaluation

overview_optima <- function(x, digits = 2) {
  if (nrow(x$runs$table) == 0) {
    ino_warn(
      event = "No records found.",
      debug = "Run some initialization strategies first."
    )
    return(invisible(NULL))
  }
  structure(
    as.data.frame(table(round(x$runs$table[[".optimum"]], digits = digits))),
    names = c("optimum", "frequency")
  )
}
