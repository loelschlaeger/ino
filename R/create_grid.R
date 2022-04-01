#' Create grid
#'
#' @description
#' This function creates a grid of all combinations of starting values, data sets, and arguments considered for
#' optimisation.
#'
#' @details
#' See the vignette "Initialization strategies" for more details.
#'
#' @param x
#' An object of class \code{ino}.
#' @param at
#' A list containing the initial values.
#'
#' @return
#' A data frame.
#'
#' @keywords
#' internal

create_grid <- function(x, at) {

  ### check inputs
  if (class(x) != "ino") {
    stop("'x' must be of class 'ino'.")
  }
  if (!is.list(at)) {
    stop("'at' must be a list.")
  }

  ### generate grid of all combinations of starting values, data sets, and further argument specifications
  ## four cases:
  ## 1. no data set and no further argument specifications
  ## 2. one (or multiple) data sets and no further argument specifications
  ## 3. no data set and further argument specifications
  ## 4. one (or multiple) data sets and further argument specifications

  # 1.
  if (!is.list(x$data) & !(any(lapply(x$f$add, length) > 1))) {
    out <- data.frame(at = rep(NA, length(at)))
    out$at <- at
  }
  # 2.
  if (is.list(x$data) & (any(lapply(x$f$add, length) <= 1))) {
    out <- expand.grid(at, 1:length(x$data))
    colnames(out) <- c("at", "data_idx")
  }
  # 3.
  if (!is.list(x$data) & (any(lapply(x$f$add, length) > 1))) {
    # name of argument for which multiple values are given
    des_argument <- names(which(lapply(x$f$add, length) > 1))
    out <- expand.grid(at, x$f$add[[des_argument]])
    colnames(out) <- c("at", "argument_value")
  }
  # 4.
  if (is.list(x$data) & (any(lapply(x$f$add, length) > 1))) {
    # name of argument for which multiple values are given
    des_argument <- names(which(lapply(x$f$add, length) > 1))
    out <- expand.grid(at, 1:length(x$data), x$f$add[[des_argument]])
    colnames(out) <- c("at", "data_idx", "argument_value")
  }

  ### return grid as data frame
  return(out)
}
