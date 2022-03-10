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
  if(!is.list(at)){
    stop("'at' must be a list.")
  }

  ### generate grid of all combinations of starting values and data sets
  # if data sets exist, create a grid of starting values and data sets
  if(is.list(x$data)) {
    out <- expand.grid(at, 1:length(x$data))
    colnames(out) <- c("at", "data_idx")
  }
  # if no data sets exist, simply return the starting values
  else{
    out <- data.frame(at = rep(NA, length(at)))
    out$at <- at
  }

  ### return the grid as data frame
  return(out)
}
