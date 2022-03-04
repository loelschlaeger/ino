#' Saving of optimization results.
#'
#' @description
#' This function saves optimization results in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param strategy
#' A character, the label for the strategy.
#' @param res
#' A list object, the optimization output.
#' @param time
#' An object of class \code{difftime}, the optimization time.
#'
#' @return
#' An object of class \code{ino}.
#'
#' @keywords
#' internal

save_optimization_results <- function(x, strategy, res, time) {
  new_results <- list("strategy" = strategy, "res" = res, "time" = time)
  if (identical(x[["optimizations"]], NA)) {
    ### first optimization
    x[["optimizations"]] <- list()
    x[["optimizations"]][[1]] <- new_results
  } else {
    ### not first optimization
    x[["optimizations"]] <- c(x[["optimizations"]], list(new_results))
  }
  return(x)
}
