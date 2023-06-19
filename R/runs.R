#' Runs Object (R6 Class)
#'
#' @description
#' A \code{Runs} object stores results of numerical optimization runs. It is
#' typically contained inside a \code{\link{Nop}} object.
#'
#' @param digits
#' An \code{integer}, the number of shown decimal places.
#' The default is \code{2}.
#'
#' @return
#' A \code{Runs} object, which is an R6 class that stores results of numerical
#' optimization runs, see the details.

Runs <- R6::R6Class(
  classname = "Runs",
  public = list(

    #' @description
    #' Creates a new \code{Runs} object.
    #' @return
    #' A new \code{Runs} object.
    initialize = function() {

    },

    #' @description
    #' Prints details of the stored numerical optimization results.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Results} object.
    print = function(digits = getOption("ino_digits", default = 2), ...) {

    }
  ),

  active = list(

  ),

  private = list(

  )
)
