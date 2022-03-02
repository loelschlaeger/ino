#' Specify the numerical optimizer
#'
#' @param x
#' Either \code{NULL} or an object of class \code{ino}.
#' @inheritParams new_optimizer
#'
#' @return
#' An object of class \code{ino}.
#'
#' @export
#'
#' @keywords
#' specification
#'
#' @examples
#' set_optimizer(optimizer = "nlm", iterlim = 1000)

set_optimizer <- function(x = NULL, optimizer = "nlm", ...) {

  if (length(optimizer) != 1) {
    stop("'optimizer' must be of length 1.")
  }

  add_optimizer <- new_optimizer(optimizer = optimizer, ...)

  if (is.null(x)) {
    ### initialize ino
    x <- new_ino()
    x[["optimizer"]] <- add_optimizer
  } else {
    ### ino object already exists
    if (class(x) != "ino") {
      stop("'x' must be of class 'ino'.")
    }
    if (identical(x[["optimizer"]], NA)) {
      ### add optimizer first time
      x[["optimizer"]] <- add_optimizer
    } else {
      ### add optimizer again
      cat("The ino object already contains an optimizer, what to do?\n")
      cat("1: Cancel\n")
      cat("2: Replace the old optimizer by the new optimizer\n")
      cat("3: Add the new optimizer\n")
      cat("\n")
      input <- readline(prompt = "Action: ")
      if(input == 1){
        return(x)
      } else if (input == 2) {
        x[["optimizer"]] <- optimizer
      } else if (input == 3) {
        x[["optimizer"]] <- append(x[["optimizer"]], add_optimizer)
      }
    }
  }

  ### add optimizer to ino
  x$optimizer <- append(x$optimizer, new_optimizer(optimizer = optimizer, ...))

  ### return ino
  return(x)
}

#' Construction of \code{optimizer} objects
#'
#' @description
#' This function constructs an object of class \code{optimizer}.
#'
#' @param optimizer
#' Either a character with the name of pre-defined optimizer or an object of
#' class \code{function}.
#'
#' @param ...
#' Additional arguments to be passed to \code{optimizer}.
#'
#' @return
#' An object of class \code{optimizer}.
#'
#' @keywords
#' internal

new_optimizer <- function(optimizer, ...) {
  if (is.character(optimizer)) {
    if (optimizer == "nlm") {
      fun <- stats::nlm
      args <- get_default_args(fun = fun, exclude = c("f", "p", "..."))
      args <- set_custom_args(args = args, custom_args = list(...))
      out <- list("value" = "minimum", "estimate" = "estimate")
    } else {
      stop("Not yet implemented.")
    }
  } else if (is.function(optimizer)) {
    stop("Not yet implemented.")
  } else {
    stop("'optimizer' must be either a character or a function.")
  }
  optimizer <- list("fun" = fun, "args" = args, "out" = out)
  class(optimizer) <- "optimizer"
  validate_optimizer(optimizer)
  return(optimizer)
}

#' Get default function arguments
#'
#' @description
#' This function extracts the default arguments of the function \code{fun},
#' excluding the names in \code{exclude}.
#'
#' @param fun
#' A function.
#'
#' @param exclude
#' A character vector.
#'
#' @return
#' A named list.
#'
#' @keywords
#' internal

get_default_args <- function(fun, exclude) {
  stopifnot(is.function(fun))
  stopifnot(is.character(exclude))
  args <- formals(fun)
  args <- args[!names(args) %in% exclude]
  return(args)
}

#' Set custom function arguments
#'
#' @description
#' This function sets elements in \code{args} to the custom values in
#' \code{custom_args}.
#'
#' @param args
#' A named list.
#'
#' @param custom_args
#' A named list.
#'
#' @return
#' A named list.
#'
#' @keywords
#' internal

set_custom_args <- function(args, custom_args) {
  stopifnot(is.list(args))
  stopifnot(is.list(custom_args))
  val <- custom_args[intersect(names(args), names(custom_args))]
  args <- utils::modifyList(x = args, val = val)
  return(args)
}

#' Validation of \code{optimizer} objects
#'
#' @description
#' This function validates an object of class \code{optimizer}.
#'
#' @param optimizer
#' An object of class \code{optimizer}.
#'
#' @return
#' No return value. Throws an error if \code{optimizer} is not proper.
#'
#' @keywords
#' internal

validate_optimizer <- function(optimizer) {
  if (class(optimizer) != "optimizer") {
    stop("'optimizer' must be of class 'optimizer'.")
  }
  optimizer <- unclass(optimizer)
  if (!identical(names(optimizer), c("fun", "args", "out"))) {
    stop("'optimizer' must have elements called 'fun', 'args' and 'out'.")
  }
  if (!identical(names(optimizer$out), c("value", "estimate"))) {
    stop("'optimizer' must have elements called 'value' and 'estimate'.")
  }
}
