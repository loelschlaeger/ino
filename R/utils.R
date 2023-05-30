#' Check for proper number
#'
#' @description
#' This function checks whether the input is proper number, i.e., a single
#' \code{numeric}.
#'
#' @param x
#' Any object.
#' @param error
#' In the case that \code{x} is not a proper number, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_number <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.vector(x) && is.numeric(x) && length(x) == 1
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be a single {.cls numeric}.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

#' Check for proper proportion
#'
#' @description
#' This function checks whether the input is proper proportion, i.e., a single
#' \code{numeric} between 0 and 1.
#'
#' @param x
#' Any object.
#' @param error
#' In the case that \code{x} is not a proper proportion, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_proportion <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is_number(x = x, error = error) && x > 0 && x < 1
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be a {.cls numeric} between 0 and 1.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

#' Check for proper count
#'
#' @description
#' This function checks whether the input is proper count, i.e., a single,
#' positive \code{integer}.
#'
#' @param x
#' Any object.
#' @param allow_zero
#' Either \code{TRUE} to allow a zero value, or \code{FALSE} (default) if not.
#' @param error
#' In the case that \code{x} is not a proper count, either \code{TRUE} (default)
#' to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_count <- function(x, allow_zero = FALSE, error = TRUE) {
  is_TRUE_FALSE(allow_zero)
  is_TRUE_FALSE(error)
  check <- is_number(x = x, error = error) && x %% 1 == 0 &&
    ifelse(allow_zero, x >= 0, x > 0)
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be a single, positive {.cls integer}.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

#' Check for proper name
#'
#' @description
#' This function checks whether the input is a proper name, i.e., a single
#' (non-trivial) \code{character}.
#'
#' @param x
#' Any object.
#' @param error
#' In the case that \code{x} is not a proper name, either \code{TRUE} (default)
#' to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_name <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.vector(x) && is.character(x) && length(x) == 1 && nchar(x) > 0
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be a single {.cls character}.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

#' Check for proper name vector
#'
#' @description
#' This function checks whether the input is a proper name vector, i.e., a
#' \code{vector} of (non-trivial) \code{character} values.
#'
#' @param x
#' Any object.
#' @param error
#' In the case that \code{x} is not a proper name vector, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_name_vector <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.vector(x) && all(sapply(x, is_name, error = FALSE))
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be a {.cls vector} of {.cls character}.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

#' Check for proper time limit
#'
#' @description
#' This function checks whether the input is proper time limit, i.e., a single,
#' positive \code{numeric}.
#'
#' @param x
#' Any object.
#' @param error
#' In the case that \code{x} is not a proper time limit, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_time_limit <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.null(x) || (is_number(x, error = error) && x > 0)
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} is not a positive {.cls numeric}.",
        .open = "<",
        .close = ">"
      ),
      "It should be a number of seconds.",
      "Alternatively, it can be {.val NULL} for no time limit."
    )
  }
  invisible(check)
}

#' Check for proper boolean
#'
#' @description
#' This function checks whether the input is proper boolean, i.e., either
#' \code{TRUE} or \code{FALSE}.
#'
#' @param x
#' Any object.
#' @param error
#' In the case that \code{x} is not a proper boolean, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_TRUE_FALSE <- function(x, error = TRUE) {
  stopifnot(isTRUE(error) || isFALSE(error))
  check <- isTRUE(x) || isFALSE(x)
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be {.val TRUE} or {.val FALSE}.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

#' Check for proper index vector
#'
#' @description
#' This function checks whether the input is proper index vector, i.e., a
#' \code{vector} of positive \code{integer} values.
#'
#' @param x
#' Any object.
#' @param error
#' In the case that \code{x} is not a proper index vector, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords utils

is_index_vector <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.vector(x) && all(sapply(x, is_count, error = FALSE))
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be an index {.cls vector}.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

