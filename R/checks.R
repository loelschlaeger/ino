#' Check for proper number vector
#'
#' @description
#' This function checks whether the input is a proper number vector, i.e., a
#' \code{numeric}.
#'
#' @param x
#' Any object.
#' @param allow_na
#' Either \code{TRUE} to allow \code{NA} in \code{x} (default) or \code{FALSE}
#' to not allow \code{NA}.
#' @param error
#' In the case that \code{x} is not a proper number vector, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords checks

is_number_vector <- function(x, allow_na = TRUE, error = TRUE) {
  is_TRUE_FALSE(error)
  is_TRUE_FALSE(allow_na)
  check <- is.vector(x) && is.numeric(x) && (allow_na || !any(is.na(x)))
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} must be {.cls numeric}.",
        .open = "<",
        .close = ">"
      )
    )
  }
  invisible(check)
}

#' Check for proper number
#'
#' @description
#' This function checks whether the input is a proper number, i.e., a single
#' \code{numeric}.
#'
#' @param x
#' Any object.
#' @param allow_na
#' Either \code{TRUE} to allow \code{NA} for \code{x} (default) or \code{FALSE}
#' to not allow \code{NA}.
#' @param error
#' In the case that \code{x} is not a proper number, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords checks

is_number <- function(x, allow_na = TRUE, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is_number_vector(x = x, error = error, allow_na = allow_na) &&
    length(x) == 1L
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
#' This function checks whether the input is a proper proportion, i.e., a single
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
#' @keywords checks

is_proportion <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is_number(x = x, error = error, allow_na = FALSE) &&
    x > 0 && x < 1
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
#' This function checks whether the input is a proper count, i.e., a single,
#' positive \code{integer}.
#'
#' @param x
#' Any object.
#' @param allow_zero
#' Either \code{TRUE} to allow a zero value (default), or \code{FALSE} if not.
#' @param error
#' In the case that \code{x} is not a proper count, either \code{TRUE} (default)
#' to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords checks

is_count <- function(x, allow_zero = TRUE, error = TRUE) {
  is_TRUE_FALSE(allow_zero)
  is_TRUE_FALSE(error)
  check <- is_number(x = x, error = error, allow_na = FALSE) &&
    x %% 1 == 0 && ifelse(allow_zero, x >= 0, x > 0)
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
#' @param allow_na
#' Either \code{TRUE} to allow \code{NA} for \code{x} (default) or \code{FALSE}
#' to not allow \code{NA}.
#' @param error
#' In the case that \code{x} is not a proper name, either \code{TRUE} (default)
#' to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords checks

is_name <- function(x, allow_na = TRUE, error = TRUE) {
  is_TRUE_FALSE(error)
  is_TRUE_FALSE(allow_na)
  check <- is.vector(x) && is.character(x) && length(x) == 1L &&
    (is.na(x) || nchar(x) > 0) && (allow_na || !is.na(x))
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
#' @param allow_na
#' Either \code{TRUE} to allow \code{NA} in \code{x} (default) or \code{FALSE}
#' to not allow \code{NA}.
#' @param error
#' In the case that \code{x} is not a proper name vector, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords checks

is_name_vector <- function(x, allow_na = TRUE, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.vector(x) &&
    all(sapply(x, is_name, allow_na = allow_na, error = FALSE))
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

#' Check for proper time
#'
#' @description
#' This function checks whether the input is a proper time, i.e., a single,
#' non-negative \code{numeric}.
#'
#' @param x
#' Any object.
#' @param allow_na
#' Either \code{TRUE} to allow \code{NA} for \code{x} (default) or \code{FALSE}
#' to not allow \code{NA}.
#' @param error
#' In the case that \code{x} is not a proper time, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords checks

is_time <- function(x, allow_na = TRUE, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is_number(x, allow_na = allow_na, error = error) && x >= 0
  if (!check && error) {
    x_name <- deparse(substitute(x))
    ino_stop(
      glue::glue(
        "Argument {.var <x_name>} is not a non-negative {.cls numeric}.",
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
#' This function checks whether the input is a proper time limit, i.e., a
#' single, positive \code{numeric} or \code{NULL}.
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
#' @keywords checks

is_time_limit <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.null(x) || (is_time(x = x, error = error) && x > 0)
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
#' This function checks whether the input is a proper boolean, i.e., either
#' \code{TRUE} or \code{FALSE}.
#'
#' @param x
#' Any object.
#' @param allow_na
#' Either \code{TRUE} to allow \code{NA} for \code{x} (default) or \code{FALSE}
#' to not allow \code{NA}.
#' @param error
#' In the case that \code{x} is not a proper boolean, either \code{TRUE}
#' (default) to throw an error or \code{FALSE} to return invisibly \code{FALSE}.
#'
#' @return
#' If \code{error = TRUE}, either invisibly \code{TRUE} or an error is thrown.
#' If \code{error = FALSE}, invisibly \code{TRUE} or \code{FALSE}.
#'
#' @keywords checks

is_TRUE_FALSE <- function(x, allow_na = TRUE, error = TRUE) {
  stopifnot(isTRUE(allow_na) || isFALSE(allow_na))
  stopifnot(isTRUE(error) || isFALSE(error))
  check <- is.logical(x) && length(x) == 1L && (allow_na || !is.na(x))
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
#' This function checks whether the input is a proper index vector, i.e., a
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
#' @keywords checks

is_index_vector <- function(x, error = TRUE) {
  is_TRUE_FALSE(error)
  check <- is.vector(x) &&
    all(sapply(x, is_count, allow_zero = FALSE, error = FALSE))
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
