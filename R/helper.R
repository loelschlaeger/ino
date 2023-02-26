#' Unified function call for initial parameter specifications
#'
#' @description
#' This helper function turns various formats of initial parameter specification
#' into a unified function call.
#'
#' @param initial
#' See documentation of method \code{$optimize()} from \code{Nop} object.
#' @param npar
#' See documentation of method \code{$initialize()} from \code{Nop} object.
#'
#' @return
#' A \code{function} without any arguments that returns a \code{numeric}
#' of length \code{npar}.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' build_initial("random", 2)(1)
#' build_initial(1:3, 3)(1)
#' build_initial(list(1:3, 2:4), 3)(2)
#' build_initial(function(run) rep(run, 4), 4)(3)
#' }
#'
#' @importFrom glue glue

build_initial <- function(initial, npar) {
  if (identical(initial, "random")) {
    function(run) rnorm(npar)
  } else if (is.list(initial)) {
    if (all(sapply(initial, is.numeric) & sapply(initial, length) == npar)) {
      runs <- length(initial)
      function(run) initial[[run]]
    } else {
      ino_stop(
        "You specified a {.cls list} as input {.var initial}.",
        "It should only contain {.cls numeric} vectors.",
        glue::glue("Each of them should be of length {npar}.")
      )
    }
  } else if (is.numeric(initial) && is.vector(initial)) {
    if (length(initial) == npar) {
      function(run) initial
    } else {
      ino_stop(
        "The {.cls numeric} input {.var initial} is misspecified.",
        glue::glue("It should be of length {npar}."),
        glue::glue("Instead, it is of length {length(initial)}.")
      )
    }
  } else if (is.function(initial)) {
    nargs <- length(formals(initial))
    initial_tmp <- if (nargs == 0) {
      function(run) initial()
    } else if (nargs == 1) {
      initial
    } else {
      ino_stop(
        "The {.cls function} input {.var initial} is misspecified.",
        glue::glue("It can have 0 or 1 arguments, but not {nargs}.")
      )
    }
    try_initial <- try(initial_tmp(1), silent = TRUE)
    if (!(is.numeric(try_initial) && length(try_initial) == npar)) {
      ino_stop(
        "The {.cls function} input {.var initial} is misspecified.",
        glue::glue("It should return initial values of length {npar}.")
      )
    }
    initial_tmp
  } else {
    ino_stop(
      "The input specification {.var initial} is unexpected.",
      "Please see the documentation for possible inputs."
    )
  }
}

#' Checks if the optimizers with given ids are active (i.e., not removed)
#'
#' @description
#' This helper function ... TODO
#'
#' @param optimizer
#' TODO
#' @param ids
#' TODO
#'
#' @return
#' TODO
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' TODO
#' }

check_optimizer_active <- function(optimizer, ids) {
  stopifnot(sapply(ids, is_number), ids <= length(optimizer))
  sapply(names(optimizer)[ids], nchar) > 0
}

#' TODO
#'
#' @description
#' This helper function ... TODO
#'
#' @param results
#' TODO
#' @param simplify
#' TODO
#'
#' @return
#' TODO
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' TODO
#' }

simplify_results <- function(results, simplify) {
  if (!isTRUE(simplify) && !isFALSE(simplify)) {
    ino_stop(
      "Input {.var simplify} must be {.val TRUE} or {.val FALSE}."
    )
  }
  if (simplify) {
    if (length(results) == 1) {
      results <- unlist(results, recursive = FALSE)
    }
    if (length(results) == 1) {
      results <- unlist(results, recursive = FALSE)
    }
    # TODO: also simplify if each optimization result is only single-elemented
  }
  return(results)
}

#' TODO
#'
#' @description
#' This helper function ... TODO
#'
#' @param results
#' TODO
#' @param only_comparable
#' TODO
#'
#' @return
#' TODO
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' TODO
#' }

filter_comparable <- function(results, only_comparable) {
  if (!(isTRUE(only_comparable) || isFALSE(only_comparable))) {
    ino_stop(
      "Argument {.var only_comparable} must be {.val TRUE} or {.val FALSE}."
    )
  }
  lapply(results, function(x) Filter(function(y) y["comparable"], x))
}
