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

#' Transform optimization results
#'
#' @description
#' This helper function transforms optimization results:
#' - filter the nested \code{list},
#' - simplify by flattening the nested \code{list} (if possible).
#'
#' @param results
#' A nested \code{list} of optimization results.
#' Each element corresponds to one optimization run and is a \code{list}
#' of results for each optimizer.
#' The results for each optimizer is a \code{list}, the output of
#' \code{\link[optimizeR]{apply_optimizer}}.
#' @param which_element
#' See documentation of method \code{$results()} from \code{Nop} object.
#' @param only_comparable
#' See documentation of method \code{$results()} from \code{Nop} object.
#' @param simplify
#' See documentation of method \code{$results()} from \code{Nop} object.
#'
#' @return
#' A \code{list}.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' transform_results(
#'   results = list("run" = list(
#'     "optimizer1" = list(
#'       "value" = 1, "comparable" = FALSE
#'     ),
#'     "optimizer2" = list(
#'       "value" = 2, "comparable" = TRUE
#'      )
#'   )),
#'   which_element = "value",
#'   only_comparable = TRUE,
#'   simplify = TRUE
#' )
#' }

transform_results <- function(
    results, which_element, only_comparable, simplify
  ) {

  ### input checks
  if (!all(sapply(which_element, is_name))) {
    ino_stop(
      "Input {.var which_element} is misspecified.",
      "It can be {.val all}, {.val basic}, or a {.cls character} (vector)."
    )
  }
  if (identical(which_element, "basic")) {
    which_element <- c("value", "parameter")
  }
  if (identical(which_element, "default")) {
    which_element <- c(
      "run", "optimizer", "value", "parameter", "seconds", "label", "error"
    )
  }
  if (!(isTRUE(only_comparable) || isFALSE(only_comparable))) {
    ino_stop(
      "Argument {.var only_comparable} must be {.val TRUE} or {.val FALSE}."
    )
  }
  if (!isTRUE(simplify) && !isFALSE(simplify)) {
    ino_stop(
      "Input {.var simplify} must be {.val TRUE} or {.val FALSE}."
    )
  }
  stopifnot(
    ### expect that 'results' is a 'list'
    is.list(results),
    ### expect that each element of 'results' is a 'list' (runs)
    sapply(results, is.list),
    ### expect that each element of each element of 'results' is a 'list'
    ### (optimizer)
    sapply(results, function(x) sapply(x, is.list))
  )

  ### filter
  results <- lapply(results, function(x) {
    Filter(function(y) y["comparable"], x)
  })
  if (!identical(which_element, "all")) {
    results <- lapply(results, function(x) {
      lapply(x, function(y) y[intersect(which_element, names(y))]
    )})
  }

  ### simplify
  if (simplify) {
    for (layer in 1:3) {
      if (length(results) == 1) {
        results <- unlist(results, recursive = FALSE, use.names = FALSE)
      } else {
        break
      }
    }
  }

  ### return
  return(results)
}
