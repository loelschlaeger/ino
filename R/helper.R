#' Unified function call for initial parameter specifications
#'
#' @description
#' This helper function turns various formats of initial parameter
#' specifications into a unified function call.
#'
#' @param initial,npar
#' See documentation of method \code{$optimize()} from \code{Nop} object.
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

#' Filter optimization results
#'
#' @description
#' This helper function filters optimization results.
#'
#' @param results
#' A nested \code{list} of optimization results.
#' Each element corresponds to one optimization run and is a \code{list}
#' of results for each optimizer.
#' The results for each optimizer is a \code{list}, the output of
#' \code{\link[optimizeR]{apply_optimizer}}.
#' @param run_ids
#' A \code{vector} of indices. Selects the first layer of \code{results}.
#' @param optimizer_ids
#' A \code{vector} of indices. Selects the second layer of \code{results}.
#' @param which_element
#' A \code{character} (vector). Selects the third layer of \code{results}.
#' @param only_comparable
#' See documentation of method \code{$results()} from \code{Nop} object.
#'
#' @return
#' A \code{list}.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' filter_results(
#'   results = list("run" = list(
#'     "optimizer1" = list(
#'       "value" = 1, "comparable" = FALSE
#'     ),
#'     "optimizer2" = list(
#'       "value" = 2, "comparable" = TRUE
#'      )
#'   )),
#'   run_ids = 1,
#'   optimizer_id = 2,
#'   which_element = "value",
#'   only_comparable = TRUE
#' )
#' }

filter_results <- function(
    results, run_ids, optimizer_ids, which_element, only_comparable
  ) {

  ### input checks
  stopifnot(
    is.list(results), sapply(run_ids, is_number),
    sapply(optimizer_ids, is_number), sapply(which_element, is_name)
  )
  if (!(isTRUE(only_comparable) || isFALSE(only_comparable))) {
    ino_stop(
      "Argument {.var only_comparable} must be {.val TRUE} or {.val FALSE}."
    )
  }

  ### filter runs
  results <- results[run_ids]

  ### filter optimizers
  results <- lapply(results, `[`, optimizer_ids)

  ### filter comparable
  if (only_comparable) {
    results <- lapply(results, function(x) {
      Filter(function(y) y["comparable"], x)
    })
  }

  ### filter elements
  results <- lapply(results, function(x) {
    lapply(x, function(y) y[intersect(which_element, names(y))]
  )})

  ### return
  return(results)
}

#' Simplify optimization results
#'
#' @description
#' This helper function simplifies optimization results (if possible).
#'
#' @inheritParams filter_results
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
#' simplify_results(
#'   results = list("run" = list(
#'     "optimizer1" = list(
#'       "value" = 1, "comparable" = FALSE
#'     ),
#'     "optimizer2" = list(
#'       "value" = 2, "comparable" = TRUE
#'      )
#'   )),
#'   simplify = TRUE
#' )
#' }

simplify_results <- function(results, simplify) {

  ### input checks
  stopifnot(is.list(results))
  if (!isTRUE(simplify) && !isFALSE(simplify)) {
    ino_stop(
      "Input {.var simplify} must be {.val TRUE} or {.val FALSE}."
    )
  }

  ### simplify
  if (simplify) {
    if (length(results) == 1) {
      results <- unlist(results, recursive = FALSE, use.names = TRUE)
      if (length(results) == 1) {
        results <- unlist(results, recursive = FALSE, use.names = TRUE)
      }
      if (length(results) == 1) {
        results <- unlist(results, recursive = FALSE, use.names = FALSE)
      }
    } else {
      if (all(sapply(results, length) == 1)) {
        results <- lapply(results, unlist, recursive = FALSE, use.names = TRUE)
        if (all(sapply(results, length) == 1)) {
          results <- lapply(
            results, unlist, recursive = FALSE, use.names = TRUE
          )
        }
      } else {
        if (all(sapply(results, function(x) sapply(x, length)) == 1)) {
          results <- lapply(results, function(x) {
            lapply(x, unlist, recursive = FALSE, use.names = TRUE)
          })
        }
      }
    }
  }

  ### return
  return(results)
}

#' Test \code{Nop} object
#'
#' @description
#' This helper function validates the configuration of a \code{Nop} object.
#'
#' @param x
#' A \code{Nop} object.
#' @param optimizer_ids
#' A \code{vector} of indices.
#' @param at,time_limit,verbose,digits
#' See documentation of method \code{$test()} from \code{Nop} object.
#'
#' @return
#' Invisibly \code{TRUE} if the tests are successful.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' TODO
#' }

test_nop <- function(
    x, at, optimizer_ids, time_limit, verbose, digits
  ) {

  ### test configurations
  ino_status("Test configuration", verbose = verbose)
  ino_success(
    glue::glue("Function specified: {x$f_name}"), verbose = verbose
  )
  ino_success(
    glue::glue(
      "Target argument specified: {x$f_target} (length {x$npar})"
    ), verbose = verbose
  )
  ino_success(
    glue::glue(
      "Test initial values specified: ",
      {paste(round(at, digits = digits), collapse = ' ')}
    ), verbose = verbose
  )

  ### test function call
  ino_status("Test function call", verbose = verbose)
  out <- x$evaluate(
    at = at, time_limit = time_limit, hide_warnings = TRUE
  )
  if (is.character(out)) {
    if (identical(out, "time limit reached")) {
      ino_warn(
        glue::glue(
          "Time limit of {time_limit}s was reached in the function call."
        ),
        "Consider increasing {.var time_limit}."
      )
    } else {
      ino_stop(
        "Function call threw an error.",
        glue::glue("Message: {out}")
      )
    }
  } else {
    if (!is.numeric(out)) {
      ino_stop(
        "Test function call did not return a {.cls numeric} value."
      )
    } else {
      ino_success(
        "Test function call returned a {.cls numeric}.",
        verbose = verbose
      )
    }
    if (length(out) != 1) {
      ino_stop(
        glue::glue("Test function call is of length {length(out)}."),
        "It should be a single {.cls numeric} value."
      )
    } else {
      ino_success(
        glue::glue("Return value: {round(out, digits = digits)}"),
        verbose = verbose
      )
    }
  }

  ### test optimization
  if (length(optimizer_ids) == 0) {
    ino_warn(
      "No optimizer specified, testing optimizer is skipped.",
      "Please use {.fun $set_optimizer} to specify an optimizer."
    )
  } else {
    for (i in optimizer_ids) {
      ino_status(
        glue::glue(
          "Test optimization with ",
          "`{paste(names(x$optimizer)[i], collapse = ', ')}`"
        ),
        verbose = verbose
      )
      out <- x$optimize(
        initial = at, runs = 1, which_optimizer = i, seed = NULL,
        return_results = TRUE, save_results = FALSE, ncores = 1,
        verbose = FALSE, simplify = TRUE, time_limit = time_limit,
        hide_warnings = TRUE
      )
      if (!is.null(out$error)) {
        if (identical(out$error, "time limit reached")) {
          ino_warn(
            glue::glue(
              "Time limit of {time_limit}s was reached in the optimization."
            ),
            "Consider increasing {.var time_limit}."
          )
        } else {
          ino_stop(
            "Optimization threw an error.",
            glue::glue("Message: {out$error}")
          )
        }
      } else {
        if (!is.list(out)) {
          ino_stop(
            "Test optimization did not return a {.cls list}."
          )
        } else {
          ino_success(
            "Test optimization returned a {.cls list}.",
            verbose = verbose
          )
          for (value in c("value", "parameter", "seconds")) {
            if (!value %in% names(out)) {
              ino_stop(
                glue::glue("Output does not contain the element '{value}'.")
              )
            } else {
              ino_success(glue::glue(
                "Return {value}: ",
                "{paste(round(out[[value]], digits = digits), collapse = ' ')}"
              ),
              verbose = verbose
              )
            }
          }
        }
      }
    }
  }
  invisible(TRUE)
}



