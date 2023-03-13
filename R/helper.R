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
#' A \code{function} that returns a \code{numeric} of length \code{npar}.
#' The \code{function} has two \code{integer} arguments:
#' 1. \code{run_id}, which selects the run,
#' 2. \code{optimizer_id}, which selects the optimizer.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' build_initial("random", 2)(1, 1)
#' build_initial(1:3, 3)(1, 2)
#' build_initial(list(1:3, 2:4), 3)(2, 1)
#' build_initial(function() stats::rnorm(4), 4)(3, 1)
#' build_initial(function(run, optimizer) c(run, optimizer), 2)(2, 3)
#' }
#'
#' @importFrom glue glue

build_initial <- function(initial, npar) {
  if (identical(initial, "random")) {
    function(run_id, optimizer_id) {
      ### same initial values across optimizers in given optimization run
      set.seed(run_id)
      rnorm(npar)
    }
  } else if (is.list(initial)) {
    if (all(sapply(initial, is.numeric) & sapply(initial, length) == npar)) {
      function(run_id, optimizer_id) {
        initial[[run_id]]
      }
    } else {
      ino_stop(
        "You specified a {.cls list} as input {.var initial}.",
        "It should only contain {.cls numeric} vectors.",
        glue::glue("Each of them should be of length {npar}.")
      )
    }
  } else if (is.numeric(initial) && is.vector(initial)) {
    if (length(initial) == npar) {
      function(run_id, optimizer_id) {
        initial
      }
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
      function(run_id, optimizer_id) {
        ### same initial values across optimizers in given optimization run
        set.seed(run_id)
        initial()
      }
    } else if (nargs == 2) {
      function(run_id, optimizer_id) {
        initial(run_id, optimizer_id)
      }
    } else {
      ino_stop(
        "The {.cls function} input {.var initial} is misspecified.",
        glue::glue("It can have 0 or 2 arguments, but not {nargs}."),
        "Please see the documentation."
      )
    }
    try_initial <- try(initial_tmp(1, 1), silent = TRUE)
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
#' Each element corresponds to one optimization run.
#' It is either \code{list()} if the run has been removed or a \code{list}
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
#' @param keep_empty
#' Set to \code{TRUE} (\code{FALSE}, the default) to keep (discard) empty
#' entries.
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
    results, run_ids, optimizer_ids, which_element, only_comparable,
    keep_empty = FALSE
  ) {

  ### input checks
  stopifnot(
    is.list(results), is_index_vector(run_ids), is_index_vector(optimizer_ids),
    is_name_vector(which_element), is_TRUE_FALSE(only_comparable),
    is_TRUE_FALSE(keep_empty)
  )

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

  ### discard empty entries
  results <- results[sapply(results, length) > 0]

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
  stopifnot(is.list(results))
  is_TRUE_FALSE(simplify)
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

  ### input checks
  is_TRUE_FALSE(verbose)

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

#' Standardize argument
#'
#' @description
#' This helper function standardizes a \code{numeric} argument.
#'
#' @param argument
#' A \code{numeric} \code{vector}, \code{matrix}, or \code{data.frame}.
#' @param by_column,center,scale,ignore
#' See documentation of method \code{$standardize()} from \code{Nop} object.
#'
#' @return
#' The standardized \code{argument}.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' standardize_argument(
#'   argument = diag(3), by_column = TRUE, center = TRUE, scale = TRUE,
#'   ignore = 1:2
#' )
#' }

standardize_argument <- function(argument, by_column, center, scale, ignore) {

  ### input checks
  attr_argument <- attributes(argument)
  vector_flag <- FALSE
  df_flag <- is.data.frame(argument)
  if (is.vector(argument) && is.numeric(argument)) {
    argument <- as.data.frame(argument)
    vector_flag <- TRUE
    by_column <- TRUE
  } else if (is.data.frame(argument) || is.matrix(argument)) {
    is_TRUE_FALSE(by_column)
    is_index_vector(ignore)
  } else {
    ino_stop(
      "Argument is not suited for standardization.",
      "Please see the function documentation."
    )
  }

  ### standardizing
  if (!by_column) {
    argument <- t(argument)
  }
  if (length(ignore) > 0) {
    if (vector_flag) {
      argument[-ignore, ] <- scale(
        argument[-ignore, ], center = center, scale = scale
      )
    } else {
      argument[, -ignore] <- scale(
        argument[, -ignore], center = center, scale = scale
      )
    }
  } else {
    argument <- scale(argument, center = center, scale = scale)
  }
  if (vector_flag) {
    argument <- argument[, 1]
  } else {
    if (!by_column) {
      argument <- t(argument)
    }
    if (df_flag) {
      argument <- as.data.frame(argument)
    }
  }

  ### check for NAs
  if (anyNA(argument)) {
    ino_warn(
      "Standardization produced NAs."
    )
  }

  ### return argument
  attributes(argument) <- attr_argument
  return(argument)
}

#' Subset argument
#'
#' @description
#' This helper function subsets an argument.
#'
#' @param argument
#' A \code{numeric} \code{vector}, \code{matrix}, or \code{data.frame}.
#' @param by_row,how,proportion,centers,ignore,seed
#' See documentation of method \code{$reduce()} from \code{Nop} object.
#'
#' @return
#' The standardized \code{argument}.
#'
#' @keywords internal
#'
#' @importFrom utils tail
#'
#' @examples
#' \dontrun{
#' subset_argument(
#'   argument = diag(1:6), by_row = TRUE, how = "similar", proportion = 0.5,
#'   centers = 2, ignore = 1:2, seed = 1
#' )
#' }

subset_argument <- function(
    argument, by_row, how, proportion, centers, ignore, seed
  ) {

  ### input checks
  is_name(how)
  if (!how %in% c("random", "first", "last", "similar", "dissimilar")) {
    ino_stop(
      "Argument {.var how} is misspecified.",
      paste(
        "It must be one of {.val random}, {.val first}, {.val last},",
        "{.val similar} or {.val dissimilar}."
      )
    )
  }
  is_proportion(proportion)
  ino_seed(seed)
  if (is.vector(argument) && length(argument) > 1) {
    argument <- as.data.frame(argument)
    vector_flag <- TRUE
    by_row <- TRUE
    ignore <- integer()
  } else if (is.data.frame(argument) || is.matrix(argument)) {
    is_TRUE_FALSE(by_row)
    if (how %in% c("similar", "dissimilar")) {
      is_index_vector(ignore)
    }
    vector_flag <- FALSE
  } else {
    ino_stop(
      glue::glue(
        "Argument is not suited for reduction."
      )
    )
  }

  ### subsetting
  if (!by_row) {
    argument <- t(argument)
  }
  n <- nrow(argument)
  m <- ceiling(n * proportion)
  if (how == "random") {
    ind <- sort(sample.int(n, m))
  } else if (how == "first") {
    ind <- seq_len(m)
  } else if (how == "last") {
    ind <- utils::tail(seq_len(n), m)
  } else {
    stopifnot(how == "similar" || how == "dissimilar")
    argument_ign <- argument
    if (length(ignore) > 0) {
      argument_ign <- argument_ign[, -ignore, drop = FALSE]
    }
    cluster <- stats::kmeans(argument_ign, centers = centers)$cluster
    ind <- integer(0)
    if (how == "similar") {
      i <- 1
      while (length(ind) < m && i <= centers) {
        ind_i <- which(cluster == i)
        ind <- c(ind, ind_i[seq_len(min(m - length(ind), length(ind_i)))])
        i <- i + 1
      }
    } else if (how == "dissimilar") {
      ind_cluster <- split(1:n, cluster)
      i <- 0
      while (length(ind) < m) {
        i_mod <- i %% centers + 1
        if (length(ind_cluster[[i_mod]]) == 0) next
        ind <- c(ind, ind_cluster[[i_mod]][1])
        ind_cluster[[i_mod]] <- ind_cluster[[i_mod]][-1]
        i <- i + 1
      }
    }
    ind <- sort(ind)
  }
  argument <- argument[ind, , drop = FALSE]
  if (vector_flag) {
    argument <- argument[, 1]
  } else {
    if (!by_row) {
      argument <- t(argument)
    }
  }

  ### check for NAs
  if (anyNA(argument)) {
    ino_warn(
      "Reduction produced NAs."
    )
  }

  ### return argument
  return(argument)
}



