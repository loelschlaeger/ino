
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

filter_results <- function(
    results, run_ids, optimizer_ids, which_element, only_comparable,
    keep_empty = FALSE) {
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
    lapply(x, function(y) y[intersect(which_element, names(y))])
  })

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
            results, unlist,
            recursive = FALSE, use.names = TRUE
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


