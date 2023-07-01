
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


#' Subset argument
#'
#' @description
#' This helper function subsets an argument.
#'
#' @param argument
#' A \code{vector}, \code{matrix}, or \code{data.frame}.
#' In case of \code{how = "(dis)similar"}, it must be \code{numeric}.
#' @param by_row,how,proportion,centers,ignore,seed
#' See documentation of method \code{$reduce()} from \code{Nop} object.
#'
#' @return
#' The subsetted \code{argument}.
#'
#' @keywords internal
#'
#' @importFrom utils tail

subset_argument <- function(
    argument, by_row, how, proportion, centers, ignore, seed = NULL) {
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
    if (isFALSE(by_row)) {
      ino_stop(
        "Currently, only {.var by_row = TRUE} is implemented."
      )
    }
    if (how %in% c("similar", "dissimilar")) {
      is_index_vector(ignore)
    }
    vector_flag <- FALSE
  } else {
    ino_stop(
      glue::glue("Argument is not suited for reduction.")
    )
  }

  ### subsetting
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
    cluster <- tryCatch(
      stats::kmeans(argument_ign, centers = centers)$cluster,
      error = function(e) {
        ino_stop(
          "CLustering with {.fun stats::kmeans} failed:",
          e$message
        )
      },
      warning = function(w) {
        ino_stop(
          "CLustering with {.fun stats::kmeans} failed:",
          w$message
        )
      }
    )
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
  }

  ### check for NAs
  if (anyNA(argument)) {
    ino_warn("Reduction produced NA's.")
  }

  ### return argument
  return(argument)
}

