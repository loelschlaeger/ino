#' Helper function for initial values
#'
#' @description
#' This function is part of the \code{$optimize()} method of a
#' \code{\link{Nop}} object and creates a \code{list} of initial values.
#'
#' @param initial
#' Specify the initial point where the optimizer should start. Either:
#' - the \code{character} \code{"random"} (the default) for random initial
#'   values drawn from a standard normal distribution,
#' - a \code{numeric} vector of length \code{npar}, the deterministic
#'   starting point for the optimization,
#' - a \code{list} of such vectors (in this case, \code{runs} is set to the
#'   length of the \code{list}),
#' - or a \code{function} without any arguments that returns a
#'   \code{numeric} vector of length \code{npar}.
#' In all these cases, the same initial values are used across optimizers.
#'
#' For more flexibility, a \code{funtion} input for \code{initial} can have
#' two arguments, where the first argument specifies the optimization run,
#' and the second argument specifies the optimizer, i.e.,
#' \code{initial <- function(run, optimizer) ...} for the
#' \code{run}-th optimization run and the \code{optimizer}-th optimizer
#' listed in the \code{$print()} output.
#' @param npar
#' A \code{integer}, the length of the initial parameter vector.
#' @param check_initial
#' Either \code{TRUE} (default) to check the initial values and fail on
#' misspecification, or \code{FALSE} to accept misspecifications.
#' @param runs
#' An \code{integer}, the number of optimization runs.
#' If \code{initial} is a \code{list}, \code{runs} is set to
#' \code{length(initial)}.
#' By default, \code{runs = 1}.
#' @param optimizer_ids
#' An \code{integer} \code{vector} with optimizer ids.
#'
#' @return
#' A \code{list} with \code{runs} elements, each element is a \code{list}, where
#' the \code{i}-th element is a \code{numeric} of length \code{npar}, which is
#' the initial parameter vector for optimization with the optimizer with the id
#' \code{optimizer_ids[i]}.
#'
#' @keywords internal

initial_values_helper <- function(
    initial, npar, check_initial, runs, optimizer_ids
  ) {
  is_count(npar)
  is_TRUE_FALSE(check_initial)
  is_count(runs)
  is_index_vector(optimizer_ids)
  initial_values <- list()
  if (identical(initial, "random")) {
    for (run in 1:runs) {
      initial_values[[run]] <- list()
      random_initial_value <- stats::rnorm(n = npar)
      for (optimizer_id in optimizer_ids) {
        initial_values[[run]][[optimizer_id]] <- random_initial_value
      }
    }
  } else if (is.list(initial)) {
    if (!check_initial ||
      all(sapply(initial, is_number_vector, error = FALSE) &
          sapply(initial, length) == npar)
    ) {
      for (run in 1:runs) {
        initial_values[[run]] <- list()
        for (optimizer_id in optimizer_ids) {
          initial_values[[run]][[optimizer_id]] <- initial[[run]]
        }
      }
    } else {
      ino_stop(
        "You specified a {.cls list} as input {.var initial}.",
        "It should only contain {.cls numeric} vectors.",
        glue::glue("Each of them should be of length {npar}.")
      )
    }
  } else if (is_number_vector(initial, error = FALSE)) {
    if (!check_initial || length(initial) == npar) {
      for (run in 1:runs) {
        initial_values[[run]] <- list()
        for (optimizer_id in optimizer_ids) {
          initial_values[[run]][[optimizer_id]] <- initial
        }
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
    if (check_initial && !nargs %in% c(0, 2)) {
      ino_stop(
        "The {.cls function} input {.var initial} is misspecified.",
        glue::glue("It can have 0 or 2 arguments, but not {nargs}.")
      )
    }
    if (nargs == 0) {
      for (run in 1:runs) {
        initial_values[[run]] <- list()
        function_initial_value <- try(initial(), silent = TRUE)
        if (!check_initial ||
            (is_number_vector(function_initial_value, error = FALSE) &
             length(function_initial_value) == npar)) {
          for (optimizer_id in optimizer_ids) {
            initial_values[[run]][[optimizer_id]] <- function_initial_value
          }
        } else {
          ino_stop(
            "The {.cls function} input {.var initial} is misspecified.",
            glue::glue("It should return initial values of length {npar}.")
          )
        }
      }
    }
    if (nargs == 2) {
      for (run in 1:runs) {
        initial_values[[run]] <- list()
        for (optimizer_id in optimizer_ids) {
          function_initial_value <- try(
            initial(run, optimizer_id), silent = TRUE
          )
          if (!check_initial ||
              (is_number_vector(function_initial_value, error = FALSE) &
               length(function_initial_value) == npar)) {
            initial_values[[run]][[optimizer_id]] <- function_initial_value
          } else {
            ino_stop(
              "The {.cls function} input {.var initial} is misspecified.",
              glue::glue("It should return initial values of length {npar}.")
            )
          }
        }
      }
    }
  } else {
    ino_stop(
      "The input specification {.var initial} is unexpected.",
      "Please see the documentation for possible inputs."
    )
  }
  return(initial_values)
}

#' Helper function for standardization
#'
#' @details
#' This function is part of the \code{$standardize()} method of a
#' \code{\link{Nop}} object and standardizes a given argument.
#'
#' @param argument
#' A \code{numeric} \code{vector}, \code{matrix}, or \code{data.frame}.
#' @param by_column
#' Only relevant if \code{argument} is a \code{matrix} or a \code{data.frame}.
#' In that case, either \code{TRUE} to standardize column-wise (default) or
#' \code{FALSE} to standardize row-wise.
#' @param center
#' Set to \code{TRUE} (default) for centering, resulting in zero mean.
#' @param scale
#' Set to \code{TRUE} (default) for scaling, resulting in unit variance.
#' @param ignore
#' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame}.
#' In that case, A \code{integer} (vector) of column indices (or row indices if
#' \code{by_column = FALSE}) to not standardize.
#' @param jointly
#' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame}.
#' In that case, a \code{list} of \code{integer} vectors with column indices
#' (or row indices if \code{by_column = FALSE}) to standardize jointly.
#'
#' @return
#' The standardized input \code{argument}.
#' The \code{numeric} centering and scalings used (if any) are added as
#' attributes \code{"standardized:center"} and \code{"standardized:scale"}.
#'
#' @keywords internal

standardize_helper <- function(
    argument, by_column = TRUE, center = TRUE, scale = TRUE, ignore = integer(),
    jointly = list()
  ) {
  vector_flag <- is.atomic(argument) && is.null(dim(argument))
  if (vector_flag) {
    argument <- as.data.frame(argument)
    by_column <- TRUE
    ignore <- integer()
    jointly <- list()
  } else if (is.data.frame(argument) || is.matrix(argument)) {
    is_TRUE_FALSE(by_column)
    indices <- if (by_column) {
      seq_len(ncol(argument))
    } else {
      seq_len(nrow(argument))
    }
    is_index_vector(ignore)
    if (!all(ignore %in% indices)) {
      ino_stop("Argument {.var ignore} is out of bound.")
    }
    if (!is.list(jointly)) {
      ino_stop("Argument {.var jointly} must be a {.cls list}.")
    }
    if (length(jointly) > 0) {
      for (joint in jointly) {
        if (!is_index_vector(joint, error = FALSE)) {
          ino_stop("Argument {.var jointly} must contain index vectors.")
        }
        if (!all(joint %in% indices)) {
          ino_stop("Argument {.var jointly} is out of bound.")
        }
      }
      if (length(unlist(jointly)) != length(unique(unlist(jointly)))) {
        ino_stop("Elements in {.var jointly} must be exclusive.")
      }
      if (length(intersect(unlist(jointly), ignore)) > 0) {
        ino_stop(
          "Cannot have same elements in {.var jointly} and {.var ignored}."
        )
      }
    }
  } else {
    ino_stop("Argument cannot be standardized.")
  }
  margin <- ifelse(by_column, 2, 1)
  center_values <- rep(0, dim(argument)[margin])
  scale_values <- rep(1, dim(argument)[margin])
  if (center) {
    center_values <- apply(
      argument, margin, mean, na.rm = TRUE, simplify = TRUE
    )
    center_values[ignore] <- 0
    for (join in jointly) {
      center_values[join] <- mean(center_values[join], na.rm = TRUE)
    }
    argument <- sweep(argument, margin, center_values, "-")
  }
  if (scale) {
    scale_values <- apply(
      argument, margin, sd, na.rm = TRUE, simplify = TRUE
    )
    scale_values[ignore] <- 1
    for (join in jointly) {
      scale_values[join] <- sqrt(mean(scale_values[join]^2, na.rm = TRUE))
    }
    argument <- sweep(argument, margin, scale_values, "/")
  }
  if (anyNA(argument)) {
    ino_warn("Argument has NAs after standardization.")
  }
  if (vector_flag) {
    argument <- argument[, 1, drop = TRUE]
  }
  if (center) {
    attr(argument, "standardized:center") <- as.numeric(center_values)
  }
  if (scale) {
    attr(argument, "standardized:scale") <- as.numeric(scale_values)
  }
  return(argument)
}

#' Helper function for subsetting
#'
#' @details
#' This function is part of the \code{$subset()} method of a
#' \code{\link{Nop}} object and subsets a given argument.
#'
#' @param argument
#' A \code{vector}, \code{matrix}, or \code{data.frame}.
#' @param by_row
#' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame}.
#' In that case, either \code{TRUE} to subset row-wise (default) or
#' \code{FALSE} to subset column-wise.
#' @param how
#' A \code{character}, specifying how to subset. Can be one of:
#' - \code{"random"} (default), subset at random
#' - \code{"first"}, subset to the first elements
#' - \code{"last"}, subset to the last elements
#' - \code{"similar"}, subset to similar elements
#' - \code{"dissimilar"}, subset to dissimilar elements
#' The options \code{"similar"} and \code{"dissimilar"} apply k-means
#' clustering via \code{\link[stats]{kmeans}} and require that
#' the argument \code{argument_name} is \code{numeric}.
#' @param proportion
#' A \code{numeric} between \code{0} and \code{1}, specifying the
#' subset proportion.
#' By default, \code{proportion = 0.5}.
#' @param centers
#' Only relevant, if \code{how = "(dis)similar"}.
#' In that case, passed to \code{\link[stats]{kmeans}}.
#' By default, \code{centers = 2}.
#' @param ignore
#' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame} and
#' \code{how = "(dis)similar"}.
#' In that case, a \code{integer} (vector) of row indices (or column indices
#' if \code{by_row = FALSE}) to ignore for clustering.
#'
#' @return
#' The subsetted input \code{argument}.
#'
#' @keywords internal

subset_helper <- function(
    argument, by_row = TRUE, how = "random", proportion = 0.5, centers = 2,
    ignore = integer()
  ) {
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
  df_flag <- ifelse(is.data.frame(argument), TRUE, FALSE)
  if (is.vector(argument) && length(argument) > 1) {
    argument <- as.data.frame(argument)
    vector_flag <- TRUE
    by_row <- TRUE
    ignore <- integer()
  } else if (is.data.frame(argument) || is.matrix(argument)) {
    is_TRUE_FALSE(by_row)
    if (!by_row) {
      argument <- t(argument)
    }
    if (how %in% c("similar", "dissimilar")) {
      is_index_vector(ignore)
    }
    vector_flag <- FALSE
  } else {
    ino_stop("Argument cannot be subsetted.")
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
    argument_ign <- argument
    if (length(ignore) > 0) {
      argument_ign <- argument_ign[, -ignore, drop = FALSE]
    }
    cluster <- tryCatch(
      stats::kmeans(argument_ign, centers = centers)$cluster,
      error = function(e) {
        ino_stop(
          "Clustering with {.fun stats::kmeans} failed:",
          e$message
        )
      },
      warning = function(w) {
        ino_stop(
          "Clustering with {.fun stats::kmeans} failed:",
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
        i <- i + 1
        if (length(ind_cluster[[i_mod]]) == 0) next
        ind <- c(ind, ind_cluster[[i_mod]][1])
        ind_cluster[[i_mod]] <- ind_cluster[[i_mod]][-1]
      }
    }
    ind <- sort(ind)
  }
  argument <- argument[ind, , drop = FALSE]
  if (vector_flag) {
    argument <- argument[, 1]
  }
  if (!by_row) {
    argument <- t(argument)
  }
  if (df_flag) {
    argument <- as.data.frame(argument)
  }
  if (anyNA(argument)) {
    ino_warn("Argument has NAs after subsetting.")
  }
  return(argument)
}

#' Helper function for flattening a list
#'
#' @description
#' This function flattens a nested \code{list} (if possible), see the details.
#'
#' @details
#' The input \code{list} \code{x} is transformed in the following ways:
#' -
#'
#' @param x
#' A \code{list}.
#'
#' @return
#' A \code{list}.
#'
#' @keywords internal

helper_flatten_list <- function(x) {
  stopifnot(is.list(x))
  if (length(x) == 1) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
    if (length(x) == 1) {
      x <- unlist(x, recursive = FALSE, use.names = TRUE)
    }
    if (length(x) == 1) {
      x <- unlist(x, recursive = FALSE, use.names = FALSE)
    }
  } else {
    if (all(sapply(x, length) == 1)) {
      x <- lapply(x, unlist, recursive = FALSE, use.names = TRUE)
      if (all(sapply(x, length) == 1)) {
        x <- lapply(x, unlist, recursive = FALSE, use.names = TRUE)
      }
    } else {
      if (all(sapply(x, function(x) sapply(x, length)) == 1)) {
        x <- lapply(x, function(x) {
          lapply(x, unlist, recursive = FALSE, use.names = TRUE)
        })
      }
    }
  }
  return(x)
}



# TODO


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



