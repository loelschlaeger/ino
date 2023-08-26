#' Helper function for standardization
#'
#' @details
#' This function is part of the \code{$argument(action = "standardize")} method
#' of a \code{\link{Nop}} object and standardizes a given argument.
#'
#' @param argument
#' A \code{numeric} \code{vector}, \code{matrix}, or \code{data.frame}.
#' @param byrow
#' Only relevant if \code{argument} is a \code{matrix} or a \code{data.frame}.
#' In that case, either \code{TRUE} to standardize row-wise or
#' \code{FALSE} to standardize column-wise (default).
#' @param center
#' Set to \code{TRUE} (default) for centering, resulting in zero mean.
#' @param scale
#' Set to \code{TRUE} (default) for scaling, resulting in unit variance.
#' @param ignore
#' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame}.
#' In that case, A \code{integer} (vector) of column indices (or row indices if
#' \code{byrow = TRUE}) to not standardize.
#' @param jointly
#' Only relevant if \code{argument} is a \code{matrix} or \code{data.frame}.
#' In that case, a \code{list} of \code{integer} vectors with column indices
#' (or row indices if \code{byrow = TRUE}) to standardize jointly.
#'
#' @return
#' The standardized input \code{argument}.
#' The \code{numeric} centering and scalings used (if any) are added as
#' attributes \code{"standardized:center"} and \code{"standardized:scale"}.
#'
#' @keywords internal
#'
#' @importFrom stats sd

helper_standardize <- function(
    argument, byrow = FALSE, center = TRUE, scale = TRUE, ignore = integer(),
    jointly = list()
  ) {
  vector_flag <- is.atomic(argument) && is.null(dim(argument))
  if (vector_flag) {
    argument <- as.data.frame(argument)
    byrow <- FALSE
    ignore <- integer()
    jointly <- list()
  } else if (is.data.frame(argument) || is.matrix(argument)) {
    checkmate::assert_logical(byrow, len = 1)
    indices <- if (byrow) {
      seq_len(nrow(argument))
    } else {
      seq_len(ncol(argument))
    }
    checkmate::assert_integerish(ignore, lower = 1)
    if (!all(ignore %in% indices)) {
      cli::cli_abort(
        "Argument {.var ignore} is out of bound.",
        call = NULL
      )
    }
    checkmate::assert_list(jointly)
    if (length(jointly) > 0) {
      for (joint in jointly) {
        if (!checkmate::test_integerish(joint)) {
          cli::cli_abort(
            "Argument {.var jointly} must contain index vectors.",
            call = NULL
          )
        }
        if (!all(joint %in% indices)) {
          cli::cli_abort(
            "Argument {.var jointly} is out of bound.",
            call = NULL
          )
        }
      }
      if (length(unlist(jointly)) != length(unique(unlist(jointly)))) {
        cli::cli_abort(
          "Elements in {.var jointly} must be exclusive.",
          call = NULL
        )
      }
      if (length(intersect(unlist(jointly), ignore)) > 0) {
        cli::cli_abort(
          "Cannot have same elements in {.var jointly} and {.var ignored}.",
          call = NULL
        )
      }
    }
  } else {
    cli::cli_abort(
      "Argument cannot be standardized.",
      call = NULL
    )
  }
  margin <- ifelse(byrow, 1, 2)
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
      argument, margin, stats::sd, na.rm = TRUE, simplify = TRUE
    )
    scale_values[ignore] <- 1
    for (join in jointly) {
      scale_values[join] <- if (byrow) {
        stats::sd(as.matrix(argument[join, ]), na.rm = TRUE)
      } else {
        stats::sd(as.matrix(argument[, join]), na.rm = TRUE)
      }
    }
    argument <- sweep(argument, margin, scale_values, "/")
  }
  if (anyNA(argument)) {
    cli::cli_warn("Argument has NAs after standardization.")
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
#' This function is part of the \code{$argument(action = "subset")} method of a
#' \code{\link{Nop}} object and subsets a given argument.
#'
#' @param argument
#' A \code{vector}, \code{matrix}, or \code{data.frame}.
#' @param byrow
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
#' if \code{byrow = FALSE}) to ignore for clustering.
#'
#' @return
#' The subsetted input \code{argument}.
#'
#' @keywords internal

helper_subset <- function(
    argument, byrow = TRUE, how = "random", proportion = 0.5, centers = 2,
    ignore = integer()
  ) {
  how <- match_arg(
    how, c("random", "first", "last", "similar", "dissimilar")
  )
  checkmate::assert_number(proportion, lower = 0, upper = 1)
  df_flag <- ifelse(is.data.frame(argument), TRUE, FALSE)
  if (is.atomic(argument) && is.null(dim(argument)) && length(argument) > 1) {
    argument <- as.data.frame(argument)
    vector_flag <- TRUE
    byrow <- TRUE
    ignore <- integer()
  } else if (is.data.frame(argument) || is.matrix(argument)) {
    checkmate::assert_logical(byrow, len = 1)
    if (!byrow) {
      argument <- t(argument)
    }
    if (how %in% c("similar", "dissimilar")) {
      checkmate::assert_integerish(ignore)
    }
    vector_flag <- FALSE
  } else {
     cli::cli_abort(
      "Subsetting can only be applied to objects of class {.cls vector} (of
      length greater than one), {.cls matrix}, or {.cls data.frame}.",
      call = NULL
    )
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
  if (!byrow) {
    argument <- t(argument)
  }
  if (df_flag) {
    argument <- as.data.frame(argument)
  }
  return(argument)
}

#' Helper function for argument matching
#'
#' @description
#' This function matches function arguments and is a modified version of
#' \code{\link[base]{match.arg}}.
#'
#' @param arg
#' A \code{character} (vector), the function argument.
#' @param choices
#' A \code{character} (vector) of allowed values for \code{arg}.
#' @param several.ok
#' Either \code{TRUE} if \code{arg} is allowed to have more than one element,
#' or \code{FALSE} else.
#' @param none.ok
#' Either \code{TRUE} if \code{arg} is allowed to have zero elements,
#' or \code{FALSE} else.
#'
#' @return
#' The un-abbreviated version of the exact or unique partial match if there is
#' one. Otherwise, an error is signaled if \code{several.ok} is \code{FALSE}
#' or \code{none.ok} is \code{FALSE}.
#' When \code{several.ok} is \code{TRUE} and (at least) one element of
#' \code{arg} has a match, all un-abbreviated versions of matches are returned.
#' When \code{none.ok} is \code{TRUE} and \code{arg} has zero elements,
#' \code{character(0)} is returned.
#'
#' @keywords internal

match_arg <- function (arg, choices, several.ok = FALSE, none.ok = FALSE) {
  checkmate::assert_character(arg)
  checkmate::assert_character(choices)
  checkmate::assert_logical(several.ok, len = 1)
  checkmate::assert_logical(none.ok, len = 1)
  arg_name <- deparse(substitute(arg))
  if (!several.ok && length(arg) > 1L) {
    cli::cli_abort(
      "{.var {arg_name}} must be of length 1.",
      call = NULL
    )
  }
  if (length(arg) == 0L) {
    if (none.ok) {
      return(character(0))
    } else {
      cli::cli_abort(
        "{.var {arg_name}} must be of length greater or equal 1.",
        call = NULL
      )
    }
  }
  i <- pmatch(arg, choices, nomatch = 0, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    cli::cli_abort(
      "{.var {arg_name}} {ifelse(none.ok, 'can', 'must')} be one
      {ifelse(several.ok, 'or more', '')} of {.val {choices}}.",
      call = NULL
    )
  }
  i <- i[i > 0L]
  choices[i]
}

#' Helper function for removing an index
#'
#' @description
#' This function removes a value from an index vector and optionally shifts the
#' indices up.
#'
#' @param x
#' An \code{integer} (vector).
#' @param index
#' An \code{integer}.
#' @param replace
#' Either \code{TRUE} the shift the indices up, or \code{FALSE} (default) else.
#'
#' @return
#' An \code{integer} (vector).
#'
#' @keywords internal

remove_index <- function(x, index, replace = FALSE) {
  checkmate::assert_integerish(x)
  checkmate::assert_int(index)
  checkmate::assert_logical(replace, len = 1)
  x <- x[!(x %in% index)]
  if (replace) x[x >= index] <- x[x >= index] - 1
  return(x)
}

#' Helper function for user confirmation
#'
#' @description
#' This function asks in an interactive question a binary question.
#'
#' @param question
#' A \code{character}, the binary question to ask. It should end with a
#' question mark.
#' @param default
#' Either \code{TRUE} or \code{FALSE} (default), the default decision.
#'
#' @return
#' Either \code{TRUE} or \code{FALSE}.
#'
#' @keywords internal

user_confirm <- function (question = "Question?", default = FALSE) {
  checkmate::assert_flag(default)
  if (!interactive()) return(default)
  checkmate::assert_string(question)
  repeat {
    selection <- ifelse(default, "[Y/n]", "[y/N]")
    cli::cli_inform("{question} {selection}:")
    response <- tryCatch(
      tolower(trimws(readLines(con = getOption("ino_connection"), n = 1))),
      interrupt = identity
    )
    if (inherits(response, "interrupt")) stop()
    if (!nzchar(response)) return(default)
    if (response %in% c("y", "yes")) return(TRUE)
    if (response %in% c("n", "no")) return(FALSE)
    cli::cli_inform("Please enter 'y' or 'n', or type Ctrl + C to cancel.")
  }
}


