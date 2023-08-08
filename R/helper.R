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
    is_TRUE_FALSE(byrow)
    indices <- if (byrow) {
      seq_len(nrow(argument))
    } else {
      seq_len(ncol(argument))
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
      argument, margin, sd, na.rm = TRUE, simplify = TRUE
    )
    scale_values[ignore] <- 1
    for (join in jointly) {
      scale_values[join] <- if (byrow) {
        sd(as.matrix(argument[join, ]), na.rm = TRUE)
      } else {
        sd(as.matrix(argument[, join]), na.rm = TRUE)
      }
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
  if (is.atomic(argument) && is.null(dim(argument)) && length(argument) > 1) {
    argument <- as.data.frame(argument)
    vector_flag <- TRUE
    byrow <- TRUE
    ignore <- integer()
  } else if (is.data.frame(argument) || is.matrix(argument)) {
    is_TRUE_FALSE(byrow)
    if (!byrow) {
      argument <- t(argument)
    }
    if (how %in% c("similar", "dissimilar")) {
      is_index_vector(ignore)
    }
    vector_flag <- FALSE
  } else {
    ino_stop(
      "Subsetting can only be applied to objects of class {.cls vector} (of
      length greater than one), {.cls matrix}, or {.cls data.frame}."
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
    cluster <- tryCatch(
      stats::kmeans(argument_ign, centers = centers)$cluster,
      error = function(e) {
        ino_stop(
          "Clustering with {.fun stats::kmeans} failed:",
          e$message
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
  if (!byrow) {
    argument <- t(argument)
  }
  if (df_flag) {
    argument <- as.data.frame(argument)
  }
  return(argument)
}

#' Helper function for list flattening
#'
#' @description
#' This function flattens a nested \code{list}, see the details.
#'
#' @details
#' The input \code{list} \code{x} is transformed in the following ways:
#' - If \code{x} is a \code{list} with only one element, this element becomes
#'   the new \code{x}.
#'   This is repeated twice.
#'   E.g., \code{list(list(1))} becomes \code{1}.
#' - Else, if \code{x} is a \code{list} where each element is a \code{list}
#'   with only one element, these elements become the elements of \code{x}.
#'   This is repeated once.
#'   E.g., \code{list(list(1), list(2))} becomes \code{list(1:2)}.
#' - Else, if each element of \code{x} is a \code{list} where each element is a
#'   \code{list} with one element, the lowest-level \code{list} is replaced by
#'   the one element.
#'   E.g., \code{list(list(list(1), list(2)), list(list(3), list(4)))}
#'   becomes \code{list(list(1, 2), list(3, 4))}.
#' - Else, \code{x} remains unchanged.
#'
#' @param x
#' A \code{list}.
#'
#' @return
#' A \code{list}.
#'
#' @keywords internal

helper_flatten <- function(x) {
  stopifnot("'x' must be a list" = is.list(x))
  if (length(x) == 1) {
    names(x) <- NULL
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
    if (is.list(x) && length(x) == 1) {
      names(x) <- NULL
      x <- unlist(x, recursive = FALSE, use.names = TRUE)
      if (is.list(x) && length(x) == 1) {
        x <- unlist(x, recursive = FALSE, use.names = FALSE)
      }
    }
  } else if (all(sapply(x, function(y) is.list(y) && length(y) == 1))) {
    x <- lapply(x, function(x) {
      names(x) <- NULL
      unlist(x, recursive = FALSE, use.names = TRUE)
    })
    if (all(sapply(x, function(y) is.list(y) && length(y) == 1))) {
      x <- lapply(x, function(x) {
        names(x) <- NULL
        unlist(x, recursive = FALSE, use.names = TRUE)
      })
    }
  } else if (all(
      sapply(x, function(x) {
        all(sapply(x, function(y) is.list(y) && length(y) == 1))
      }
      ))) {
    x <- lapply(x, function(x) {
      lapply(x, function(x) {
        names(x) <- NULL
        unlist(x, recursive = FALSE, use.names = TRUE)
      })
    })
  }
  return(x)
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
  is_name_vector(arg, allow_na = FALSE)
  is_name_vector(choices, allow_na = FALSE)
  is_TRUE_FALSE(several.ok, allow_na = FALSE)
  is_TRUE_FALSE(none.ok, allow_na = FALSE)
  arg_name <- deparse(substitute(arg))
  if (!several.ok && length(arg) > 1L) {
    ino_stop(
      "{.var arg_name} must be of length 1."
    )
  }
  if (length(arg) == 0L) {
    if (none.ok) {
      return(character(0))
    } else {
      ino_stop(
        "{.var arg_name} must be of length greater or equal 1."
      )
    }
  }
  i <- pmatch(arg, choices, nomatch = 0, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    ino_stop(
      glue::glue(
        "{.var <arg_name>} must be one",
        ifelse(several.ok, " (or more) ", " "),
        "of",
        .open = "<", .close = ">"
      ),
      glue::glue(
        "{.val <choices>}",
        .open = "<", .close = ">"
      )
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
  is_index_vector(x)
  is_index(index)
  is_TRUE_FALSE(replace)
  x <- x[!(x %in% index)]
  if (replace) {
    x[x >= index] <- x[x >= index] - 1
  }
  return(x)
}




