#' @description
#' Sets additional arguments for \code{f}.
#' @param ...
#' Optionally additional named arguments for \code{f}.
#' @importFrom glue glue
#' @return
#' Invisibly the \code{Nop} object.
set_argument = function(...) {
  arguments <- list(...)
  argument_names <- names(arguments)
  argument_names[which(is.null(argument_names))] <- ""
  for (i in seq_along(arguments)) {
    if (!is_name(argument_names[i], error = FALSE)) {
      ino_stop(glue::glue("Please name argument {i}."))
    }
    if (argument_names[i] %in% names(private$.arguments)) {
      ino_stop(
        glue::glue(
          "Argument {.var <argument_names[i]>} already exists, ",
          "call {.var $remove_argument({.val <argument_names[i]>})} first.",
          .open = "<", .close = ">"
        )
      )
    }
  }
  for (i in seq_along(arguments)) {
    private$.arguments[[argument_names[i]]] <- arguments[[i]]
  }
  invisible(self)
},

#' @description
#' Gets an argument value of \code{f}.
#' @param argument_name
#' A \code{character}, the argument to extract.
#' @return
#' The argument.
get_argument = function(argument_name) {
  if (missing(argument_name)) {
    ino_stop("Please specify {.var argument_name}.")
  }
  is_name(argument_name, error = TRUE)
  private$.check_additional_argument_exists(argument_name)
  private$.arguments[[argument_name]]
},

#' @description
#' Removes an additional argument for \code{f}.
#' @param argument_name
#' A \code{character}, the argument to remove.
#' @return
#' Invisibly the \code{Nop} object.
remove_argument = function(argument_name) {
  if (missing(argument_name)) {
    ino_stop("Please specify {.var argument_name}.")
  }
  is_name(argument_name, error = TRUE)
  private$.check_additional_argument_exists(argument_name)
  arg_id <- which(names(private$.arguments) == argument_name)
  private$.arguments[arg_id] <- NULL
  invisible(self)
},

#' @description
#' Resets an additional argument for \code{f} after transformation with
#' \code{$standardize()} or \code{$subset()}.
#' @param argument_name
#' A \code{character}, the name of the argument to reset.
#' @return
#' Invisibly the \code{Nop} object.
reset_argument = function(
    argument_name, verbose = getOption("verbose", default = FALSE)
) {
  if (missing(argument_name)) {
    ino_stop("Please specify {.var argument_name}.")
  }
  private$.check_additional_argument_exists(argument_name)
  if (!is.null(private$.original_arguments[[argument_name]])) {
    original_argument <- private$.original_arguments[[argument_name]]
    private$.arguments[[argument_name]] <- original_argument
    private$.original_arguments[[argument_name]] <- NULL
    ino_status(
      glue::glue("Reset `{argument_name}`."),
      verbose = verbose
    )
  } else {
    ino_warn("Nothing to reset.")
  }
  invisible(self)
},

#' @description
#' Standardizes the optimization problem.
#' @param argument_name
#' A \code{character}, the name of the argument of \code{f} to be
#' standardized. The argument must a \code{numeric} \code{vector},
#' \code{matrix}, or \code{data.frame}.
#' @param by_column
#' Only relevant if the argument \code{argument_name} is a \code{matrix} or
#' a \code{data.frame}.
#' In that case, either \code{TRUE} to standardize column-wise (default) or
#' \code{FALSE} to standardize row-wise.
#' @param center
#' Set to \code{TRUE} (default) for centering, resulting in zero mean.
#' @param scale
#' Set to \code{TRUE} (default) for scaling, resulting in unit variance.
#' @param ignore
#' A \code{integer} (vector) of column indices (or row indices if
#' \code{by_column = FALSE}) to not standardize.
#' @param jointly
#' A \code{list} of \code{integer} vectors with column indices (or row
#' indices if \code{by_column = FALSE}) to standardize jointly.
#' @return
#' Invisibly the \code{Nop} object.
#' The \code{numeric} centering and scalings used (if any) are added as
#' attributes \code{"standardized:center"} and \code{"standardized:scale"}
#' to the argument specified via \code{argument_name}.
standardize = function(
    argument_name, by_column = TRUE, center = TRUE, scale = TRUE,
    ignore = integer(), jointly = list(),
    verbose = getOption("verbose", default = FALSE)
) {
  original_argument <- self$get_argument(argument_name)
  standardized_argument <- standardize_helper(
    argument = original_argument, by_column = by_column, center = center,
    scale = scale, ignore = ignore, jointly = jointly
  )
  private$.arguments[[argument_name]] <- standardized_argument
  if (is.null(private$.original_arguments[[argument_name]])) {
    private$.original_arguments[[argument_name]] <- original_argument
  }
  ino_status(
    glue::glue("Standardized `{argument_name}`."),
    verbose = verbose
  )
  invisible(self)
},

#' @description
#' Subsets the optimization problem.
#' @param argument_name
#' A \code{character}, the name of the argument of \code{f} to be subsetted.
#' @param by_row
#' Only relevant if the argument \code{argument_name} is a \code{matrix} or
#' a \code{data.frame}.
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
#' Only relevant, if \code{how = "(dis)similar"}.
#' In that case, a \code{integer} (vector) of row indices (or column indices
#' if \code{by_row = FALSE}) to ignore for clustering.
#' @return
#' Invisibly the \code{Nop} object.
subset = function(
    argument_name, by_row = TRUE, how = "random", proportion = 0.5,
    centers = 2, ignore = integer(), seed = NULL,
    verbose = getOption("verbose", default = FALSE)
) {
  original_argument <- self$get_argument(argument_name)
  ino_seed(seed, verbose = verbose)
  subsetted_argument <- subset_helper(
    argument = original_argument, by_row = by_row, how = how,
    proportion = proportion, centers = centers, ignore = ignore
  )
  ino_status(
    glue::glue(
      "Reduced '{argument_name}' from ",
      if (is.vector(original_argument)) {
        length_old <- length(original_argument)
        length_new <- length(subsetted_argument)
        "{length_old} to {length_new} {how} element(s)."
      } else {
        if (by_row) {
          nrow_old <- nrow(original_argument)
          nrow_new <- nrow(subsetted_argument)
          glue::glue(
            "{nrow_old} to {nrow_new} {how} row(s).",
          )
        } else {
          ncol_old <- ncol(original_argument)
          ncol_new <- ncol(subsetted_argument)
          glue::glue(
            "{ncol_old} to {ncol_new} {how} column(s).",
          )
        }
      }
    ),
    verbose = verbose
  )
  private$.arguments[[argument_name]] <- subsetted_argument
  if (is.null(private$.original_arguments[[argument_name]])) {
    private$.original_arguments[[argument_name]] <- original_argument
  }
  invisible(self)
},

