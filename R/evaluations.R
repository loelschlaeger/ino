#' Summary of initialization
#'
#' @description
#' This function gives an overview of the initialization runs in an \code{ino}
#' object.
#'
#' @details
#' The following values are available for each \code{ino} object:
#' \itemize{
#'   \item \code{.strategy}, the name of the initialization strategy,
#'   \item \code{.optimizer}, the name of the optimizer (if more than one),
#'   \item \code{.time}, the optimization time,
#'   \item \code{.optimum}, the function value at the optimum.
#' }
#'
#' @param object
#' An object of class \code{ino}.
#' @param group
#' A character vector for grouping the optimization results, or \code{NULL}
#' (default) for no grouping.
#' @param ...
#' Named functions for computing statistics.
#'
#' @return
#' A \code{data.frame}.
#'
#' @keywords
#' evaluation
#'
#' @export
#'
#' @importFrom dplyr group_by_at summarize n across all_of

summary.ino <- function(object, group = NULL, ...) {

  ### check input
  if (!inherits(object, "ino")) {
    stop("'object' must be of class 'ino'.")
  }
  if (nrow(object$runs$table) == 0) {
    warning("No optimization runs found.")
    return(data.frame())
  }

  ### if group is empty, print the entire table
  if (length(group) > 0) {
    opt <- dplyr::group_by_at(
      object$runs$table,
      dplyr::vars(dplyr::all_of(group))
    )
    opt <- dplyr::summarize(opt, "runs" = dplyr::n(), ...)
    opt <- dplyr::ungroup(opt)
  } else {
    opt <- object$runs$table
  }

  ### return summary
  class(opt) <- c("summary.ino", class(opt))
  return(opt)
}

#' @noRd
#' @export

print.summary.ino <- function(x, digits = NULL, ...) {
  if (!is.null(digits)) {
    x <- dplyr::mutate_if(x, is.numeric, digits, digits = digits)
  }
  print(as.data.frame(x))
}

#' Visualization of initialization
#'
#' @description
#' This function plots one or multiple summary statistics on the initialization
#' runs in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param var
#' The name of the statistic to be plotted.
#' @param by
#' Plots the summary statistic \code{var} for all groups listed in \code{by}.
#' @param type
#' Governs the type of plot. Either \code{"boxplot"}, \code{"histogram"}, or
#' \code{"barplot"}.
#' @param ...
#' Additional arguments to be passed to the plotting function.
#'
#' @return
#' An \code{ggplot} object.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar geom_histogram geom_boxplot facet_wrap
#'
#' @keywords
#' evaluation

plot.ino <- function(x, var = ".time", by = ".strategy", type = "boxplot", ...) {

  ### extract optimization
  optimization_df <- x$runs$table
  optimization_df$.time <- as.numeric(optimization_df$.time)
  optimization_df$minimum <- round(optimization_df$.optimum, digits = 2)

  ### check input
  if (nrow(optimization_df) == 0) {
    stop("Optimization runs have not yet been performed.",
         call. = FALSE)
  }
  if (length(var) > 1) {
    stop("Only one summary statistic can be selected in 'var'.",
         call. = FALSE)
  }
  if (!(type %in% c("boxplot", "histogram", "barplot"))) {
    stop("'type' only allows 'boxplot', 'histogram', or 'barplot'.",
      call. = FALSE
    )
  }
  if (!(var %in% colnames(optimization_df))) {
    stop(paste0("Column '", var, "' does not exist."),
         call. = FALSE)
  }
  if (sum(!(by %in% colnames(optimization_df))) > 0) {
    stop(paste0(
      "Column '", by[!(by %in% colnames(optimization_df))],
      "' does not exist."
    ),
    call. = FALSE
    )
  }

  ### build plot
  if (type == "boxplot") {
    out_plot <- ggplot(optimization_df, aes(y = .data[[var]])) +
      geom_boxplot() +
      facet_wrap(by, labeller = "label_both")
  }
  if (type == "histogram") {
    out_plot <- ggplot(optimization_df, aes(x = .data[[var]])) +
      geom_histogram(position = "dodge") +
      facet_wrap(by, labeller = "label_both")
  }
  if (type == "barplot") {
    out_plot <- ggplot(optimization_df, aes(x = .data[[var]])) +
      geom_bar(position = "dodge") +
      facet_wrap(by, labeller = "label_both")
  }

  ### return plot
  return(out_plot)
}

#' Overview of optima
#'
#' @description
#' This function provides an overview of the identified optima.
#'
#' @param x
#' An object of class \code{ino}.
#' @param digits
#' The number of digits of the optima values.
#' @param plot
#' Set to \code{TRUE} for a visualization.
#'
#' @return
#' Either a \code{data.frame} (if \code{plot = FALSE}), or otherwise a
#' \code{ggplot} object.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal xlab
#'
#' @keywords
#' evaluation

overview_optima <- function(x, digits = 2, plot = FALSE) {
  optima_found <- x$runs$table$.optimum
  optima_found <- round(optima_found, digits = digits)
  out <- as.data.frame(table(optima_found))
  colnames(out) <- c("optimum", "frequency")
  if (!plot) {
    return(out)
  } else {
    out_plot <- ggplot2::ggplot(out, ggplot2::aes(
      x = .data$optimum,
      y = .data$frequency
    )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_minimal() +
      ggplot2::xlab("optimum")
    return(out_plot)
  }
}
