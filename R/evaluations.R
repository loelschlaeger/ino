#' Summary of an \code{ino} object
#'
#' @description
#' This function gives an overview of the optimization runs in an \code{ino}
#' object.
#'
#' @param object
#' An object of class \code{ino}.
#' @param group
#' A character vector for grouping the optimization results.
#' @param ...
#' Named functions for computing statistics.
#'
#' @export
#'
#' @importFrom dplyr group_by_at summarize n across all_of

summary.ino <- function(object, group = c(".strategy", ".optimizer"), ...) {

  ### check input
  if (!inherits(object, "ino")) {
    stop("'object' must be of class 'ino'.")
  }
  if (nrow(object$runs$table) == 0){
    stop("No optimization runs found.", call. = FALSE)
  }

  ### if group is empty, print the entire table
  if(length(group) > 0) {
    ### grouping
    opt <- dplyr::group_by_at(object$runs$table, dplyr::vars(dplyr::all_of(group)))

    ### summarizing
    opt <- dplyr::summarize(opt, "runs" = dplyr::n(), ...)

    ### un-grouping
    opt <- dplyr::ungroup(opt)
  }
  else {
    opt <- object$runs$table
  }

  ### return summary
  class(opt) <- c("summary.ino", class(opt))
  return(opt)

}

#' @noRd
#' @export

print.summary.ino <- function(x, digits = NULL, ...) {
  if(!is.null(digits)){
    x <- dplyr::mutate_if(x, is.numeric, round, digits = digits)
  }
  print(as.data.frame(x))
}

#' Plotting function.
#'
#' @description
#' This function plots one or multiple summary statistics on the optimization
#' runs in an \code{ino} object.
#'
#' @param x
#' An object of class \code{ino}.
#' @param var
#' A (numeric) summary statistic on the optimization runs.
#' @param by
#' Plots the summary statistic \code{var} for all groups listed in \code{by}.
#' @param type
#' Governs the type of plot. Either \code{"boxplot"}, \code{"histogram"}, or
#' \code{"barplot"}.
#' @param ...
#' Additional arguments to be passed to the plotting function.
#'
#' @return
#' A ggplot.
#'
#' @export
#'
#' @examples
#' # plot.ino()
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
  optimization_df$minimum <- round(optimization_df$minimum, digits = 2)

  ### check input
  if (nrow(optimization_df) == 0) {
    stop("Optimisations runs have not yet been performed.", call. = FALSE)
  }
  if(length(var) > 1) {
    stop("Only one summary statistic can be selected in 'var'.", call. = FALSE)
  }
  if(!(type %in% c("boxplot", "histogram", "barplot"))) {
    stop("'type' does allow only the following entries: 'boxplot', 'histogram', or 'barplot'",
         call. = FALSE)
  }
  if(!(var %in% colnames(optimization_df))) {
    stop(paste("Column", var, "does not exist."), call. = FALSE)
  }
  if(sum(!(by %in% colnames(optimization_df))) > 0) {
    stop(paste("Column", by[!(by %in% colnames(optimization_df))], "does not exist."),
         call. = FALSE)
  }

  ### build plot
  if(type == "boxplot"){
    out_plot <- ggplot(optimization_df, aes(y = .data[[var]])) +
      geom_boxplot() + facet_wrap(by, labeller = "label_both")
  }
  if(type == "histogram"){
    out_plot <- ggplot(optimization_df, aes(x = .data[[var]])) +
      geom_histogram(position = "dodge") + facet_wrap(by, labeller = "label_both")
  }
  if(type == "barplot"){
    out_plot <- ggplot(optimization_df, aes(x = .data[[var]])) +
      geom_bar(position = "dodge") + facet_wrap(by, labeller = "label_both")
  }

  ### return plot
  #print(out_plot)
  return(out_plot)
}


#' Number of optima found
#'
#' @description
#' This function provides an overview of the number of optima found with respect
#' to the initialization strategies.
#'
#' @param x
#' An object of class \code{ino}.
#' @param round
#' TBA
#'
#' @return
#' A ggplot.
#'
#' @export
#'
#' @examples
#' # overview_optima()
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal xlab
#'
#' @keywords
#' evaluation

overview_optima <- function(x, round = 2) {
  optima_found <- x$runs$table$minimum
  optima_found <- round(optima_found, digits = round)
  out  <- as.data.frame(table(optima_found))
  colnames(out) <- c("optimum", "frequency")
  out_plot <- ggplot2::ggplot(out, ggplot2::aes(x = .data$optimum, y = .data$frequency)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() + ggplot2::xlab("function value at the optimum")
  print(out_plot)
  print(out)
  return(out_plot)
}
