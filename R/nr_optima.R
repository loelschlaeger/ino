#' Overview of the number of optima found.
#'
#' @description
#' This function provides an overview of the number of optima found with respect to
#' the initialization strategies.
#'
#' @param x
#' An object of class \code{ino}.
#' @param plot
#' Set to \code{TRUE} for plotting a frequency plot of found optima.
#' @param round
#' Sets the number of decimal places for the location of the optima.
#'
#' @return
#' A data frame.
#'
#' @export
#'
#' @examples
#' # nr_optima()
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal
#'
#' @keywords
#' evaluation

nr_optima <- function(x, plot = FALSE, round = 2) {
  optima_found <- unlist(sapply(x$optimizations, '[[', "res")["minimum", ])
  optima_found <- round(optima_found, digits = round)
  out  <- as.data.frame(table(optima_found))
  colnames(out) <- c("optimum", "frequency")

  if(plot){
    out_plot <- ggplot2::ggplot(out, ggplot2::aes(x = .data$optimum, y = .data$frequency)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_minimal()
    print(out_plot)
  }
  return(out)
}
