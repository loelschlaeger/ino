#' Overview of the number of optima found.
#'
#' @description
#' This function provides an overview of the number of optima found with respect to
#' the initialization strategies.
#'
#' @param x An object of class \code{ino}.
#'
#' @return
#' A data frame.
#' @export
#'
#' @examples
#' # nr_optima()
#'
#' @importFrom ggplot2 ggplot
#'
#' @keywords
#' evaluation

nr_optima <- function(x, plot = FALSE) {

  optima_found <- unlist(sapply(x$optimizations, '[[', "res")["minimum",])
  out  <- as.data.frame(table(optima_found))
  colnames(out) <- c("optimum", "frequency")

  out_plot <- ggplot2::ggplot(out, ggplot2::aes(x = optimum, y = frequency)) + ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal()
  if(plot) print(out_plot)
  return(out)
}
