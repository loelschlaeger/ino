#' Overview of optimization times.
#'
#' @description
#' This function provides an overview of the optimization times with respect to
#' the initialization strategies.
#'
#' @param x
#' An object of class \code{ino}.
#' @param plot_hist
#' Set to \code{TRUE} for plotting a histogram of the optimization times.
#' @param plot_freq
#' Set to \code{TRUE} for plotting a frequency plot of the optimization times.
#' @param round
#' Sets the number of decimal places for the optimization time.
#'
#' @return
#' A data frame.
#'
#' @export
#'
#' @examples
#' # optimization_time()
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar geom_histogram scale_fill_manual theme_minimal
#' @importFrom cowplot plot_grid
#'
#' @keywords
#' evaluation

optimization_time <- function(x, plot_hist = FALSE, plot_freq = FALSE, round = 2) {

  out <- data.frame(
    "strategy" = sapply(x$optimizations, '[[', "strategy"),
    "time" = sapply(x$optimizations, '[[', "time")
  )

  ## colour-blind-friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ## histogram
  out_hist <- ggplot2::ggplot(out, ggplot2::aes(x = .data$time, fill = .data$strategy)) +
    ggplot2::geom_histogram(alpha = 0.6) +
    ggplot2::scale_fill_manual(name = "initialization strategy", values = cbPalette) +
    ggplot2::theme_minimal()

  ## frequency table
  optim_times <- round(out$time, digits = round)
  optim_times_table <- as.data.frame(table(optim_times))
  colnames(optim_times_table) <- c("time", "frequency")

  out_freq <- ggplot2::ggplot(optim_times_table, ggplot2::aes(x = .data$time, y = .data$frequency)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal()

  if(plot_hist & plot_freq){
    print(cowplot::plot_grid(out_hist, out_freq, ncol = 2))
  }
  if(plot_hist & !plot_freq){
    print(out_hist)
  }
  if(plot_freq & !plot_hist){
    print(out_freq)
  }

  return(out)
}
