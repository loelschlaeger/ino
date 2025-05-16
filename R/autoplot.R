#' @name autoplot.Nop
#'
#' @title Nop plotting methods
#'
#' @description
#' - `autoplot.Nop()` # TODO
#' - `autoplot.Nop_optima()`
#' - `autoplot.Nop_deviation()`
#' - `autoplot.Nop_results()`
#'
#' @param object
#' Either: # TODO
#'
#' @param ...
#' Other arguments passed to specific methods.
#'
#' @return
#' A `ggplot` object.

NULL

#' @rdname autoplot.Nop
#'
#' @param xlim,xlim2 \[`numeric(2)`\]\cr
#' Ranges for the first and second parameter to plot.
#'
#' If `NULL`, they are derived from the specified initial values in `object`.
#'
#' @export

autoplot.Nop <- function(object, xlim = NULL, xlim2 = NULL, ...) {

  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(object),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_class(object, "Nop"),
    var_name = "object"
  )
  npar <- sum(object$npar)
  oeli::input_check_response(
    check = checkmate::check_count(npar, positive = TRUE),
    prefix = "Field {.var npar} of {.var object} is bad:"
  )
  oeli::input_check_response(
    check = list(
      checkmate::check_numeric(
        xlim, any.missing = FALSE, len = 2, sorted = TRUE, null.ok = FALSE,
        finite = TRUE
      ),
      if (is.null(xlim) && length(object$initial_values) > 0) {
        TRUE
      } else {
        "Specify initial values in {.var object}"
      }
    ),
    var_name = "xlim"
  )

  ### call 1D or 2D autoplot
  if (npar == 1) {
    autoplot.Nop_1d(object, xlim = xlim, ...)
  } else if (npar == 2) {
    oeli::input_check_response(
      check = list(
        checkmate::check_numeric(
          xlim2, any.missing = FALSE, len = 2, sorted = TRUE, null.ok = FALSE,
          finite = TRUE
        ),
        if (is.null(xlim2) && length(object$initial_values) > 0) {
          TRUE
        } else {
          "Specify initial values in {.var object}"
        }
      ),
      var_name = "xlim2"
    )
    autoplot.Nop_2d(object, xlim = xlim, xlim2 = xlim2, ...)
  } else {
    cli::cli_warn("Currently not available for more than 2 parameter")
    return(NULL)
  }

}

#' @keywords internal

autoplot.Nop_1d <- function(object, xlim, ...) {
  initial_values <- object$initial_values
  if (is.null(xlim)) {
    range <- range(unlist(initial_values))
    xlim <- c(floor(range[1]), ceiling(range[2]))
  }
  objective <- object$.__enclos_env__$private$.objective
  objective_name <- objective$objective_name
  target_name <- objective$target
  f <- function(x) objective$evaluate(x)
  data <- data.frame(x = unlist(initial_values), y = sapply(initial_values, f))
  if (nrow(data) == 0) data <- data.frame(x = numeric(), y = numeric())
  ggplot2::ggplot() +
    ggplot2::geom_function(fun = function(x) sapply(x, f), xlim = xlim) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::geom_point(data, mapping = ggplot2::aes(x = x, y = y), color = "red") +
    ggplot2::labs(
      x = target_name,
      y = paste0(objective_name, "(", target_name, ")")
    )
}

#' @keywords internal

autoplot.Nop_2d <- function(object, xlim, xlim2, ...) {
  initial_values <- object$initial_values
  if (is.null(xlim)) {
    range <- range(sapply(initial_values, `[`, 1))
    xlim <- c(floor(range[1]), ceiling(range[2]))
  }
  if (is.null(xlim2)) {
    range <- range(sapply(initial_values, `[`, 2))
    xlim2 <- c(floor(range[1]), ceiling(range[2]))
  }
  objective <- object$.__enclos_env__$private$.objective
  objective_name <- objective$objective_name
  target_name <- objective$target
  f <- function(x, y) objective$evaluate(c(x, y))
  grid_x <- seq(xlim[1], xlim[2], length.out = 100)
  grid_y <- seq(xlim2[1], xlim2[2], length.out = 100)
  grid <- expand.grid(x = grid_x, y = grid_y)
  grid$z <- apply(grid, 1, function(row) f(row["x"], row["y"]))
  data <- data.frame(x = sapply(initial_values, `[`, 1), y = sapply(initial_values, `[`, 2))
  if (nrow(data) == 0) data <- data.frame(x = numeric(), y = numeric())
  ggplot2::ggplot() +
    ggplot2::geom_contour_filled(data = grid, mapping = ggplot2::aes(x = x, y = y, z = z)) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_y_continuous(limits = xlim2) +
    ggplot2::geom_point(data = data, mapping = ggplot2::aes(x = x, y = y), color = "red") +
    ggplot2::labs(
      x = paste0(target_name, "[1]"),
      y = paste0(target_name, "[2]"),
      fill = paste0(objective_name, "(", target_name, ")")
    )
}

#' @rdname autoplot.Nop
#' @export

autoplot.Nop_optima <- function(object, ...) {

  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(object),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_class(object, "Nop_optima"),
    var_name = "object"
  )

  ### produce bar chart
  object |>
    ggplot2::ggplot(ggplot2::aes(x = factor(value), y = n)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Value", y = "Frequency")
}

#' @rdname autoplot.Nop
#' @export

autoplot.Nop_deviation <- function(object, ...) {
  # TODO
}

#' @rdname autoplot.Nop
#' @export

autoplot.Nop_results <- function(object, ...) {
  # TODO
}
