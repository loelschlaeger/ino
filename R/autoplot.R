#' @name autoplot.Nop
#'
#' @title Plotting methods
#'
#' @description
#' - `autoplot.Nop()` plots the objective function
#' - `autoplot.Nop_results()` plots boxplots of optimization results
#' - `autoplot.Nop_optima()` plots a bar chart of the found optima
#' - `autoplot.Nop_deviation()` plots deviations per dimension from a reference
#'
#' @param object
#' Depends on the method:
#' - for `autoplot.Nop()`, a `Nop` object
#' - for `autoplot.Nop_results()`, the value `Nop$results`
#' - for `autoplot.Nop_optima()`, the value `Nop$optima`
#' - for `autoplot.Nop_deviation()`, the value `Nop$deviation`
#'
#' @param ...
#' Other arguments passed to specific methods.
#'
#' @return
#' A `ggplot` object.

NULL

#' @rdname autoplot.Nop
#'
#' @method autoplot Nop
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
        xlim,
        any.missing = FALSE, len = 2, sorted = TRUE, null.ok = FALSE,
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
          xlim2,
          any.missing = FALSE, len = 2, sorted = TRUE, null.ok = FALSE,
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
  if (no_initials <- nrow(data) == 0) {
    data <- data.frame(x = numeric(), y = numeric())
  }
  plot <- ggplot2::ggplot() +
    ggplot2::geom_function(
      aes(color = "Objective function"),
      fun = function(x) sapply(x, f),
      xlim = xlim
    ) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_color_manual(
      values = c("Objective function" = "black", "Initial values" = "red"),
      name = NULL
    ) +
    ggplot2::labs(
      x = target_name,
      y = paste0(objective_name, "(", target_name, ")")
    ) +
    ggplot2::theme(
      legend.position = if (no_initials) "none" else "top"
    )
  if (!no_initials) {
    plot <- plot + ggplot2::geom_point(
      data,
      mapping = ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]], color = "Initial values"
      )
    )
  }
  return(plot)
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
  if (no_initials <- nrow(data) == 0) {
    data <- data.frame(x = numeric(), y = numeric())
  }
  plot <- ggplot2::ggplot() +
    ggplot2::geom_contour_filled(
      data = grid,
      mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]], z = .data[["z"]])
    ) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_y_continuous(limits = xlim2) +
    ggplot2::labs(
      x = paste0(target_name, "[1]"),
      y = paste0(target_name, "[2]"),
      fill = paste0(objective_name, "(", target_name, ")")
    )
  if (!no_initials) {
    plot <- plot + ggplot2::geom_point(
      data = data,
      mapping = ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]], color = "Initial values"
      )
    ) +
      ggplot2::scale_color_manual(
        values = c("Initial values" = "red"),
        name = NULL
      )
  }
  return(plot)
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

  ### grouped?
  if (inherits(object, "group_by")) {
    return(
      lapply(object, function(x) {
        class(x) <- c("Nop_optima", class(x))
        autoplot(x, ...)
      })
    )
  }

  ### produce bar chart
  object |>
    ggplot2::ggplot(ggplot2::aes(x = factor(.data[["value"]]), y = .data[["n"]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Value", y = "Frequency")
}

#' @rdname autoplot.Nop
#'
#' @param jitter \[`logical(1)`\]\cr
#' Apply jitter to the points?
#'
#' @method autoplot Nop_deviation
#' @export

autoplot.Nop_deviation <- function(object, jitter = TRUE, ...) {
  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(object),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_class(object, "Nop_deviation"),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(jitter),
    var_name = "jitter"
  )

  ### produce figure
  object |>
    tidyr::pivot_longer(cols = -dplyr::all_of(".optimization_label")) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["name"]], y = .data[["value"]])) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[".optimization_label"]]),
      position = ifelse(jitter, "jitter", "identity")
    ) +
    ggplot2::labs(x = "parameter", y = "deviation") +
    ggplot2::geom_hline(yintercept = 0)
}

#' @rdname autoplot.Nop
#' @method autoplot Nop_results
#'
#' @param which_element \[`character(1)\]\cr
#' A column name of `object` to plot.
#'
#' @param group_by \[`character(1)\]\cr
#' Selects how the plot is grouped. Either:
#' - `NULL` to not group,
#' - `"optimization"` to group by optimization label,
#' - `"optimizer"`` to group by optimizer label.
#'
#' @param relative \[`logical(1)\]\cr
#' Plot values relative to the overall median?
#'
#' @export

autoplot.Nop_results <- function(
    object,
    which_element = "seconds",
    group_by = NULL,
    relative = FALSE,
    ...) {
  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(object),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_data_frame(object),
    var_name = "object"
  )
  if (nrow(object) == 0) {
    return(ggplot2::ggplot())
  }
  suitable_columns <- !vapply(object, is.list, logical(1)) &
    vapply(object, is.numeric, logical(1))
  which_element_selection <- object[suitable_columns] |> colnames()
  which_element <- oeli::match_arg(
    which_element,
    choices = which_element_selection
  )
  oeli::input_check_response(
    check = checkmate::check_choice(
      group_by,
      choices = c("optimization", "optimizer"),
      null.ok = TRUE
    ),
    var_name = "group_by"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(relative),
    var_name = "relative"
  )

  ### transform data to relative values
  if (relative) {
    med <- object[[which_element]] |> stats::median(na.rm = TRUE)
    object[[which_element]] <- (object[[which_element]] - med) / med
  }

  ### build graphic
  mapping <- if (identical(group_by, "optimization")) {
    ggplot2::aes(
      x = .data[[which_element]],
      y = .data[[".optimization_label"]]
    )
  } else if (identical(group_by, "optimizer")) {
    ggplot2::aes(
      x = .data[[which_element]],
      y = .data[[".optimizer_label"]]
    )
  } else {
    ggplot2::aes(
      x = .data[[which_element]],
      y = NULL
    )
  }
  object |> ggplot2::ggplot() +
    ggplot2::geom_boxplot(mapping = mapping)
}
