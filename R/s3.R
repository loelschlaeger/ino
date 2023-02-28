#' @noRd
#' @importFrom crayon underline
#' @importFrom glue glue
#' @importFrom cli style_italic
#' @exportS3Method

print.Nop <- function(x, digits = getOption("ino_digits", default = 2), ...) {

  ### optimization problem
  cat(glue::glue(
    crayon::underline("Optimization problem:"),
    "- Function: {x$f_name}",
    "- Optimize over: {x$f_target} (length {x$npar})",
    .sep = "\n"
  ), "\n")
  arguments <- suppressWarnings(x$arguments)
  if (length(arguments) > 0) {
    cat(
      glue::glue(
        "- Additional arguments: {paste(names(arguments), collapse = ', ')}",
      ),
      "\n"
    )
  }
  true_parameter <- suppressWarnings(x$true_parameter)
  if (!is.null(true_parameter)) {
    cat(
      glue::glue(
        "- True optimum at: ",
        "{paste(round(true_parameter, digits = digits), collapse = ' ')}"
      ),
      "\n"
    )
  }
  true_value <- suppressWarnings(x$true_value)
  if (!is.null(true_value)) {
    cat(glue::glue(
      "- True optimum value: ",
      "{round(true_value, digits = digits)}"
    ), "\n")
  }

  ### optimizers
  optimizer <- suppressWarnings(x$optimizer)
  cat(crayon::underline("Numerical optimizer:\n"))
  if (length(optimizer) == 0) {
    cat(cli::style_italic("No optimizer specified yet.\n"))
  } else {
    for (id in seq_along(optimizer)) {
      optimizer_label <- names(optimizer)[id]
      if (attr(optimizer[[id]], "active")) {
        cat(glue::glue("- {id}: {optimizer_label}"), "\n")
      } else {
        cat(
          glue::glue("- {id}:"),
          cli::style_italic(glue::glue("{optimizer_label} has been removed.")),
          "\n"
        )
      }
    }
  }

  ### results
  cat(crayon::underline("Optimization results:\n"))
  results <- suppressWarnings(x$results())
  if (length(results) == 0) {
    cat(cli::style_italic("No results saved yet.\n"))
  } else {
    best_parameter <- round(x$best_parameter(), digits = digits)
    best_value <- round(x$best_value(), digits = digits)
    cat(glue::glue(
      "- Optimization runs: {length(results)}",
      "- Best parameter: {paste(best_parameter, collapse = ' ')}",
      "- Best value: {best_value}",
      .sep = "\n"
    ), "\n")
  }

  ### return self invisibly
  invisible(x)
}

#' @noRd
#' @exportS3Method

summary.Nop <- function(object, ...) {
  object$summary(...)
}

#' @noRd
#' @importFrom ggplot2 ggplot aes scale_y_continuous geom_boxplot facet_wrap
#' theme element_blank ylab
#' @importFrom scales percent
#' @importFrom dplyr group_by %>% mutate
#' @importFrom rlang .data
#' @exportS3Method

plot.Nop <- function(x, by = NULL, relative = TRUE, log = FALSE, ...) {
  ### input checks
  if (is.null(by)) {
    relative <- FALSE
  } else {
    if (!(identical(by, "label") || identical(by, "optimizer"))) {
      ino_stop(
        "Argument {.var by} must be {.val NULL}, {.val label}, or {.val optimizer}."
      )
    }
    if (!isTRUE(relative) && !isFALSE(relative)) {
      ino_stop("Argument {.var relative} must be {.val TRUE} or {.val FALSE}.")
    }
  }
  if (!isTRUE(log) && !isFALSE(log)) {
    ino_stop("Argument {.var log} must be {.val TRUE} or {.val FALSE}.")
  }

  ### prepare optimization times
  data <- x$summary(columns = c("seconds", by), digits = Inf)
  if (!is.null(by)) {
    data[[by]] <- reorder(
      data[[by]], data[["seconds"]],
      FUN = median, decreasing = TRUE, na.rm = TRUE
    )
    data <- dplyr::group_by(data, .data[[by]])
  }
  if (relative) {
    med <- dplyr::summarize(
      data,
      "median" = median(.data$seconds, na.rm = TRUE), .groups = "drop"
    ) %>%
      dplyr::select(median) %>%
      min()
    data <- data %>% dplyr::mutate("seconds" = (seconds - med) / med + 1)
  }

  ### build plot
  if (is.null(by)) {
    base_plot <- ggplot2::ggplot(data, aes(y = "")) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  } else {
    base_plot <- ggplot2::ggplot(data, aes(y = .data[[by]]))
  }
  base_plot <- base_plot +
    ggplot2::geom_boxplot(aes(x = .data$seconds), na.rm = TRUE)
  if (relative) {
    base_plot <- base_plot +
      ggplot2::xlab("relative optimization time")
    if (log) {
      base_plot <- base_plot +
        ggplot2::scale_x_log10(labels = scales::percent)
    } else {
      base_plot <- base_plot +
        ggplot2::scale_x_continuous(labels = scales::percent)
    }
  } else {
    base_plot <- base_plot +
      ggplot2::xlab("optimization time in seconds")
    if (log) {
      base_plot <- base_plot +
        ggplot2::scale_x_log10()
    } else {
      base_plot <- base_plot +
        ggplot2::scale_x_continuous()
    }
  }
  plot <- base_plot +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
  return(plot)
}
