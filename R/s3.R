#' @noRd
#' @importFrom crayon underline
#' @importFrom glue glue
#' @importFrom cli style_italic
#' @exportS3Method

print.Nop <- function(x, digits = getOption("ino_digits", default = 2), ...) {
  print_optimization_problem(x = x, digits = digits)
  print_optimizers(x = x)
  print_optimization_results(x = x, digits = digits)
  invisible(x)
}

#' @noRd

print_optimization_problem <- function(x, digits) {
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
}

#' @noRd

print_optimizers <- function(x) {
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
}

#' @noRd

print_optimization_results <- function(x, digits) {
  cat(crayon::underline("Optimization results:\n"))
  suppressWarnings({
    results <- x$results()
    nruns <- x$number_runs()
    nruns_comparable <- x$number_runs(only_comparable = TRUE)
  })
  if (nruns == 0) {
    cat(cli::style_italic("No results saved yet.\n"))
  } else if (nruns_comparable == 0) {
    cat(glue::glue(
      "- Total runs (comparable): {nruns} (0)"
    ))
  } else {
    best_parameter <- round(x$best_parameter(), digits = digits)
    best_value <- round(x$best_value(), digits = digits)
    cat(glue::glue(
      "- Total runs (comparable): {nruns} ({nruns_comparable})",
      "- Best parameter: {paste(best_parameter, collapse = ' ')}",
      "- Best value: {best_value}",
      .sep = "\n"
    ), "\n")
  }
}

#' @noRd
#' @exportS3Method

summary.Nop <- function(
    object, which_element = "basic", which_run = "all", which_optimizer = "all",
    digits = getOption("ino_digits", default = 2), only_comparable = FALSE, ...) {
  ### extract results and combine in data.frame
  out <- data.frame()
  results <- object$results(
    which_run = which_run, which_optimizer = which_optimizer,
    which_element = which_element, only_comparable = only_comparable,
    simplify = FALSE
  )
  if (length(results) == 0) {
    return(invisible(out))
  }
  for (run_id in seq_along(results)) {
    for (optimizer_id in seq_along(results[[run_id]])) {
      append <- results[[run_id]][[optimizer_id]]
      if (length(append) > 0) {
        out <- dplyr::bind_rows(out, as.data.frame(t(cbind(append))))
      }
    }
  }
  rownames(out) <- NULL

  ### add elements
  add_vars <- list(...)
  if (length(add_vars) > 0) {
    results_all <- object$results(
      which_run = which_run, which_optimizer = which_optimizer,
      which_element = "all", only_comparable = only_comparable
    )
    true_value <- suppressWarnings(object$true_value)
    true_parameter <- suppressWarnings(object$true_parameter)
    for (i in seq_along(add_vars)) {
      out[[names(add_vars)[i]]] <- sapply(
        unlist(results_all, recursive = FALSE),
        function(r) {
          env <- new.env()
          list2env(r, env)
          tryCatch(
            eval(parse(text = add_vars[[i]]), env),
            error = function(e) NA
          )
        }
      )
    }
  }

  ### unlist single-valued records
  for (i in seq_len(ncol(out))) {
    unlist_try <- unlist(out[, i], recursive = FALSE)
    if (length(unlist_try) == nrow(out)) {
      out[, i] <- unlist(out[, i], recursive = FALSE)
    }
  }

  ### round numeric records
  for (i in seq_len(ncol(out))) {
    if (is.vector(out[, i]) && is.numeric(out[, i])) {
      out[, i] <- round(out[, i], digits = digits)
    }
    if (is.list(out[, i]) && all(sapply(out[, i], is.numeric))) {
      out[[i]] <- lapply(out[, i], round, digits = digits)
    }
  }

  ### return data.frame
  return(out)
}

#' @noRd
#' @importFrom stats reorder median
#' @importFrom ggplot2 ggplot aes scale_y_continuous geom_boxplot facet_wrap
#' theme element_blank ylab
#' @importFrom scales percent
#' @importFrom dplyr group_by mutate
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
    is_TRUE_FALSE(relative)
  }
  is_TRUE_FALSE(log)

  ### prepare optimization times
  data <- x$summary(which_element = c("seconds", by), digits = Inf)
  if (!is.null(by)) {
    data[[by]] <- stats::reorder(
      data[[by]], data[["seconds"]],
      FUN = stats::median, decreasing = TRUE, na.rm = TRUE
    )
    data <- dplyr::group_by(data, .data[[by]])
  }
  if (relative) {
    med <- dplyr::summarize(
      data,
      "median" = stats::median(.data$seconds, na.rm = TRUE), .groups = "drop"
    ) |>
      dplyr::select(.data[["median"]]) |>
      min()
    data <- data |>
      dplyr::mutate("seconds" = (.data[["seconds"]] - med) / med + 1)
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

  ### return plot
  return(plot)
}
