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
#' @importFrom stats complete.cases reorder median
#' @importFrom ggplot2 ggplot aes scale_y_continuous theme_minimal
#' @importFrom ggridges stat_density_ridges
#' @importFrom dplyr group_by summarize select mutate
#' @importFrom rlang .data
#' @exportS3Method

plot.Nop <- function(
    x, which_element = "seconds", by = NULL, relative = FALSE,
    which_run = "all", which_optimizer = "all", only_comparable = FALSE, ...
  ) {

  ### input checks
  if (!which_element %in% c("seconds", "value")) {
    ino_stop(
      "Argument {.var which_element} must be {.val seconds} or {.val value}."
    )
  }
  if (identical(which_element, "value")) {
    relative <- FALSE
  }
  if (is.null(by)) {
    relative <- FALSE
  } else {
    if (!(identical(by, "label") || identical(by, "optimizer"))) {
      ino_stop(
        "Argument {.var by} must be {.val label} or {.val optimizer}."
      )
    }
    is_TRUE_FALSE(relative)
  }

  ### get data
  data <- x$summary(
    which_element = c(which_element, by), which_run = which_run,
    which_optimizer = which_optimizer, only_comparable = only_comparable,
    digits = Inf
  )
  data <- data[stats::complete.cases(data), , drop = FALSE]

  ### compute relative times
  if (identical(which_element, "seconds") && relative) {
    med <- data |> dplyr::group_by(.data[[by]]) |> dplyr::summarize(
      "median" = stats::median(.data$seconds), .groups = "drop"
    ) |> dplyr::select("median") |> min()
    data <- data |>
      dplyr::mutate("seconds" = (.data[["seconds"]] - med) / med)
  }

  ### add times
  if (identical(which_element, "seconds")) {

    ### build base plot
    if (is.null(by)) {
      base_plot <- ggplot2::ggplot(data, ggplot2::aes(y = ""))
    } else {
      base_plot <- ggplot2::ggplot(data, ggplot2::aes(y = .data[[by]]))
    }
    base_plot <- base_plot +
      ggplot2::theme_minimal()

    ### build time visualization
    base_plot <- base_plot +
      ggridges::stat_density_ridges(
        aes(x = .data[["seconds"]]),
        quantile_lines = TRUE, quantiles = 0.5,
        calc_ecdf = TRUE, jittered_points = TRUE,
        point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7
      )
    if (relative) {
      breaks_high <- ceiling(max(data$seconds, na.rm = TRUE))
      breaks <- unique(c(-1, 0, 1:breaks_high))
      labels <- paste0(breaks * 100, "%")
      labels[which(breaks == 0)] <- "reference"
      base_plot <- base_plot +
        ggplot2::scale_x_continuous(
          name = "relative optimization time",
          breaks = breaks,
          labels = labels
        )
    } else {
      base_plot <- base_plot +
        ggplot2::scale_x_continuous(
          name = "optimization time in seconds"
        )
    }
  }

  ### add values
  if (identical(which_element, "value")) {
    base_plot <- base_plot +
      geom_point(
        aes(x = .data[["value"]]), position = "jitter"
      )
  }

  ### modify y-axis
  if (is.null(by)) {
    base_plot <- base_plot +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      )
  } else {
    base_plot <- base_plot +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank()
      )
  }

  ### return plot
  return(base_plot)
}
