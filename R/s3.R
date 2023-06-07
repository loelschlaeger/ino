#' @noRd
#' @importFrom crayon underline
#' @importFrom glue glue
#' @importFrom forcats fct_reorder
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
  result_names <- unique(names(
    unlist(unlist(results, recursive = FALSE), recursive = FALSE)
  ))
  for (run_id in seq_along(results)) {
    for (optimizer_id in seq_along(results[[run_id]])) {
      append <- results[[run_id]][[optimizer_id]]
      missing_results <- setdiff(result_names, names(append))
      if (length(missing_results) > 0) {
        append[missing_results] <- NA
      }
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
      which_element = "all", only_comparable = only_comparable,
      simplify = FALSE
    )
    true_value <- object$true_value
    true_parameter <- object$true_parameter
    best_value <- object$best_value()
    best_parameter <- object$best_parameter()
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
#' @importFrom stats complete.cases median
#' @importFrom dplyr summarize mutate
#' @importFrom ggplot2 ggplot aes scale_x_continuous theme_minimal theme
#' @importFrom ggplot2 geom_boxplot geom_vline annotate element_blank ggtitle
#' @importFrom ggplot2 coord_cartesian
#' @importFrom scales label_percent
#' @importFrom forcats fct_reorder
#' @importFrom rlang .data
#' @importFrom scales percent
#' @exportS3Method

plot.Nop <- function(
    x, which_element = "seconds", by = NULL, relative = FALSE,
    which_run = "all", which_optimizer = "all", only_comparable = FALSE,
    title = paste("Optimization of", x$f_name), xlim = c(NA, NA), ...) {
  ### input checks
  if (!which_element %in% c("seconds", "value")) {
    ino_stop(
      "Argument {.var which_element} must be {.val seconds} or {.val value}."
    )
  }
  if (!is.null(by)) {
    if (!by %in% c("label", "optimizer")) {
      ino_stop(
        "Argument {.var by} must be {.val label} or {.val optimizer} or {.val NULL}."
      )
    }
  }
  is_TRUE_FALSE(relative)
  if (identical(which_element, "value") && relative) {
    ino_status(
      "Argument {.var relative} cannot be {.val TRUE} if {.var which_element} is {.val value}."
    )
    relative <- FALSE
  }
  is_name(title)
  if (!(is.vector(xlim) && length(xlim) == 2)) {
    ino_stop(
      "Argument {.var xlim} must be {.cls numeric} vector of length 2.",
      "Entries can also be {.val NA}."
    )
  }

  ### get data
  data <- x$summary(
    which_element = c(which_element, by), which_run = which_run,
    which_optimizer = which_optimizer, only_comparable = only_comparable,
    digits = Inf
  )

  ### drop incomplete cases
  incomplete_cases <- which(!stats::complete.cases(data))
  if (length(incomplete_cases) > 0) {
    ino_status(
      paste("Dropped", length(incomplete_cases), "results with missing data.")
    )
    data <- data[-incomplete_cases, , drop = FALSE]
  }
  if (nrow(data) == 0) {
    ino_status(
      "No data to plot."
    )
    return(invisible(NULL))
  }

  ### compute relative 'seconds' wrt to median seconds
  if (identical(which_element, "seconds") && relative) {
    med <- dplyr::summarize(
      data,
      "median" = stats::median(.data[["seconds"]], na.rm = TRUE)
    ) |> as.numeric()
    data <- data |>
      dplyr::mutate("seconds" = (.data[["seconds"]] - med) / med)
  }

  ### sort by median 'seconds'
  if (identical(which_element, "seconds") && !is.null(by)) {
    data <- data |>
      dplyr::mutate(
        label = forcats::fct_reorder(
          .f = .data[[by]], .x = .data[["seconds"]],
          .fun = stats::median, .desc = TRUE
        )
      )
  }

  ### build base plot
  if (is.null(by)) {
    base_plot <- ggplot2::ggplot(
      data, ggplot2::aes(x = .data[[which_element]], y = "")
    )
  } else {
    base_plot <- ggplot2::ggplot(
      data, ggplot2::aes(x = .data[[which_element]], y = .data[[by]])
    )
  }
  base_plot <- base_plot +
    ggplot2::theme_minimal()

  ### add 'values'
  if (identical(which_element, "value")) {
    base_plot <- base_plot +
      ggplot2::geom_point(
        position = "jitter", alpha = 0.5
      ) +
      ggplot2::scale_x_continuous(
        name = "Function value at optimum"
      )
  }

  ### add 'seconds'
  if (identical(which_element, "seconds")) {
    base_plot <- base_plot +
      ggplot2::geom_boxplot()
    if (relative) {
      base_plot <- base_plot +
        ggplot2::scale_x_continuous(
          labels = scales::label_percent(style_positive = c("plus")),
          name = "Relative deviation in optimization time"
        )
    } else {
      base_plot <- base_plot +
        ggplot2::scale_x_continuous(
          name = "Optimization time in seconds",
          limits = c(0, NA)
        )
    }
    if (!is.null(by)) {
      med <- dplyr::summarize(
        data,
        "median" = stats::median(.data[["seconds"]], na.rm = TRUE)
      ) |> as.numeric()
      base_plot <- base_plot + ggplot2::geom_vline(
        xintercept = med
      ) +
        ggplot2::annotate(
          x = med, y = Inf, label = "Overall median",
          geom = "label", vjust = 1
        )
    }
  }

  ### modify labels and axes
  if (is.null(by)) {
    base_plot <- base_plot +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  } else {
    base_plot <- base_plot +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank()
      )
  }
  if (!is.null(title)) {
    base_plot <- base_plot + ggplot2::ggtitle(label = title)
  }
  base_plot <- base_plot + ggplot2::coord_cartesian(xlim = xlim)

  ### return plot
  return(base_plot)
}
