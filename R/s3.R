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
  if (!is.null(arguments)) {
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
  if (length(x$optimizer) == 0) {
    cat(cli::style_italic("No optimizer specified yet.\n"))
  } else {
    for (i in seq_along(x$optimizer)) {
      if (check_optimizer_active(optimizer, i)) {
        cat(glue::glue("- {i}: {names(x$optimizer)[i]}"), "\n")
      } else {
        cat(
          glue::glue("- {i}: {cli::style_italic(x$optimizer[[i]])}"), "\n"
        )
      }
    }
  }

  ### results
  cat(crayon::underline("Optimization results:\n"))
  results <- suppressWarnings(x$results())
  if (length(results) == 0) {
    cat(cli::style_italic("No optimization results saved yet.\n"))
  } else {
    cat(glue::glue(
      "- Optimization runs: {length(results)}",
      "- Best parameter: {paste(round(x$best_parameter(), digits = digits), collapse = ' ')}",
      "- Best value: {round(x$best_value(), digits = digits)}",
      .sep = "\n"
    ), "\n")
  }

  ### return self invisibly
  invisible(x)
}

#' @noRd
#' @importFrom dplyr bind_rows
#' @exportS3Method

summary.Nop <- function(
    object, columns = c("value", "parameter", "seconds", "optimizer", "label"),
    which_runs = "all", which_optimizer = "all",
    digits = getOption("ino_digits", default = 2),
    only_comparable = FALSE, ...
  ) {

  private <- object$.__enclos_env__$private # REMOVE

  ### input checks
  if (!is.character(columns)) {
    ino_stop(
      "Argument {.var columns} must be a {.cls character} (vector)."
    )
  }
  if (!(isTRUE(only_comparable) || isFALSE(only_comparable))) {
    ino_stop(
      "Argument {.var only_comparable} must be {.val TRUE} or {.val FALSE}."
    )
  }
  run_ids <- private$.get_run_ids(which_runs)
  optimizer_ids <- private$.get_optimizer_ids(which_optimizer)
  records <- private$.records[run_ids]

  ### combine records in data.frame
  out <- data.frame()
  for (run in run_ids) {
    for (opt in optimizer_ids) {
      out_tmp <- as.data.frame(t(cbind(private$.records[[run]][[opt]])))
      out <- dplyr::bind_rows(out, out_tmp)
    }
  }

  ### add elements
  add_vars <- list(...)
  for (i in seq_along(add_vars)) {
    out[[names(add_vars)[i]]] <- sapply(
      unlist(object$records, recursive = FALSE),
      function(r) {
        env <- new.env()
        env$true_value <- object$true_value
        env$true_parameter <- object$true_parameter
        list2env(r, env)
        eval(parse(text = add_vars[[i]]), env)
      }
    )
  }
  columns <- c(columns, names(add_vars))

  ### unlist single-valued records
  for (i in 1:ncol(out)) {
    if (all(sapply(out[,i], length) == 1 &
            sapply(out[,i], class) %in% c("character", "numeric", "logical"))) {
      out[,i] <- unlist(out[,i])
    }
  }

  ### filter for comparable results
  if (only_comparable) {
    out <- out[which(out[, "comparable"]), ]
  }

  ### filter columns
  if (!identical(columns, "all")) {
    out <- dplyr::select(out, dplyr::any_of(columns))
  }

  ### round numeric records
  for (i in 1:ncol(out)) {
    if (is.vector(out[,i]) && is.numeric(out[,i])) {
      out[,i] <- round(out[,i], digits = digits)
    }
    if (is.list(out[,i]) && all(sapply(out[,i], is.numeric))) {
      out[[i]] <- lapply(out[,i], round, digits = digits)
    }
  }

  ### return data.frame
  return(out)
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
      data[[by]], data[["seconds"]], FUN = median, decreasing = TRUE, na.rm = TRUE
    )
    data <- dplyr::group_by(data, .data[[by]])
  }
  if (relative) {
    med <- dplyr::summarize(
      data, "median" = median(.data$seconds, na.rm = TRUE), .groups = "drop"
    ) %>%
      dplyr::select(median) %>% min()
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
