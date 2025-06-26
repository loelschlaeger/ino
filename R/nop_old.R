

Nop_old <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Defines initial values based on results from previous optimizations.
    #' @return
    #' Invisibly the \code{Nop} object.

    initialize_continue = function(
      which_run, which_optimizer = "all", which_direction = c("min", "max")
    ) {
      out <- self$results(
        which_run = which_run, which_optimizer = which_optimizer,
        which_direction = which_direction,
        which_element = c("parameter", "seconds")
      )
      at <- lapply(out, `[[`, "parameter")
      seconds <- sapply(out, `[[`, "seconds")
      runs <- length(at)
      drop <- integer()
      for (i in seq_len(runs)) {
        check <- try(
          private$.check_target(at[[i]], verbose = FALSE),
          silent = TRUE
        )
        if (inherits(check, "try-error")) {
          drop <- c(drop, i)
        }
      }
      if (length(drop) > 0) {
        at <- at[-drop]
        seconds <- seconds[-drop]
        if (self$verbose) {
          cli::cli_alert_info(
            "{length(drop)} set{?s} of results cannot be continued."
          )
        }
      }
      self$initialize_custom(
        at = at, seconds = seconds, type = "continued"
      )
    },

    #' @description
    #' Visualizes the optimization time or value.
    #' @param which_element
    #' Either:
    #' - \code{"seconds"} to plot the optimization times (default)
    #' - \code{"value"} to plot the optimization values
    #' @param relative
    #' Only if \code{which_element = "seconds"}.
    #' In this case, set to \code{TRUE} to plot relative time differences with
    #' respect to the overall median.
    #' @param ...
    #' Currently not used.
    #' @return
    #' A \code{\link[ggplot2]{ggplot}} object.

    plot = function(
      which_element = "seconds", group_by = NULL, relative = FALSE,
      which_run = "comparable", which_direction = c("min", "max"),
      which_optimizer = "all", ...
    ) {

      ### input checks
      group_by <- private$.check_group_by(group_by = group_by)
      checkmate::assert_flag(relative)
      if (identical(which_element, "value") && relative) {
        cli::cli_warn(
          "Set {.var relative} to {.val FALSE}.",
          "Cannot be {.val TRUE} if {.var which_element} is {.val value}."
        )
        relative <- FALSE
      }

      ### prepare plot
      data <- self$summary(
        which_element = which_element, which_run = which_run,
        which_optimizer = which_optimizer, which_direction = which_direction,
        add_identifier = c(".optimization_label", ".optimizer_label")
      )
      incomplete_cases <- which(!stats::complete.cases(data))
      if (length(incomplete_cases) > 0) {
        cli::cli_alert_info(
          "Dropped {length(incomplete_cases)} row{?s} with missing data."
        )
        data <- data[-incomplete_cases, , drop = FALSE]
      }
      if (nrow(data) == 0) {
        cli::cli_alert_info("No data to plot.")
        return(invisible(NULL))
      }
      if (identical(which_element, "seconds") && relative) {
        med <- dplyr::summarize(
          data,
          "median" = stats::median(.data[["seconds"]], na.rm = TRUE)
        ) |> as.numeric()
        data <- data |>
          dplyr::mutate("seconds" = (.data[["seconds"]] - med) / med)
      }
      if (identical(which_element, "seconds") && !is.null(group_by)) {
        data <- data |>
          dplyr::mutate(
            label = forcats::fct_reorder(
              .f = .data[[group_by]], .x = .data[["seconds"]],
              .fun = stats::median, .desc = TRUE
            )
          )
      }
      if (is.null(group_by)) {
        base_plot <- ggplot2::ggplot(
          data, ggplot2::aes(x = .data[[which_element]], y = "")
        )
      } else {
        base_plot <- ggplot2::ggplot(
          data, ggplot2::aes(x = .data[[which_element]], y = .data[[group_by]])
        )
      }
      base_plot <- base_plot +
        ggplot2::theme_minimal()
      if (identical(which_element, "value")) {
        base_plot <- base_plot +
          ggplot2::geom_jitter(
            alpha = 0.5, width = 0
          ) +
          ggplot2::scale_x_continuous(
            name = "Function value at optimum"
          )
      }
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
        if (!is.null(group_by)) {
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
      if (is.null(group_by)) {
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

      ### add plot title
      base_plot + ggplot2::ggtitle(
        label = paste0("Optimization of ", self$f_name)
      )

      ### return plot
      return(base_plot)
    }

  )
)

