

Nop_old <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Prints details of the numerical optimization problem.
    #' @param ...
    #' Currently not used.
    #' @return
    #' Invisibly the \code{Nop} object.

    print = function(...) {

      ### info on additional arguments
      argument_names <- private$.objective$fixed_arguments
      if (length(argument_names) > 0) {
        modified <- argument_names %in% names(private$.original_arguments)
        if (any(modified)) {
          names(argument_names) <- ifelse(modified, "!", " ")
        }
        cli::cli_h2("Fixed arguments")
        cli::cli_bullets(argument_names)
        if (any(modified)) {
          cat(cli::style_italic("\nSome arguments are currently modified.\n"))
        }
      }

    },

    #' @description
    #' Manages fixed arguments for \code{objective}.
    #' @param action
    #' One of:
    #' - \code{"set"} to set an argument,
    #' - \code{"get"} to extract an argument value,
    #' - \code{"remove"} to remove an argument,
    #' - \code{"reset"} to reset an argument to the original value (if any),
    #' - \code{"modify"} to modify an argument to a new value, but the original
    #'   value is saved and can be recovered via \code{"reset"}.
    #' @param ...
    #' Additional parameters depending on \code{action}:
    #' - if \code{action = "set"} or \code{"modify"}, one or more named
    #'   arguments,
    #' - if \code{action = "get"}, \code{"remove"}, or \code{"reset"},
    #'   the \code{argument_name}, a \code{character} that selects an argument
    #' @return
    #' The argument value if \code{action = "get"} and invisibly the \code{Nop}
    #' object, else.

    fixed_argument = function(action, ...) {
      if (missing(action)) {
        cli::cli_abort(
          "Please specify the argument {.var action}.",
          call = NULL
        )
      }
      checkmate::assert_string(action)
      action <- oeli::match_arg(
        action, c("set", "get", "remove", "reset", "modify")
      )
      args <- list(...)
      arg_names <- names(args)
      if (action == "set") {
        self$set_argument(...)
      }
      if (action == "get") {
        if (!"argument_name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var argument_name}.",
            call = NULL
          )
        }
        return(self$get_argument(argument_name = args[["argument_name"]]))
      }
      if (action == "remove") {
        if (!"argument_name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var argument_name}.",
            call = NULL
          )
        }
        argument_name <- args[["argument_name"]]
        private$.objective$remove_argument(
          argument_name, verbose = self$verbose
        )
        if (argument_name %in% names(private$.original_arguments)) {
          arg_id <- which(names(private$.original_arguments) == argument_name)
          private$.original_arguments[arg_id] <- NULL
        }
        if (self$verbose) {
          cli::cli_alert_info("Removed argument {.var {argument_name}}.")
        }
      }
      if (action == "reset") {
        if (!"argument_name" %in% arg_names) {
          cli::cli_abort(
            "Please specify {.var argument_name}.",
            call = NULL
          )
        }
        argument_name <- args[["argument_name"]]
        checkmate::assert_string(argument_name)
        if (!is.null(private$.original_arguments[[argument_name]])) {
          original_argument <- private$.original_arguments[[argument_name]]
          arg <- list("set", original_argument)
          names(arg) <- c("action", argument_name)
          do.call(self$fixed_argument, arg)
          private$.original_arguments[[argument_name]] <- NULL
          if (self$verbose) {
            cli::cli_alert_info("Reset argument {.var {argument_name}}.")
          }
        } else {
          cli::cli_warn("Nothing to reset.")
        }
      }
      if (action == "modify") {
        if (length(args) == 0) {
          cli::cli_warn("No argument to modify.")
        } else if (length(args) > 1) {
          for (i in 1:length(args)) {
            arg <- list("modify", args[[i]])
            names(arg) <- c("action", arg_names[i])
            do.call(self$fixed_argument, arg)
          }
        } else {
          argument_name <- names(args)[1]
          if (is.null(private$.original_arguments[[argument_name]])) {
            original_argument <- private$.objective$get_argument(
              argument_name, verbose = FALSE
            )
            private$.original_arguments[[argument_name]] <- original_argument
          }
          do.call(
            private$.objective$set_argument,
            c(args, list("overwrite" = TRUE, "verbose" = FALSE))
          )
          if (self$verbose) {
            cli::cli_alert_info("Modified argument {.var {argument_name}}.")
          }
        }
      }
      invisible(self)
    },

    #' @description
    #' Set a fixed argument for \code{objective}.
    #' @param ...
    #' One or more named arguments.
    #' @return
    #' Invisibly the \code{Nop} object.

    set_argument = function(...) {
      args <- list(...)
      if (length(args) == 0) {
        cli::cli_warn("No argument to set.")
      } else if (length(args) > 1) {
        for (i in 1:length(args)) {
          do.call(self$set_argument, args[i])
        }
      } else {
        do.call(
          private$.objective$set_argument,
          c(args, list("overwrite" = TRUE, "verbose" = self$verbose))
        )
      }
    },

    #' @description
    #' Get a fixed argument for \code{objective}.
    #' @param argument_name
    #' A \code{character} that selects an argument.
    #' @return
    #' Invisibly the \code{Nop} object.

    get_argument = function(argument_name) {
      private$.objective$get_argument(argument_name, verbose = self$verbose)
    },

    #' @description
    #' Standardizes the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the fixed argument of \code{object} to be
    #' standardized. The argument must a \code{numeric} \code{vector},
    #' \code{matrix}, or \code{data.frame}.
    #' @param byrow
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to standardize row-wise or
    #' \code{FALSE} (default) to standardize row-wise.
    #' @param center
    #' Either \code{TRUE} (default) for centering or \code{FALSE}, else.
    #' @param scale
    #' Either \code{TRUE} (default) for scaling or \code{FALSE}, else.
    #' @param ignore
    #' A \code{integer} (vector) of column indices (or row indices if
    #' \code{byrow = TRUE}) to not standardize.
    #' @return
    #' Invisibly the \code{Nop} object.

    standardize = function(
      argument_name, byrow = FALSE, center = TRUE, scale = TRUE,
      ignore = integer()
    ) {
      original_argument <- self$get_argument(argument_name)
      standardized_argument <- normalize::normalize(
        x = original_argument, byrow = byrow,
        center = center, scale = scale, ignore = ignore
      )
      if (self$verbose) {
        cli::cli_alert_info(
          "Standardized argument '{argument_name}'."
        )
      }
      arg <- list("modify", standardized_argument)
      names(arg) <- c("action", argument_name)
      suppressMessages(do.call(self$fixed_argument, arg))
    },

    #' @description
    #' Reduces the optimization problem.
    #' @param argument_name
    #' A \code{character}, the name of the argument of \code{f} to be reduced.
    #' @param byrow
    #' Only relevant if the argument \code{argument_name} is a \code{matrix} or
    #' a \code{data.frame}.
    #' In that case, either \code{TRUE} to reduce row-wise (default) or
    #' \code{FALSE} to reduce column-wise.
    #' @param how
    #' A \code{character}, specifying how to reduce. Can be one of:
    #' - \code{"random"} (default), reduce at random
    #' - \code{"first"}, reduce to the first elements
    #' - \code{"last"}, reduce to the last elements
    #' - \code{"similar"}, reduce to similar elements
    #' - \code{"dissimilar"}, reduce to dissimilar elements
    #' Note that \code{"similar"} and \code{"dissimilar"} are based on k-means
    #' clustering via \code{\link[stats]{kmeans}}. To apply these options,
    #' the argument \code{argument_name} must be \code{numeric}.
    #' @param proportion
    #' A \code{numeric} between \code{0} and \code{1}, specifying the
    #' reduction proportion.
    #' By default, \code{proportion = 0.5}.
    #' @param centers
    #' Only relevant, if \code{how = "(dis)similar"}.
    #' In that case, passed to \code{\link[stats]{kmeans}}.
    #' By default, \code{centers = 2}.
    #' @param ignore
    #' Only relevant, if \code{how = "(dis)similar"}.
    #' In that case a \code{integer} (vector) of row indices (or column indices
    #' if \code{byrow = FALSE}) to ignore for clustering.
    #' @return
    #' Invisibly the \code{Nop} object.

    reduce = function(
      argument_name, byrow = TRUE, how = "random", proportion = 0.5,
      centers = 2, ignore = integer()
    ) {
      original_argument <- self$get_argument(argument_name)
      reduced_argument <- portion::portion(
        x = original_argument, byrow = byrow, how = how,
        proportion = proportion, centers = centers, ignore = ignore
      )
      if (self$verbose) {
        cli::cli_alert_info(
          glue::glue(
            "Reduced argument '{argument_name}' from ",
            if (checkmate::test_atomic_vector(original_argument)) {
              length_old <- length(original_argument)
              length_new <- length(reduced_argument)
              "{length_old} to {length_new} {how} element(s)."
            } else {
              if (byrow) {
                nrow_old <- nrow(original_argument)
                nrow_new <- nrow(reduced_argument)
                glue::glue(
                  "{nrow_old} to {nrow_new} {how} row(s).",
                )
              } else {
                ncol_old <- ncol(original_argument)
                ncol_new <- ncol(reduced_argument)
                glue::glue(
                  "{ncol_old} to {ncol_new} {how} column(s).",
                )
              }
            }
          )
        )
      }
      arg <- list("modify", reduced_argument)
      names(arg) <- c("action", argument_name)
      suppressMessages(do.call(self$fixed_argument, arg))
    },

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
    #' Defines a subset of promising initial values for the optimization.
    #' @param proportion
    #' A \code{numeric} between 0 and 1, the subset proportion.
    #' @param condition
    #' Defines the condition on which the initial values are selected, either
    #' - \code{"gradient_steep"} for the points where the numerical gradient is steepest,
    #' - \code{"value_low"} for the points where the function value is lowest,
    #' - \code{"value_high"} for the points where the function value is highest.
    #' @return
    #' Invisibly the \code{Nop} object.

    initialize_promising = function(proportion, condition = "gradient_steep") {
      checkmate::assert_number(proportion, lower = 0, upper = 1)
      oeli::match_arg(condition, c("gradient_steep", "value_low", "value_high"))
      runs <- length(private$.initial_values)
      if (runs == 0) {
        if (self$verbose) {
          cli::cli_alert_info("No initial values defined yet.")
        }
      } else {
        runs <- ceiling(runs * proportion)
        if (condition == "gradient_steep") {
          gradient_norms <- sapply(
            private$.initial_values,
            function(x) {
              grad <- numDeriv::grad(
                func = private$.objective$evaluate,
                x = x
              )
              sqrt(sum(grad^2))
            }
          )
          indices <- order(gradient_norms, decreasing = TRUE)[seq_len(runs)]
        } else {
          values <- sapply(private$.initial_values, self$evaluate)
          indices <- order(values, decreasing = (condition == "value_high"))[seq_len(runs)]
        }
        private$.initial_values <- private$.initial_values[indices]
        private$.initial_seconds <- private$.initial_seconds[indices]
        private$.initial_type <- private$.initial_type[indices]
        if (self$verbose) {
          cli::cli_alert_info(
            "Reduced to a subset of {runs} initial parameter value{?s} based
            on the condition {.val {condition}}."
          )
        }
      }
      invisible(self)
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

  ),

  private = list(

    .original_arguments = list()




  )
)

