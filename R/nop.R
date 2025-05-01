#' Nop Object
#'
#' @description
#' A `Nop` object defines a numerical optimization problem.
#'
#' @param f \[`function`\]\cr
#' A \code{function} to be optimized (the so-called objective function).
#'
#' It is expected that \code{f} has at least one \code{numeric} argument.
#'
#' Further, it is expected that the return value of \code{f} is of the
#' structure \code{numeric(1)}, i.e. a single \code{numeric} value.
#'
#' @param target \[`character()`\]\cr
#' The argument name(s) that get optimized (the so-called target arguments).
#'
#' All target arguments must be \code{numeric}.
#'
#' Can be \code{NULL} (default), then the first function argument is selected.
#'
#' @param npar \[`integer()`\]\cr
#' The length of each target argument, i.e., the length(s) of the
#' \code{numeric} \code{vector} argument(s) specified by \code{target}.
#'
#' @param optimizer \[`Optimizer`\]\cr
#' An \code{Optimizer} object, which can be created via
#' \code{\link[optimizeR]{Optimizer}}.
#'
#' @param optimizer_label \[`character(1)`\]\cr
#' A (unique) label for the optimizer.
#'
#' @param sampler \[`function`\]\cr
#' A `function` without any arguments that returns a \code{numeric}
#' vector of length \code{sum(self$npar)}.
#'
#' @param runs \[`integer(1)`\]\cr
#' The number of optimization runs.

Nop <- R6::R6Class(
  classname = "Nop",
  public = list(

    #' @description
    #' Creates a new `Nop` object.
    #'
    #' @param ...
    #' Optionally additional function arguments that are fixed during the
    #' optimization.

    initialize = function(f, target = NULL, npar, ...) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(f),
        var_name = "f"
      )
      oeli::input_check_response(
        check = oeli::check_missing(npar),
        var_name = "npar"
      )

      ### build Objective Object
      private$.objective <- optimizeR::Objective$new(
        f = f, target = target, npar = npar, ...
      )

      ### build result index
      private$.results <- oeli::Storage$new()

    },

    #' @description
    #' Prints details of the `Nop` object.
    #'
    #' @param ...
    #' Currently not used.

    print = function(...) {

      ### info on optimization problem
      cli::cli_h2("Optimization problem")
      target <- private$.objective$.__enclos_env__$private$.target
      cli::cli_bullets(c(
        "*" = "Objective: {private$.objective$objective_name}",
        "*" = "Target: {target} (length {self$npar})"
      ))

    },

    #' @description
    #' Evaluates the objective function.
    #'
    #' @param at \[`numeric()`\]\cr
    #' The values for the target argument(s), written in a single vector.
    #'
    #' Must be of length `sum(self$npar)`.

    evaluate = function(at = stats::rnorm(sum(self$npar))) {
      private$.objective$evaluate(.at = at)
    },

    #' @description
    #' Specifies a numerical optimizer.

    set_optimizer = function(optimizer, optimizer_label = optimizer$label) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(optimizer),
        var_name = "optimizer"
      )
      oeli::input_check_response(
        check = checkmate::check_class(optimizer, "Optimizer"),
        var_name = "optimizer"
      )
      oeli::input_check_response(
        check = checkmate::check_string(optimizer_label),
        var_name = "optimizer_label"
      )
      oeli::input_check_response(
        check = checkmate::check_names(
          optimizer_label, disjunct.from = names(private$.optimizer)
        ),
        var_name = "optimizer_label"
      )

      ### set optimizer
      private$.optimizer[[optimizer_label]] <- optimizer
      if (self$verbose) {
        cli::cli_alert_info("Set optimizer {.val {optimizer_label}}.")
      }
      invisible(self)
    },

    #' @description
    #' Defines fixed initial values for the optimization.
    #'
    #' @param at \[`integer(self$sum(npar))` | `list()`\]\cr
    #' The fixed initial parameter vector.
    #'
    #' It can also be a `list` of such vectors.

    initialize_fixed = function(at) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(at),
        var_name = "at"
      )

      ### set initial values
      if (!checkmate::test_list(at)) at <- list(at)
      self$initialize_custom(at, type = "fixed")

    },

    #' @description
    #' Defines random initial values for the optimization.

    initialize_random = function(
      sampler = function() stats::rnorm(sum(self$npar)), runs = 1L
    ) {

      ### input checks
      oeli::input_check_response(
        check = checkmate::check_function(sampler, nargs = 0),
        var_name = "sampler"
      )
      oeli::input_check_response(
        check = checkmate::check_count(runs, positive = TRUE),
        var_name = "runs"
      )

      ### set initial values
      at <- list()
      seconds <- numeric(runs)
      for (run in seq_len(runs)) {
        t_start <- Sys.time()
        value <- try(sampler(), silent = TRUE)
        t_end <- Sys.time()
        seconds[run] <- as.numeric(difftime(t_end, t_start, units = "secs"))
        at[[run]] <- value
      }
      self$initialize_custom(at, seconds = seconds, type = "random")

    },

    #' @description
    #' Defines custom initial values for the optimization.
    #'
    #' @param at \[`list()`\]\cr
    #' A `list` of initial parameter vectors.
    #'
    #' @param seconds \[`numeric(length(at))`\]\cr
    #' The number of seconds it took to obtain each initial value in `at`,
    #' which is added to the overall optimization time.
    #'
    #' @param type \[`character(1)`\]\cr
    #' The type of the initial values.

    initialize_custom = function(
      at, seconds = rep(0, length(at)), type = "custom"
    ) {

      ### input checks
      oeli::input_check_response(
        check = oeli::check_missing(at),
        var_name = "at"
      )
      runs <- length(at)
      oeli::input_check_response(
        check = checkmate::check_numeric(seconds, lower = 0, len = runs),
        var_name = "seconds"
      )
      oeli::input_check_response(
        check = checkmate::check_string(type),
        var_name = "type"
      )
      lapply(at, private$.check_target)

      ### set initial values
      private$.initial_values <- c(private$.initial_values, at)
      private$.initial_type <- c(private$.initial_type, rep(type, runs))
      private$.initial_seconds <- c(private$.initial_seconds, seconds)
      if (self$verbose) {
        cli::cli_alert_info("Added {runs} {type} initial parameter value{?s}.")
      }
      invisible(self)
    },

    #' @description
    #' Optimizes the target function.
    #'
    #' @details
    #' Supports:
    #'
    #' - Parallel computation of multiple optimization runs via \code{{future}}
    #' - Progress messages via \code{{progressr}}
    #'
    #' @param optimization_label \[`character(1)`\]\cr
    #' A label for the optimization to distinguish optimization runs.
    #'
    #' @param reset_initial_afterwards \[`logical(1)`\]\cr
    #' Reset the initial values after the optimization?

    optimize = function(
      optimization_label = self$fresh_label,
      which_optimizer = "all", which_direction = "min",
      seconds = Inf, hide_warnings = TRUE,
      reset_initial_afterwards = TRUE
    ) {

      ### input checks
      private$.check_arguments_complete(verbose = self$verbose)
      oeli::input_check_response(
        check = checkmate::check_string(optimization_label),
        var_name = "optimization_label"
      )
      optimizer_ids <- private$.check_which_optimizer(
        which_optimizer = which_optimizer, to_id = TRUE, verbose = self$verbose
      )
      if (length(optimizer_ids) == 0) {
        cli::cli_warn("No optimizer selected.")
        return(invisible(self))
      }
      which_direction <- private$.check_which_direction(
        which_direction = which_direction, both_allowed = TRUE,
        verbose = self$verbose
      )
      oeli::input_check_response(
        check = checkmate::check_number(seconds, lower = 0),
        var_name = "seconds"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(hide_warnings),
        var_name = "hide_warnings"
      )
      oeli::input_check_response(
        check = checkmate::check_flag(reset_initial_afterwards),
        var_name = "reset_initial_afterwards"
      )

      ### initial values available?
      if (length(private$.initial_values) == 0) {
        self$initialize_random(runs = 1)
        cli::cli_warn(
          "No initial values defined by user, random initial values are used.",
          "Call {.fun $initialize_*} first to customize the initialization."
        )
      }

      ### build grid of optimization combinations
      combinations <- expand.grid(
        initial = private$.initial_values,
        optimizer_id = optimizer_ids,
        which_direction = which_direction
      )

      ### optimize
      if (self$verbose) {
        cli::cli_alert_info("Start optimization {.val {optimization_label}}.")
      }
      progress_step <- progressr::progressor(steps = nrow(combinations))
      results <- future.apply::future_apply(
        combinations, MARGIN = 1, function(x) {
          out <- private$.optimize(
            initial = x$initial,
            optimizer_id = x$optimizer_id,
            direction = x$which_direction,
            seconds = seconds,
            hide_warnings = hide_warnings
          )
          progress_step()
          return(out)
        }, future.seed = TRUE
      )

      ### save results
      private$.save_results(
        results = results, optimization_label = optimization_label
      )
      if (reset_initial_afterwards) self$initialize_reset()
      invisible(self)
    }

  ),
  active = list(

    #' @field npar \[`integer()`, read-only\]\cr
    #' The length of each target argument.

    npar = function(value) {
      if (missing(value)) {
        private$.objective$npar
      } else {
        cli::cli_abort(
          "Field {.var $npar} is read only.",
          call = NULL
        )
      }
    },

    #' @field verbose \[`logical(1)`\]\cr
    #' Print progress and details?

    verbose = function(value) {
      if (missing(value)) {
        private$.verbose
      } else {
        oeli::input_check_response(
          check = checkmate::check_flag(verbose),
          var_name = "verbose"
        )
        private$.verbose <- value
      }
    },

    #' @field digits verbose \[`integer(1)`\]\cr
    #' The number of decimal places of interest.

    digits = function(value) {
      if (missing(value)) {
        private$.digits
      } else {
        oeli::input_check_response(
          check = checkmate::check_int(verbose, lower = 0),
          var_name = "verbose"
        )
        private$.digits <- value
      }
    },

    #' @field fresh_label \[`character(1)`, read-only\]\cr
    #' An optimization label that has not been used yet.

    fresh_label = function(value) {
      if (missing(value)) {
        default_label <- "unlabeled"
        n <- 1
        while (TRUE) {
          label <- glue::glue("{default_label}_{n}")
          if (!label %in% private$.optimization_labels) {
            return(as.character(label))
          } else {
            n <- n + 1
          }
        }
      } else {
        cli::cli_abort(
          "Field {.var $fresh_label} is read-only.",
          call = NULL
        )
      }
    }

  ),
  private = list(

    ### global options
    .verbose = getOption("verbose", default = FALSE),
    .digits = getOption("digits", default = 7),

    ### optimization problem
    .objective = NULL,
    .results = NULL,
    .optimizer = list(),

    ### initial values
    .initial_values = list(),
    .initial_type = character(),
    .initial_seconds = numeric(),

    ### optimization results
    .optimization_labels = character(),

    ### checks
    .check_target = function(at, verbose = self$verbose) {
      private$.objective$.__enclos_env__$private$.check_target(
        .at = at, .verbose = verbose
      )
    },
    .check_arguments_complete = function(verbose = self$verbose) {
      private$.objective$.__enclos_env__$private$.check_arguments_complete(
        .verbose = verbose
      )
    }

  )
)
